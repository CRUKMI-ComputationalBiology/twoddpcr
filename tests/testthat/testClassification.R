context("Getting and setting classification")

testDir <- system.file("testdata", package = "twoddpcr")
testWell <- ddpcrWell(file.path(testDir, "sample_B03_Amplitude.csv"))
testPlate <- ddpcrPlate(testDir)

test_that("Existing classification can be retrieved", {
  expect_equal(wellClassification(testWell, cMethod="Sample"),
               factor(c("NN", "NN", "NP", "PN", "PP", "NN"),
                      levels=ddpcr$classesRain))
})

test_that("Classification can be set and retrieved (ddpcrWell)", {
    wellClassification(testWell, cMethod="manual") <-
      factor(c("NN", "NN", "NP", "PN", "PP", "Rain"), levels=ddpcr$classesRain)
  expect_true("manual" %in% wellClassificationMethod(testWell))
  expect_equal(
    wellClassification(testWell, cMethod="manual"),
    factor(c("NN", "NN", "NP", "PN", "PP", "Rain"), levels=ddpcr$classesRain)
  )
})

test_that("Classification can be set and retrieved (ddpcrPlate)", {
  # ddpcrPlate by list of factors
  plateClassification(testPlate, cMethod="manual") <-
    list(
      "B03"=factor(c("NN", "NN", "NP", "PN", "PP", "Rain"),
                   levels=ddpcr$classesRain),
      "A04"=factor(c("NN", "NP", "PN", "Rain"),
                   levels=ddpcr$classesRain),
      "A05"=factor(c("NN", "NP", "PN", "Rain"),
                   levels=ddpcr$classesRain),
      "B06"=factor(c("NN", "NN", "NN", "NN", "NN",
                     "NP", "NP", "NP", "NP", "NP"),
                   levels=ddpcr$classesRain)
    )
  expect_true("manual" %in% commonClassificationMethod(testPlate))
  expect_equal(
    plateClassification(testPlate, cMethod="manual"),
    list(
      "B03"=factor(c("NN", "NN", "NP", "PN", "PP", "Rain"),
                   levels=ddpcr$classesRain),
      "A04"=factor(c("NN", "NP", "PN", "Rain"),
                   levels=ddpcr$classesRain),
      "A05"=factor(c("NN", "NP", "PN", "Rain"),
                   levels=ddpcr$classesRain),
      "B06"=factor(c("NN", "NN", "NN", "NN", "NN",
                     "NP", "NP", "NP", "NP", "NP"),
                   levels=ddpcr$classesRain)
    )
  )

  # ddpcrPlate by a long factor
  plateClassification(testPlate, cMethod="manual") <-
    factor(c("NN", "NN", "NP", "PN", "PP", "Rain",
             "NN", "NP", "PN", "Rain",
             "NN", "NP", "PN", "Rain",
             "NN", "NN", "NN", "NN", "NN",
             "NP", "NP", "NP", "NP", "NP"),
           levels=ddpcr$classesRain)
  expect_true("manual" %in% commonClassificationMethod(testPlate))
  expect_equal(
    plateClassification(testPlate, cMethod="manual"),
    list(
      "B03"=factor(c("NN", "NN", "NP", "PN", "PP", "Rain"),
                   levels=ddpcr$classesRain),
      "A04"=factor(c("NN", "NP", "PN", "Rain"),
                   levels=ddpcr$classesRain),
      "A05"=factor(c("NN", "NP", "PN", "Rain"),
                   levels=ddpcr$classesRain),
      "B06"=factor(c("NN", "NN", "NN", "NN", "NN",
                     "NP", "NP", "NP", "NP", "NP"),
                   levels=ddpcr$classesRain)
    )
  )

 # binding together
  expect_equivalent(
    unlist(plateClassification(testPlate, cMethod="manual")),
    factor(c("NN", "NN", "NP", "PN", "PP", "Rain",
             "NN", "NP", "PN", "Rain",
             "NN", "NP", "PN", "Rain",
             "NN", "NN", "NN", "NN", "NN",
             "NP", "NP", "NP", "NP", "NP"),
           levels=ddpcr$classesRain)
  )

  # withAmplitudes should give a list of data frames
  cl <- plateClassification(testPlate, cMethod="manual", withAmplitudes=TRUE)
  expect_named(cl, c("B03", "A04", "A05", "B06"))
  expect_s3_class(cl[["B03"]], "data.frame")
  expect_s3_class(cl[["A04"]], "data.frame")
  expect_s3_class(cl[["A05"]], "data.frame")
  expect_s3_class(cl[["B06"]], "data.frame")
  expect_named(cl[["B03"]], c("Ch1.Amplitude", "Ch2.Amplitude", "manual"))
  expect_named(cl[["A04"]], c("Ch1.Amplitude", "Ch2.Amplitude", "manual"))
  expect_named(cl[["A05"]], c("Ch1.Amplitude", "Ch2.Amplitude", "manual"))
  expect_named(cl[["B06"]], c("Ch1.Amplitude", "Ch2.Amplitude", "manual"))

  # withAmplitudes and binding the results
  cl <- plateClassification(testPlate, cMethod="manual",
                            withAmplitudes=TRUE)
  cl <- do.call(rbind, cl)
  expect_s3_class(cl, "data.frame")
  expect_named(cl, c("Ch1.Amplitude", "Ch2.Amplitude", "manual"))


  # wellCol
  cl <- plateClassification(testPlate, cMethod="manual",
                            withAmplitudes=TRUE, wellCol=TRUE)
  expect_named(cl[["B03"]],
               c("Ch1.Amplitude", "Ch2.Amplitude", "manual", "Well"))

  # wellCol and binding
  cl <- plateClassification(testPlate, cMethod="manual", withAmplitudes=TRUE,
                            wellCol=TRUE)
  cl <- do.call(rbind, cl)
  expect_named(cl,
               c("Ch1.Amplitude", "Ch2.Amplitude", "manual", "Well"))
  expect_equal(levels(cl$Well), c("B03", "A04", "A05", "B06"))
})

test_that("Trying to set classification of the wrong number of droplets", {
  expect_error(
    wellClassification(testWell, cMethod="manual") <-
      factor(c("NN", "NN", "NP", "PN", "PP"), levels=ddpcr$classesRain),
  )
  expect_error(
    wellClassification(testWell, cMethod="manual") <-
      factor(c("NN", "NN", "NP", "PN", "PP", "Rain", "NN"),
             levels=ddpcr$classesRain),
  )

  # ddpcrPlate by list of factors
  expect_error(
    plateClassification(testPlate, cMethod="manual") <-
    list(
      "B03"=factor(c("NN", "NN", "NP", "PN", "PP", "Rain"),
                   levels=ddpcr$classesRain),
      "A04"=factor(c("NN", "NP", "PN", "Rain"),
                   levels=ddpcr$classesRain),
      "A05"=factor(c("NN", "NP", "PN", "Rain"),
                   levels=ddpcr$classesRain),
      "B06"=factor(c("NN", "NN", "NN", "NN", "NN",
                     "PP", "PP", "PP", "PP", "PP", "NN"),
                   levels=ddpcr$classesRain)
    )
  )
  expect_error(
    plateClassification(testPlate, cMethod="manual") <-
    factor(c("NN", "NN", "NP", "PN", "PP", "Rain",
             "NN", "NP", "PN", "Rain",
             "NN", "NP", "PN", "Rain",
             "NN", "NN", "NN", "NN", "NN",
             "PP", "PP", "PP", "PP", "PP", "NN"),
           levels=ddpcr$classesRain),
  )
  expect_error(
    plateClassification(testPlate, cMethod="manual") <-
    factor(c("NN", "NN", "NP", "PN", "PP", "Rain",
             "NN", "NP", "PN", "Rain",
             "NN", "NP", "PN", "Rain",
             "NN", "NN", "NN", "NN", "NN",
             "PP", "PP", "PP", "PP"),
           levels=ddpcr$classesRain),
  )
})
