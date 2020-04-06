context("Classification means, covariances")

testDir <- system.file("testdata", package = "twoddpcr")

test_that("centres for three clusters are found", {
  testWell <- ddpcrWell(file.path(testDir, "sample_A04_Amplitude.csv"))
  cl <- wellClassification(testWell, cMethod="Sample", withAmplitudes=TRUE)

  centres <- data.frame(c("NN", "NP", "PN"),
                        "Ch1.Amplitude"=c(5015, 5430, 10100),
                        "Ch2.Amplitude"=c(2510, 7060, 2100),
                        row.names=1)
  expect_identical(classMeans(cl, classCol="Sample"), centres)
})

test_that("centres for four clusters are found", {
  testWell <- ddpcrWell(file.path(testDir, "sample_A05_Amplitude.csv"))
  cl <- wellClassification(testWell, cMethod="Sample", withAmplitudes=TRUE)

  centres <- data.frame(c("NN", "NP", "PN", "PP"),
                        "Ch1.Amplitude"=c(5020, 5450, 10050, 9050),
                        "Ch2.Amplitude"=c(1520, 7050, 2050, 6050),
                        row.names=1)
  expect_identical(classMeans(cl, classCol="Sample"), centres)
})

test_that("centres for four clusters are found (more droplets)", {
  testWell <- ddpcrWell(file.path(testDir, "sample_B03_Amplitude.csv"))
  cl <- wellClassification(testWell, cMethod="Sample", withAmplitudes=TRUE)

  centres <- data.frame(c("NN", "NP", "PN", "PP"),
                        "Ch1.Amplitude"=c((5000 + 5000 + 6800) / 3,
                                          5500, 10000, 9000),
                        "Ch2.Amplitude"=c((1500 + 1500 + 1600) / 3,
                                          7000, 2000, 6000),
                        row.names=1)
  expect_identical(classMeans(cl, classCol="Sample"), centres)
})

test_that("centres for two clusters are found (more droplets)", {
  testWell <- ddpcrWell(file.path(testDir, "sample_B06_Amplitude.csv"))
  cl <- wellClassification(testWell, cMethod="Sample", withAmplitudes=TRUE)
  centres <-
    data.frame(c("NN", "NP"),
               "Ch1.Amplitude"=c((5000 + 5016 + 5011 + 5005 + 4995) / 5,
                                 (5506 + 5494 + 5503 + 5494 + 5502) / 5),
               "Ch2.Amplitude"=c((1500 + 1497 + 1504 + 1497 + 1498) / 5,
                                 (6996 + 7005 + 6998 + 6996 + 7006) / 5),
               row.names=1)
  expect_identical(classMeans(cl, classCol="Sample"), centres)
})



getDfDiff <- function(x, cl, knownCovs)
{
  compCov <- classCov(cl, classCol="Sample")[[x]]
  ourCov <- knownCovs[[x]]
  if(is.null(compCov) && is.null(ourCov))
    return(0)
  else
    return(max(abs(compCov - ourCov)))
}

test_that("covariances for two clusters are found (more droplets)", {
  testWell <- ddpcrWell(file.path(testDir, "sample_B06_Amplitude.csv"))
  cl <- wellClassification(testWell, cMethod="Sample", withAmplitudes=TRUE)
  knownCovs <-
    list("NN"=data.frame(c("Ch1.Amplitude", "Ch2.Amplitude"),
                         "Ch1.Amplitude"=c(70.30, 3.15),
                         "Ch2.Amplitude"=c(3.15, 8.70),
                         row.names=1),
         "NP"=data.frame(c("Ch1.Amplitude", "Ch2.Amplitude"),
                         "Ch1.Amplitude"=c(30.20, -5.95),
                         "Ch2.Amplitude"=c(-5.95, 24.20),
                         row.names=1),
         "PN"=NULL,
         "PP"=NULL)
  maxDiff <- max(vapply(ddpcr$classes, getDfDiff, numeric(1), cl, knownCovs))
  expect_lte(maxDiff, 0.01)
})
