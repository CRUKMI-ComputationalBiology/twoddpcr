context("k-NN classification")

testDir <- system.file("testdata", package = "twoddpcr")

# Create some training data.
trAm <- readCSVDataFrame(file.path(testDir, "sample_A05_Amplitude.csv"))[[1]]
trAm <- trAm[, c("Ch1.Amplitude", "Ch2.Amplitude")]
trCl <- gridClassify(trAm,
                     ch1NNThreshold=6500, ch2NNThreshold=1900,
                     ch1NPThreshold=6500, ch2NPThreshold=5000,
                     ch1PNThreshold=9000, ch2PNThreshold=2500,
                     ch1PPThreshold=7500, ch2PPThreshold=5000,
                     fullTable=FALSE)

test_that("k-NN still works when the test set has fewer clusters", {
  testWell <- ddpcrWell(file.path(testDir, "sample_A04_Amplitude.csv"))
  testWell <- knnClassify(testWell, trAm, trCl, k=1)
  knc <- wellClassification(testWell, "knn")
  expect_equal(length(unique(knc)), 3)
})

test_that("k-NN works for a ddpcrPlate", {
  testPlate <- ddpcrPlate(testDir)
  testPlate <- knnClassify(testPlate, trAm, trCl, k=1)
  knc <- unlist(plateClassification(testPlate, "knn"))
  expect_equal(length(unique(knc)), 4)
})

test_that("k-NN works for a ddpcrPlate", {
  testPlate <- ddpcrPlate(testDir)
  testPlate <- knnClassify(testPlate, trAm, trCl, k=3)
  knc <- unlist(plateClassification(testPlate, "knn"))
  expect_equal(length(unique(knc)), 4)
})

