context("k-means clustering")

testDir <- system.file("testdata", package = "twoddpcr")

test_that("k-means works for 3 classes", {
  testWell <- ddpcrWell(file.path(testDir, "sample_A04_Amplitude.csv"))
  centres <- matrix(c(5000, 1500, 5500, 7000, 10000, 2000),
                    ncol=2, byrow=TRUE)
  testWell <- kmeansClassify(testWell, centres=centres)
  kmc <- wellClassification(testWell, cMethod="kmeans")
  expect_equal(length(unique(kmc)), 3)
})

test_that("k-means works for 4 classes", {
  testPlate <- ddpcrPlate(testDir)
  centres <- matrix(c(5000, 1500, 5500, 7000, 10000, 2000, 9000, 6000),
                    ncol=2, byrow=TRUE)
  testPlate <- kmeansClassify(testPlate, centres=centres)
  kmc <- unlist(plateClassification(testPlate, cMethod="kmeans"))
  expect_equal(length(unique(kmc)), 4)
})

test_that("k-means error if clusters/centres are not close", {
  testWell <- ddpcrWell(file.path(testDir, "sample_A04_Amplitude.csv"))
  centres <- matrix(c(5000, 1500, 5500, 7000, 10000, 2000, 9000, 6000),
                    ncol=2, byrow=TRUE)
  expect_error(kmeansClassify(testWell, centres=centres))
})

test_that("k-means errors for centres", {
  testPlate <- ddpcrPlate(testDir)
  centres <- matrix(c(5000, 1500, 5500, 7000, 10000, 2000, 9000, 6000,
                      7000, 4000), ncol=2, byrow=TRUE)
  expect_error(kmeansClassify(testPlate, centres=centres))
  expect_error(kmeansClassify(testPlate, centres=5))

  noCentres <- matrix()
  expect_error(kmeansClassify(testPlate, centres=noCentres))
  expect_error(kmeansClassify(testPlate, centres=0))
  expect_error(kmeansClassify(testPlate, centres=-1))
  expect_error(kmeansClassify(testPlate, centres=3.5))
  expect_error(kmeansClassify(testPlate, centres="centres"))
})

