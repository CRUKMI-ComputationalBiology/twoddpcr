context("SD rain")

test_that("SD rain works for small sample", {
  testWell <- ddpcrWell("data/sample_B03_Amplitude.csv")
  centres <- matrix(c(5000, 1500, 5500, 7000, 10000, 2000, 9000, 6000),
                    ncol=2, byrow=TRUE)
  testWell <- kmeansClassify(testWell, centres=centres)
  expect_error(sdRain(testWell, cMethod="kmeans", errorLevel=3), NA)
})

test_that("SD rain removes ambiguous droplets", {
  testPlate <- ddpcrPlate("data")
  centres <- matrix(c(5000, 1500, 5500, 7000, 10000, 2000, 9000, 6000),
                    ncol=2, byrow=TRUE)
  testPlate <- kmeansClassify(testPlate, centres=centres)
  testPlate <- sdRain(testPlate, cMethod="kmeans", errorLevel=3)
  
  kmeansCl <- unlist(plateClassification(testPlate, cMethod="kmeans"))
  rainCl <- unlist(plateClassification(testPlate, cMethod="kmeansSdRain"))
  
  expect_true("kmeansSdRain" %in% commonClassificationMethod(testPlate))
  
  expect_lte(sum(rainCl == "NN"), sum(kmeansCl == "NN"))
  expect_lte(sum(rainCl == "NP"), sum(kmeansCl == "NP"))
  expect_lte(sum(rainCl == "PN"), sum(kmeansCl == "PN"))
  expect_lte(sum(rainCl == "PP"), sum(kmeansCl == "PP"))
  
  # The above tests are for <=, but the test data has been constructed so that 
  # there are some major outliers, hence the following is strictly > 0.
  expect_gt(sum(rainCl == "Rain"), 0)
  
  # These should also be > 0.
  expect_gt(sum(rainCl == "NN"), 0)
  expect_gt(sum(rainCl == "NP"), 0)
  expect_gt(sum(rainCl == "PN"), 0)
  expect_gt(sum(rainCl == "PP"), 0)
})

