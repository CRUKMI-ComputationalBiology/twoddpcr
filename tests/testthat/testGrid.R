context("Classifying by grid")

testWell <- ddpcrWell("data/sample_B03_Amplitude.csv")
testPlate <- ddpcrPlate("data")

test_that("gridClassify classifies the droplets correctly", {
  testPlate <- gridClassify(testPlate,
                            ch1NNThreshold=6500, ch2NNThreshold=1900,
                            ch1NPThreshold=6500, ch2NPThreshold=5000,
                            ch1PNThreshold=9000, ch2PNThreshold=2500,
                            ch1PPThreshold=7500, ch2PPThreshold=5000)
  cl <- unlist(plateClassification(testPlate, cMethod="grid"))
  expect_equal(sum(cl == "NN"), 9)
  expect_equal(sum(cl == "NP"), 8)
  expect_equal(sum(cl == "PN"), 3)
  expect_equal(sum(cl == "PP"), 2)
  expect_equal(sum(cl == "Rain"), 2)
})

test_that("thresholdClassify classifies the droplets correctly", {
  testPlate <- thresholdClassify(testPlate,
                                 ch1Threshold=7000, ch2Threshold=3000)
  cl <- unlist(plateClassification(testPlate, cMethod="thresholds"))
  expect_equal(sum(cl == "NN"), 10)
  expect_equal(sum(cl == "NP"), 9)
  expect_equal(sum(cl == "PN"), 3)
  expect_equal(sum(cl == "PP"), 2)
  expect_equal(sum(cl == "Rain"), 0)
})

test_that("gridClassify stops if the thresholds overlap", {
  expect_error(
    gridClassify(testPlate,
                 ch1NNThreshold=6500, ch2NNThreshold=3000,
                 ch1NPThreshold=6500, ch2NPThreshold=3000,
                 ch1PNThreshold=6499, ch2PNThreshold=3000,
                 ch1PPThreshold=6500, ch2PPThreshold=3000)
  )
  expect_error(
    gridClassify(testPlate,
                 ch1NNThreshold=6500, ch2NNThreshold=3000,
                 ch1NPThreshold=6500, ch2NPThreshold=3000,
                 ch1PNThreshold=6500, ch2PNThreshold=3000,
                 ch1PPThreshold=6499, ch2PPThreshold=3000)
  )
  expect_error(
    gridClassify(testPlate,
                 ch1NNThreshold=6500, ch2NNThreshold=3000,
                 ch1NPThreshold=6500, ch2NPThreshold=2999,
                 ch1PNThreshold=6500, ch2PNThreshold=3000,
                 ch1PPThreshold=6500, ch2PPThreshold=3000)
  )
  expect_error(
    gridClassify(testPlate,
                 ch1NNThreshold=6500, ch2NNThreshold=3000,
                 ch1NPThreshold=6500, ch2NPThreshold=3000,
                 ch1PNThreshold=6500, ch2PNThreshold=3000,
                 ch1PPThreshold=6500, ch2PPThreshold=2999)
  )
  expect_error(
    gridClassify(testPlate,
                 ch1NNThreshold=6000, ch2NNThreshold=3000,
                 ch1NPThreshold=6000, ch2NPThreshold=3000,
                 ch1PNThreshold=6000, ch2PNThreshold=3000,
                 ch1PPThreshold=6000, ch2PPThreshold=3000),
    regexp=NA
  )
})
