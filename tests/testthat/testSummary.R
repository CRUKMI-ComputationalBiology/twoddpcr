context("Plate summary")

testPlate <- ddpcrPlate("data")
centres <- matrix(c(5000, 1500, 5500, 7000, 10000, 2000, 9000, 6000),
                  ncol=2, byrow=TRUE)
testPlate <- kmeansClassify(testPlate, centres=centres)
kmeansSummary <- plateSummary(testPlate, cMethod="kmeans")

test_that("Summary generated", {
  expect_equal(rownames(kmeansSummary), c("B03", "A04", "A05", "B06"))
  expect_true(all(kmeansSummary$PP %% 1 == 0))
  expect_true(all(kmeansSummary$PN %% 1 == 0))
  expect_true(all(kmeansSummary$NP %% 1 == 0))
  expect_true(all(kmeansSummary$NN %% 1 == 0))
  expect_true(all(kmeansSummary$AcceptedDroplets %% 1 == 0))
  expect_true(all(kmeansSummary$MtPositives %% 1 == 0))
  expect_true(all(kmeansSummary$MtNegatives %% 1 == 0))
  expect_true(all(kmeansSummary$WtPositives %% 1 == 0))
  expect_true(all(kmeansSummary$WtNegatives %% 1 == 0))
  expect_true(all(is.numeric(kmeansSummary$MtConcentration)))
  expect_true(all(is.numeric(kmeansSummary$WtConcentration)))
  expect_true(all(is.numeric(kmeansSummary$MtCopiesPer20uLWell)))
  expect_true(all(is.numeric(kmeansSummary$WtCopiesPer20uLWell)))
  expect_true(all(is.numeric(kmeansSummary$TotalCopiesPer20uLWell)))
  expect_true(all(is.numeric(kmeansSummary$TotalCopiesPer20uLWell)))
  expect_true(all(is.numeric(kmeansSummary$Ratio)))
  expect_true(all(is.numeric(kmeansSummary$FracAbun)))
})

testPlate <- mahalanobisRain(testPlate, cMethod="kmeans", maxDistances=3)
rainySummary <- plateSummary(testPlate, cMethod="kmeansMahRain")

test_that("Rain is removed", {
  expect_true(all(rainySummary$PP <= kmeansSummary$PP))
  expect_true(all(rainySummary$PN <= kmeansSummary$PN))
  expect_true(all(rainySummary$NP <= kmeansSummary$NP))
  expect_true(all(rainySummary$NN <= kmeansSummary$NN))
  expect_true(all(rainySummary$AcceptedDroplets <=
                  kmeansSummary$AcceptedDroplets))
  expect_true(any(rainySummary$AcceptedDroplets <
                  kmeansSummary$AcceptedDroplets))
})
