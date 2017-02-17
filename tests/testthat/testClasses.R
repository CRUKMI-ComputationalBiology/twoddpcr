context("Loading data to the classes")

test_that("Well names are correctly loaded", {
  expect_equal(names(readCSVDataFrame("data")), c("B03", "A04", "A05", "B06"))
  expect_equal(names(readCSVDataFrame("data", sortByLetter=TRUE)),
               c("A04", "A05", "B03", "B06"))
  expect_s3_class(readCSVDataFrame("data")[["B03"]], "data.frame")
  expect_s3_class(readCSVDataFrame("data")[["A04"]], "data.frame")
  expect_s3_class(readCSVDataFrame("data")[["A05"]], "data.frame")
  expect_s3_class(readCSVDataFrame("data")[["B06"]], "data.frame")
})

test_that("The objects are S4 objects", {
  expect_s4_class(ddpcrWell(), class="ddpcrWell")
  expect_s4_class(ddpcrWell("data/sample_B03_Amplitude.csv"), class="ddpcrWell")
  expect_s4_class(ddpcrPlate(), class="ddpcrPlate")
  expect_s4_class(ddpcrPlate("data"), class="ddpcrPlate")
})

test_that("Files that don't exist are handled", {
  expect_error(ddpcrWell("a-file-that-doesnt-exist.csv"))
  expect_error(ddpcrPlate("a-dir-that-doesnt-exist"))
})

