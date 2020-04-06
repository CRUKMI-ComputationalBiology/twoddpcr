context("Loading data to the classes")

testDir <- system.file("testdata", package = "twoddpcr")
testWell <- ddpcrWell(file.path(testDir, "sample_B03_Amplitude.csv"))


test_that("Well names are correctly loaded", {
  expect_equal(names(readCSVDataFrame(testDir)), c("B03", "A04", "A05", "B06"))
  expect_equal(names(readCSVDataFrame(testDir, sortByLetter=TRUE)),
               c("A04", "A05", "B03", "B06"))
  expect_s3_class(readCSVDataFrame(testDir)[["B03"]], "data.frame")
  expect_s3_class(readCSVDataFrame(testDir)[["A04"]], "data.frame")
  expect_s3_class(readCSVDataFrame(testDir)[["A05"]], "data.frame")
  expect_s3_class(readCSVDataFrame(testDir)[["B06"]], "data.frame")
})

test_that("The objects are S4 objects", {
  expect_s4_class(ddpcrWell(), class="ddpcrWell")
  expect_s4_class(
    ddpcrWell(file.path(testDir, "sample_B03_Amplitude.csv")),
    class="ddpcrWell"
  )
  expect_s4_class(ddpcrPlate(), class="ddpcrPlate")
  expect_s4_class(ddpcrPlate(testDir), class="ddpcrPlate")
})

test_that("Files that don't exist are handled", {
  expect_error(ddpcrWell("a-file-that-doesnt-exist.csv"))
  expect_error(ddpcrPlate("a-dir-that-doesnt-exist"))
})

