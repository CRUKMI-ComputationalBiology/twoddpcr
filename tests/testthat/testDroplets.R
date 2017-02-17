context("Retrieving droplets")

test_that("Droplets can be retrieved in a data frame", {
  expect_s3_class(amplitudes(ddpcrWell()), "data.frame")
  expect_s3_class(amplitudes(ddpcrWell("data/sample_B03_Amplitude.csv")),
                  class="data.frame")
  expect_equal(class(amplitudes(ddpcrPlate())), "data.frame")
  expect_equal(class(amplitudes(ddpcrPlate("data"))), "list")
  expect_equal(class(do.call(rbind, amplitudes(ddpcrPlate("data")))),
               "data.frame")
})

test_that("The wells names are loaded from filenames", {
  expect_equal(names(ddpcrPlate("data")), c("B03", "A04", "A05", "B06"))
})

test_that("There are the right number of droplets", {
  expect_equal(numDroplets(ddpcrWell()), 0)
  expect_equal(numDroplets(ddpcrWell("data/sample_B03_Amplitude.csv")), 6)
  expect_equal(numDroplets(ddpcrPlate()), c())
  expect_equivalent(numDroplets(ddpcrPlate("data")), c(6, 4, 4, 10))
  expect_equivalent(numDroplets(ddpcrPlate(list(A01=ddpcrWell()))), c(0),
                    info="Plate with an empty well")
})

test_that("isEmpty works as expected", {
  expect_true(isEmpty(ddpcrWell()))
  expect_true(isEmpty(ddpcrPlate()))
  expect_true(isEmpty(ddpcrPlate(list(A01=ddpcrWell()))),
              info="Plate with an empty well")
  
  expect_false(isEmpty(ddpcrWell("data/sample_B03_Amplitude.csv")))
  expect_false(isEmpty(ddpcrPlate("data")))
})

