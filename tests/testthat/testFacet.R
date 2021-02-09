context("Facet plot")

krasPlate <- ddpcrPlate(KRASdata)

test_that("Facet plot gives a output with correct parameters", {
  expect_s3_class(facetPlot(krasPlate), c("gg", "ggplot"))
  expect_s3_class(facetPlot(krasPlate, cMethod=NULL), c("gg", "ggplot"))
  expect_s3_class(facetPlot(krasPlate, cMethod="Cluster"), c("gg", "ggplot"))
})

test_that("Facet plot fails when plotting the wrong column", {
  expect_error(facetPlot(krasPlate, cMethod="Well"))
  expect_error(facetPlot(krasPlate, cMethod="AAA"))
})

