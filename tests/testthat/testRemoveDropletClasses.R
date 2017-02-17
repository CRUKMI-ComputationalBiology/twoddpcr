context("Removing droplet classes")

# Dummy data set with classification to check.
df <- data.frame("Ch1.Amplitude"=c(1, 2, 3, 4, 5, 6),
                 "Ch2.Amplitude"=c(1, 2, 3, 4, 5, 6),
                 "class"=c("NN", "NP", "PN", "PP", "N/A", "Rain"))

test_that("Classes are correctly removed (without rain)", {
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c(),
                                keepUnclassified=FALSE)
    as.character(res$class)
  }, c("NN", "NP", "PN", "PP"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("NN"),
                                keepUnclassified=FALSE)
    as.character(res$class)
  }, c("NP", "PN", "PP"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("NP"),
                                keepUnclassified=FALSE)
    as.character(res$class)
  }, c("NN", "PN", "PP"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("PN"),
                                keepUnclassified=FALSE)
    as.character(res$class)
  }, c("NN", "NP", "PP"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("PP"),
                                keepUnclassified=FALSE)
    as.character(res$class)
  }, c("NN", "NP", "PN"))
})

test_that("Classes are correctly removed (without rain)", {
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c(),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NN", "NP", "PN", "PP", "N/A", "Rain"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("NN"),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NP", "PN", "PP", "N/A", "Rain"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("NP"),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NN", "PN", "PP", "N/A", "Rain"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("PN"),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NN", "NP", "PP", "N/A", "Rain"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("PP"),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NN", "NP", "PN", "N/A", "Rain"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("N/A"),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NN", "NP", "PN", "PP", "Rain"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class", classesToRemove=c("Rain"),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NN", "NP", "PN", "PP", "N/A"))
})

test_that("Classes are correctly removed (multiple classes)", {
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class",
                                classesToRemove=c("PN", "PP"),
                                keepUnclassified=FALSE)
    as.character(res$class)
  }, c("NN", "NP"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class",
                                classesToRemove=c("NP", "PP"),
                                keepUnclassified=FALSE)
    as.character(res$class)
  }, c("NN", "PN"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class",
                                classesToRemove=c("PN", "PP"),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NN", "NP", "N/A", "Rain"))
  expect_equal({
    res <- removeDropletClasses(df, cMethod="class",
                                classesToRemove=c("NP", "PP"),
                                keepUnclassified=TRUE)
    as.character(res$class)
  }, c("NN", "PN", "N/A", "Rain"))
})

