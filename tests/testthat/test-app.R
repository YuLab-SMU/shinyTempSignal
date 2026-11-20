test_that("extract_date_match prefers captured groups", {
  expect_equal(
    extract_date_match("1_NC428479_2013_Human_China", "^[^_]+_[^_]+_(\\d{4})"),
    "2013"
  )
})

test_that("override_tip_dates uses external dates by label", {
  labels <- c("a", "b")
  date <- c("2010", NA)
  external <- data.frame(label = c("b", "c"), date = c("2011", "2012"))

  expect_equal(
    override_tip_dates(labels, date, external),
    c("2010", "2011")
  )
})

test_that("resolve_tip_dates handles tip label regex and csv override", {
  tree <- structure(
    list(
      edge = matrix(c(3, 1, 3, 2), byrow = TRUE, ncol = 2),
      tip.label = c("1_NC428479_2013_Human_China", "2_NC428480_2014_Swine_USA"),
      edge.length = c(1, 1),
      Nnode = 1
    ),
    class = "phylo"
  )
  external <- data.frame(
    label = "2_NC428480_2014_Swine_USA",
    date = "2015"
  )

  expect_equal(
    resolve_tip_dates(tree, "^[^_]+_[^_]+_(\\d{4})", "yyyy", external),
    c(2013, 2015)
  )
})
