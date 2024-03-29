context("Recent changes")

test_that("Recentchanges feed entries can be retrieved through recent_changes", {
  skip_on_cran()
  expect_true({recent_changes("en","wikipedia", limit=1);TRUE})
})

test_that("recent_changes respects tags", {
  skip_on_cran()
  expect_true({recent_changes("en","wikipedia", limit=1, tag = "mobile edit");TRUE})
})

test_that("recent_changes respects types", {
  skip_on_cran()
  expect_true({recent_changes("en","wikipedia", limit=1, type = "new");TRUE})
})

test_that("recent_changes respects directional changes", {
  skip_on_cran()
  expect_true({recent_changes("en","wikipedia", limit=1, top = TRUE);TRUE})
})
