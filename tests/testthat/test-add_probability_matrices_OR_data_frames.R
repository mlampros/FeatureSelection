context("Add probability data frames or matrices")

testthat::test_that("adding 3 matrices or data.frames results in a a matrix of all 2's", {
  m1 = matrix(rep(1, 25), 5, 5)
  m2 = matrix(rep(2, 25), 5, 5)
  m3 = matrix(rep(3, 25), 5, 5)
  
  lst = list(m1, m2, m3)
  
  testthat::expect_equal(add_probs_dfs(lst), matrix(rep(2, 25), 5,5), check.attributes = FALSE)     # check.attributes = F otherwise due to dimnames error
})


testthat::test_that("if PREDS_LST is a matrix throws an error", {
  m1 = matrix(rep(1, 25), 5, 5)

  testthat::expect_error(add_probs_dfs(m1), "PREDS_LST must be a list")
})


testthat::test_that("if the dimensions of each matrix or data frame in the list is different it throws an error", {
  m1 = matrix(rep(1, 25), 5, 5)
  m2 = matrix(rep(2, 100), 10, 10)
  m3 = matrix(rep(3, 25), 5, 5)
  
  lst = list(m1, m2, m3)
  
  testthat::expect_error(add_probs_dfs(lst), "the dimensions of the included data.frames or matrices differ")
})

