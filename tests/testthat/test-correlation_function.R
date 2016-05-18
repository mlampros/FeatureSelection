data(iris)
iris1 = iris
iris1$Species = as.numeric(iris1$Species)

context('Correlation function')


testthat::test_that("if any of the data columns is factor or character it returns an error", {

  testthat::expect_error(func_correlation(iris, target = NULL, correlation_thresh = 0.75, use_obs = "everything", correlation_method = "pearson"))
})


testthat::test_that("if the correlation_thresh is NULL it returns an error", {
  
  testthat::expect_error(func_correlation(iris[, -ncol(iris)], target = NULL, correlation_thresh = NULL, use_obs = "everything", correlation_method = "pearson"))
})

testthat::test_that("if the correlation_thresh is > 1.0 it returns an error", {
  
  testthat::expect_error(func_correlation(iris[, -ncol(iris)], target = NULL, correlation_thresh = 1.1, use_obs = "everything", correlation_method = "pearson"))
})

testthat::test_that("if the correlation_thresh is <= 0.0 it returns an error", {
  
  testthat::expect_error(func_correlation(iris[, -ncol(iris)], target = NULL, correlation_thresh = -1.0, use_obs = "everything", correlation_method = "pearson"))
})

testthat::test_that("if the use_obs is NULL it returns an error", {
  
  testthat::expect_error(func_correlation(iris[, -ncol(iris)], target = NULL, correlation_thresh = 0.75, use_obs = NULL, correlation_method = "pearson"))
})


testthat::test_that("if the use_obs is NULL it returns an error", {
  
  testthat::expect_error(func_correlation(iris[, -ncol(iris)], target = NULL, correlation_thresh = 0.75, use_obs = "everything", correlation_method = NULL))
})


testthat::test_that("if data is not a data frame or matrix it returns an error", {
  
  testthat::expect_error(func_correlation(list(iris[, -ncol(iris)]), target = NULL, correlation_thresh = 0.75, use_obs = "everything", correlation_method = "pearson"))
})


testthat::test_that("it takes a data frame without a target and it returns a list", {
  
  testthat::expect_true(is.list(func_correlation(iris1, target = NULL, correlation_thresh = 0.75, use_obs = "everything", correlation_method = "pearson")))
})

testthat::test_that("it takes a data frame with a target and it returns a data frame", {
  
  testthat::expect_true(is.data.frame(func_correlation(iris1, target = c('Species'), correlation_thresh = 0.75, use_obs = "everything", correlation_method = "pearson")))
})


testthat::test_that("it takes a data frame with a vector of predictors and it returns a data frame", {
  
  testthat::expect_true(is.data.frame(func_correlation(iris1, target = c('Species', 'Petal.Width'), correlation_thresh = 0.75, use_obs = "everything", correlation_method = "pearson")))
})


