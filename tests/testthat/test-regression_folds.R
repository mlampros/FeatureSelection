context("Split data in regression folds")


testthat::test_that("throws an error if the RESP is not a factor", {
  
  y = as.factor(c(1:50))
  
  testthat::expect_error(regr_folds(5, y, stratified = F), "this function is meant for regression for classification use the 'class_folds' function")
})


testthat::test_that("returns a warning if the folds are not equally split", {
  
  y = sample(1:5, 99, replace = T)
  
  testthat::expect_warning(regr_folds(5, y, stratified = F), 'the folds are not equally split')
})

testthat::test_that("the number of folds equals the number of the resulted sublist indices", {
  
  y = sample(1:5, 100, replace = T)
  
  testthat::expect_length(regr_folds(5, y, stratified = F), 5)
})

# object with stratified = T

testthat::test_that("the number of folds equals the number of the resulted sublist indices", {
  
  y = sample(1:5, 100, replace = T)
  
  testthat::expect_length(regr_folds(5, y, stratified = T), 5)
})
