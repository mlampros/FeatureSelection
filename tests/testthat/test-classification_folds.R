context("Split data in classification folds")


testthat::test_that("throws an error if the RESP is not a factor", {
  
  y = c(1:10)
  
  testthat::expect_error(class_folds(5, y, shuffle = T), "RESP must be a factor")
})


testthat::test_that("returns a warning if the folds are not equally split", {
  
  y = as.factor(sample(1:5, 99, replace = T))
  
  testthat::expect_warning(class_folds(5, y, shuffle = T), 'the folds are not equally split')
})

testthat::test_that("the number of folds equals the number of the resulted sublist indices", {
  
  y = as.factor(sample(1:5, 100, replace = T))
  
  testthat::expect_length(class_folds(5, y, shuffle = T), 5)
})


# return object with shuffle = F

testthat::test_that("the number of folds equals the number of the resulted sublist indices", {
  
  y = as.factor(sample(1:5, 100, replace = T))
  
  testthat::expect_length(class_folds(5, y, shuffle = F), 5)
})
