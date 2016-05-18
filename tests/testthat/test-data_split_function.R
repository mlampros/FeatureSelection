context('Split of data')


testthat::test_that("throws an error if the TrainRation is less than expected", {
  
  y = c(1:50)
  
  testthat::expect_error(DataSplit(y, TrainRatio = 1.1), 'TrainRation should be a float number greater than 0 and less than 1.0')
})


testthat::test_that("returns a list of indices", {
  
  y = c(1:50)
  
  testthat::expect_true(is.list(DataSplit(y, TrainRatio = 0.75)))
})


# test function with regression = T, F and shuffle = T, F

testthat::test_that("returns a list of indices", {
  
  y = as.factor(sample(letters[1:2], 50 , replace = T))
  
  testthat::expect_true(is.list(DataSplit(y, TrainRatio = 0.75, regression = F)))
})

testthat::test_that("returns a list of indices", {
  
  y = c(1:50)
  
  testthat::expect_true(is.list(DataSplit(y, TrainRatio = 0.75, shuffle = T)))
})

testthat::test_that("returns a list of indices", {
  
  y = c(1:50)
  
  testthat::expect_true(is.list(DataSplit(y, TrainRatio = 0.75, shuffle = F)))
})
