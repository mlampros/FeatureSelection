context('Shuffle and normalize data functions')


# shuffle data 

testthat::test_that("shuffle data takes a vector as input and returns a vector as output", {
  
  y = c(1:50)
  
  testthat::expect_true(is.vector(func_shuffle(y, times = 10)))
})

testthat::test_that("the length of the input vector equals the length of the output vector", {
  
  y = c(1:50)
  
  output = func_shuffle(y, times = 10)
  
  testthat::expect_true(length(y) == length(output))
})


# normalize data


testthat::test_that("normalize data takes a vector as input and returns a vector as output", {
  
  y = c(1:50)
  
  testthat::expect_true(is.vector(normalized(y)))
})

testthat::test_that("the length of the input vector equals the length of the output vector", {
  
  y = c(1:50)
  
  output = normalized(y)
  
  testthat::expect_true(length(y) == length(output))
})
