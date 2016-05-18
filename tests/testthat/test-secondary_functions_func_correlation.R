context('Secondary functions func_correlation')

# 'remove_duplic_func' function

testthat::test_that("takes a data frame and returns a data frame", {
  
  df = data.frame(a = sample(1:4, 100, replace = T))
  
  testthat::expect_true(is.data.frame(remove_duplic_func(df)))
})


testthat::test_that("takes a single column data frame and returns a 3-column data frame", {
  
  df = data.frame(a = sample(1:4, 100, replace = T))
  
  out = ncol(remove_duplic_func(df))
  
  testthat::expect_true(out == 3)
})


# 'second_func_cor' function

testthat::test_that("takes a data frame and returns a list", {
  
  df = data.frame(a = sample(0:4, 100, replace = T), d = sample(0:6, 100, replace = T))
  
  testthat::expect_true(is.list(second_func_cor(df)))
})


testthat::test_that("takes a data frame and returns a list, the length of the list equals the number of columns of the data frame", {
  
  df = data.frame(a = sample(0:4, 100, replace = T), d = sample(0:6, 100, replace = T))
  
  lst = second_func_cor(df)
  
  testthat::expect_true(ncol(df) == length(lst))
})

