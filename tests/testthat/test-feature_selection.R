data(iris)


context('Feature selection')

#====================
# handling of errors
#====================


testthat::test_that("feature selection returns a error if method is not specified", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  testthat::expect_error(feature_selection(X, y, method = NULL, params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL))
})


testthat::test_that("feature selection returns a error if CV less than 1", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  testthat::expect_error(feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 0, cores_glmnet = NULL))
})


testthat::test_that("glmnet lasso returns a error if data is not a data frame for folds = 1", {

  X = iris[, -5]
  y = X[, 1]
  X = list(X[, -1])

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  testthat::expect_error(feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL))
})


testthat::test_that("glmnet lasso returns a error if data is not a data frame for folds = 5", {

  X = iris[, -5]
  y = X[, 1]
  X = list(X[, -1])

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  testthat::expect_error(feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = NULL))
})


#===========================================================
# handling NA's in glmnet-lasso
# xgboost, ranger treat missing values as an extra category
#===========================================================


testthat::test_that("missing values in data frame will be replaced with the median, in case of CV_folds = 1", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  X[, 1] = func_nas(X, 1)                    # add NA's to first and last column
  X[, ncol(X)] = func_nas(X, ncol(X))


  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("missing values in data frame will be replaced with the median, in case of CV_folds = 5", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  X[, 1] = func_nas(X, 1)                    # add NA's to first and last column
  X[, ncol(X)] = func_nas(X, ncol(X))


  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = NULL)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("missing values in sparse Matrix will be replaced with the median, in case of CV_folds = 1", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  X[, 1] = func_nas(X, 1)                    # add NA's to first and last column
  X[, ncol(X)] = func_nas(X, ncol(X))

  X = Matrix::Matrix(as.matrix(X), sparse = T)       # convert to sparse matrix

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("missing values in sparse Matrix will be replaced with the median, in case of CV_folds = 5", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  X[, 1] = func_nas(X, 1)                    # add NA's to first and last column
  X[, ncol(X)] = func_nas(X, ncol(X))
  X = Matrix::Matrix(as.matrix(X), sparse = T)     # convert to sparse matrix

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = NULL)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})

#======================
# test - cases : glmnet
#======================


testthat::test_that("glmnet returns a data frame with the important predictors, in case 3 folds", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = NULL, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("glmnet returns a data frame with the important predictors, in case 1 fold", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("glmnet returns an error if data is not a matrix, data.frame or sparse matrix , in case of fold = 1", {

  X = iris[, -5]
  y = X[, 1]
  X = list(X[, -1])

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  testthat::expect_error(feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL))       # is.data.frame and not empty
})


testthat::test_that("glmnet takes a matrix and returns a data frame with the important predictors, in case 1 fold", {

  X = iris[, -5]
  y = X[, 1]
  X = as.matrix(X[, -1])

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("glmnet takes a matrix and returns a data frame with the important predictors, in case 3 fold", {

  X = iris[, -5]
  y = X[, 1]
  X = as.matrix(X[, -1])

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 3, cores_glmnet = NULL)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("glmnet takes a sparse matrix and returns a data frame with the important predictors, in case 1 fold", {

  X = iris[, -5]
  y = X[, 1]
  X = Matrix::Matrix(as.matrix(X[, -1]), sparse = T)

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})

testthat::test_that("glmnet takes a sparse matrix and returns a data frame with the important predictors, in case 3 fold", {

  X = iris[, -5]
  y = X[, 1]
  X = Matrix::Matrix(as.matrix(X[, -1]), sparse = T)

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 3, cores_glmnet = NULL, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("glmnet takes a sparse matrix and returns a data frame with the important predictors, in case of 1 fold, here use argument scale_coefs_glmnet AND verbose", {

  X = iris[, -5]
  y = X[, 1]
  X = Matrix::Matrix(as.matrix(X[, -1]), sparse = T)

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL, scale_coefs_glmnet = T, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})

testthat::test_that("glmnet takes a sparse matrix and returns a data frame with the important predictors, in case of 3 folds, here use argument scale_coefs_glmnet AND verbose", {

  X = iris[, -5]
  y = X[, 1]
  X = Matrix::Matrix(as.matrix(X[, -1]), sparse = T)

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 3, cores_glmnet = NULL, scale_coefs_glmnet = T, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})

testthat::test_that("glmnet takes a data frame and in case of the binomial classification it returns a data frame with the important predictors, in case of 1 fold", {

  X = iris
  y = X[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'versicolor'
  y = as.factor(y)
  X = X[, -5]

  params_glmnet = list(alpha = 1, family = 'binomial', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL, scale_coefs_glmnet = T, verbose = F)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("glmnet takes a data frame and in case of the binomial classification it returns a data frame with the important predictors, in case of 3 fold", {

  X = iris
  y = X[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'versicolor'
  y = as.factor(y)
  X = X[, -5]

  params_glmnet = list(alpha = 1, family = 'binomial', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 3, cores_glmnet = NULL, scale_coefs_glmnet = T, verbose = F)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("glmnet takes a data frame and in case of the multinomial classification it returns a data frame with the important predictors, in case of 1 fold", {

  X = iris
  y = X[, 5]
  X = X[, -5]

  params_glmnet = list(alpha = 1, family = 'multinomial', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL, scale_coefs_glmnet = T, verbose = F)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})

testthat::test_that("glmnet takes a data frame and in case of the multinomial classification it returns a data frame with the important predictors, in case of 3 fold", {

  X = iris
  y = X[, 5]
  X = X[, -5]

  params_glmnet = list(alpha = 1, family = 'multinomial', nfolds = 3, parallel = F)
  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 3, cores_glmnet = NULL, scale_coefs_glmnet = T, verbose = F)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


#=======================
# test - cases : xgboost
#=======================


testthat::test_that("xgboost returns an error if data not data.frame or matrix, if folds = 1", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = list(iris[, -5])

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  testthat::expect_error(feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1))       # is.data.frame and not empty
})


testthat::test_that("xgboost returns an error if data not data.frame or matrix, if folds = 5", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = list(iris[, -5])

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  testthat::expect_error(feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 5))       # is.data.frame and not empty
})


testthat::test_that("xgboost returns a data frame with the important predictors, in case 1 folds, default xgb_sort = Frequency", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("xgboost returns a data frame with the important predictors, in case 1 folds, xgb_sort = Gain", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1, xgb_sort = 'Gain', verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("xgboost returns a data frame with the important predictors, in case 1 folds, xgb_sort = Cover", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1, xgb_sort = 'Cover')

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})



testthat::test_that("xgboost using sparse Matrix returns a data frame with the important predictors, in case 1 folds", {

  X = iris[, -5]
  y = X[, 1]
  X = Matrix::Matrix(as.matrix(X[, -1]), sparse = T)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.35, "subsample" = 0.65, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, y, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("xgboost using sparse Matrix returns a data frame with the important predictors, in case 3 folds, default xgb_sort = Frequency", {

  X = iris[, -5]
  y = X[, 1]
  X = Matrix::Matrix(as.matrix(X[, -1]), sparse = T)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.35, "subsample" = 0.65, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, y, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 3)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("xgboost using sparse Matrix returns a data frame with the important predictors, in case 3 folds, default xgb_sort = Frequency", {

  X = iris[, -5]
  y = X[, 1]
  X = Matrix::Matrix(as.matrix(X[, -1]), sparse = T)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.35, "subsample" = 0.65, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, y, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 3, xgb_sort = 'Gain', verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})



testthat::test_that("xgboost using sparse Matrix returns a data frame with the important predictors, in case 3 folds, default xgb_sort = Frequency", {

  X = iris[, -5]
  y = X[, 1]
  X = Matrix::Matrix(as.matrix(X[, -1]), sparse = T)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.35, "subsample" = 0.65, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, y, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 3, xgb_sort = 'Cover')

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("xgboost using data.frame in multiclass classification, returns a data frame with the important predictors, in case 3 folds, default xgb_sort = Frequency", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 3, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("xgboost using data.frame in binary classification, returns a data frame with the important predictors, in case 5 folds, default xgb_sort = Frequency", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, 1)
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "binary:logistic", "bst:eta" = 0.35, "subsample" = 0.65, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 5)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})



#=======================
# test - cases : ranger
#=======================


testthat::test_that("ranger returns an error if data not data.frame or matrix, folds = 1", {

  y = iris[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'virginica'
  X = list(iris[, -5])

  params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

  testthat::expect_error(feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 1))       # is.data.frame and not empty
})

testthat::test_that("ranger returns an error if data not data.frame or matrix, folds > 1", {

  y = iris[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'virginica'
  X = list(iris[, -5])

  params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

  testthat::expect_error(feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 3))       # is.data.frame and not empty
})

testthat::test_that("ranger returns a data.frame with important predictors if folds = 1", {

  y = iris[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'virginica'
  X = iris[, -5]

  params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

  res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 1, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("ranger returns a data.frame with important predictors if folds = 5", {

  y = iris[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'virginica'
  X = iris[, -5]

  params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

  res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 5, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("ranger returns a data.frame with important predictors if folds = 1, in regression", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_ranger = list(write.forest = TRUE, probability = F, num.threads = 6, num.trees = 50, verbose = FALSE, classification = F, mtry = 2, min.node.size = 5, importance = 'impurity')

  res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 1, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("ranger returns a data.frame with important predictors if folds = 5, in regression", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_ranger = list(write.forest = TRUE, probability = F, num.threads = 6, num.trees = 50, verbose = FALSE, classification = F, mtry = 2, min.node.size = 5, importance = 'impurity')

  res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 5, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})



testthat::test_that("ranger returns a data.frame with important predictors if folds = 5, in regression", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_ranger = list(write.forest = TRUE, probability = F, num.threads = 6, num.trees = 50, verbose = FALSE, classification = F, mtry = 2, min.node.size = 5, importance = 'impurity')

  res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 5)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("ranger returns a data.frame with important predictors if folds = 1, in regression, when using dependent.variable.name ", {

  X = iris
  y = iris[, 'Species']           # IN case that I give dependent.variable.name , THEN I should also specify the response variable (y), so that folds can be built

  params_ranger = list(dependent.variable.name = "Species", write.forest = TRUE, probability = T, num.threads = 6, num.trees = 50, verbose = FALSE, classification = T, mtry = 2,

                       min.node.size = 5, importance = 'impurity')

  res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 1)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})


testthat::test_that("ranger returns a data.frame with important predictors if folds = 3, in regression, when using dependent.variable.name ", {

  X = iris
  y = iris[, 'Species']           # IN case that I give dependent.variable.name , THEN I should also specify the response variable (y), so that folds can be built

  params_ranger = list(dependent.variable.name = "Species", write.forest = TRUE, probability = T, num.threads = 6, num.trees = 50, verbose = FALSE, classification = T, mtry = 2,

                       min.node.size = 5, importance = 'impurity')

  res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 3, verbose = T)

  testthat::expect_true(is.data.frame(res) & sum(dim(res)) > 0)       # is.data.frame and not empty
})

