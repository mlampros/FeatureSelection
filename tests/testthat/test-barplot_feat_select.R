data(iris)

context('Bar plot feature selection')



# first check single methods:

testthat::test_that("it returns an error if the object is NULL", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = NULL)

  params_barplot = list(keep_features = NULL, horiz = TRUE, cex.names = 0.8)

  testthat::expect_error(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})


testthat::test_that("it returns an error if sort method of xgoobst not one of Frequency, Gain, Cover", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1, xgb_sort = NULL)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)

  testthat::expect_error(barplot_feat_select(res, params_barplot, xgb_sort = 'some_method'))
})

#===========================================
# use testthat::expect_silent to test plots
#===========================================

# feature_selection() function

testthat::test_that("glmnet for the feature_selection object, returns 3 column data frame (CV_folds > 1) in the res object, and plot", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = NULL)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})



testthat::test_that("it returns plot in case of binomial", {

  X = iris
  y = X[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'versicolor'
  y = as.factor(y)
  X = X[, -5]

  params_glmnet = list(alpha = 1, family = 'binomial', nfolds = 3, parallel = F)

  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = NULL, scale_coefs_glmnet = T, verbose = F)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})


testthat::test_that("glmnet for the feature_selection object, returns 2 column data frame (CV_folds = 1) in the res object, and plot", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 1, cores_glmnet = NULL)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})



testthat::test_that("xgboost for the feature_selection object, xgb_sort Frequency", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1, xgb_sort = NULL)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = 'Frequency'))
})


testthat::test_that("xgboost for the feature_selection object, xgb_sort Gain", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1, xgb_sort = NULL)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = 'Gain'))
})


testthat::test_that("xgboost for the feature_selection object, xgb_sort Cover", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 1, xgb_sort = NULL)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = 'Cover'))
})


testthat::test_that("ranger for the feature_selection object", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_ranger = list(write.forest = TRUE, probability = F, num.threads = 6, num.trees = 50, verbose = FALSE, classification = F, mtry = 2, min.node.size = 5, importance = 'impurity')

  res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 5)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})


# wrapper_feat_select() function


testthat::test_that("plot of all methods when union = T", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = NULL, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})


testthat::test_that("plot of all methods when union = T", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = NULL, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = 'Gain'))
})


testthat::test_that("plot of all methods when union = T", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = NULL, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = 'Cover'))
})

testthat::test_that("plot of all methods when union = F", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = F)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = NULL, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})


testthat::test_that("plot two of the methods when union = F", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]


  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = F)

  res = wrapper_feat_select(X, y, params_glmnet = NULL, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = 'Gain'))
})


testthat::test_that("plot one of the methods when union = F, [ ranger ]", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = F)

  res = wrapper_feat_select(X, y, params_glmnet = NULL, params_xgboost = NULL, params_ranger = params_ranger, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})


testthat::test_that("plot one of the methods when union = F [ glmnet - lasso ]", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)


  params_features = list(keep_number_feat = NULL, union = F)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = NULL, params_ranger = NULL, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = NULL, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})


testthat::test_that("plot one of the methods when union = F [ xgboost ]", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_features = list(keep_number_feat = NULL, union = F)

  res = wrapper_feat_select(X, y, params_glmnet = NULL, params_xgboost = params_xgboost, params_ranger = NULL, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = NULL))
})


testthat::test_that("plot two of the methods when union = T", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]


  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, y, params_glmnet = NULL, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)

  testthat::expect_silent(barplot_feat_select(res, params_barplot, xgb_sort = 'Gain'))
})

