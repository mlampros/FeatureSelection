data(iris)

context('Wraps all the methods')


testthat::test_that("it returns an error if all methods are NULL", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 100, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 100, verbose = TRUE, classification = FALSE, mtry = 5, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = TRUE)

  testthat::expect_error(wrapper_feat_select(X, y, params_glmnet = NULL, params_xgboost = NULL, params_ranger = NULL, xgb_sort = NULL,
                                             CV_folds = 1, stratified_regr = FALSE, cores_glmnet = NULL, params_features = params_features))
})


testthat::test_that("it returns an error if cv folds less than 2", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 100, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 100, verbose = TRUE, classification = FALSE, mtry = 5, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = TRUE)

  testthat::expect_error(wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,
                                             CV_folds = 1, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features))
})


testthat::test_that("it returns an error if the importance method in ranger is not specified", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 100, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 100, verbose = TRUE, classification = FALSE, mtry = 5, min.node.size = 10, num.threads = 2, importance = 'none')

  params_features = list(keep_number_feat = NULL, union = TRUE)

  testthat::expect_error(wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,
                                             CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features))
})



testthat::test_that("it returns an error if params_features$union == TRUE AND length(method) == 1", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 100, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 100, verbose = TRUE, classification = FALSE, mtry = 5, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = TRUE)

  testthat::expect_error(wrapper_feat_select(X, y, params_glmnet = NULL, params_xgboost = NULL, params_ranger = params_ranger, xgb_sort = NULL,
                                             CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features))
})


testthat::test_that("it returns a list with non-empty data frames when union = F", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = F)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  testthat::expect_true(is.list(res) & all(unlist(lapply(res, function(x) sum(dim(x)))) > 0))
})


testthat::test_that("it returns a list with non-empty data frames when union = F, sorted by Gain in xgboost", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = F)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = "Gain",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  testthat::expect_true(is.list(res) & all(unlist(lapply(res, function(x) sum(dim(x)))) > 0))
})


testthat::test_that("it returns a list of two when union = T. The data frames in the first list and in the second list should be non-empty", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  testthat::expect_true(is.list(res) & all(unlist(lapply(res$all_feat, function(x) sum(dim(x)))) > 0) & sum(dim(res$union_feat)) > 0)
})


testthat::test_that("it returns a list of two when union = F. The data frames in the first list and in the second list should be non-empty", {

  X = iris[, -5]
  y = X[, 1]
  X = X[, -1]

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  testthat::expect_true(is.list(res) & all(unlist(lapply(res$all_feat, function(x) sum(dim(x)))) > 0) & sum(dim(res$union_feat)) > 0)
})


testthat::test_that("wrapper_feat_select works if the dependent.variable.name is in the parameters of ranger and all other methods are NULL", {

  X = iris
  X$Species = as.numeric(X$Species)
  y = X$Species

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = F)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(dependent.variable.name = "Species", probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = F)

  res = wrapper_feat_select(X, y, params_glmnet = NULL, params_xgboost = NULL, params_ranger = params_ranger, xgb_sort = NULL,

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = NULL, params_features = params_features)

  testthat::expect_true(is.list(res) & sum(dim(res$ranger)) > 0)
})


testthat::test_that("wrapper_feat_select returns an error if the dependent.variable.name is in the parameters of ranger and one or two of the other methods are not NULL", {

  X = iris
  X$Species = as.numeric(X$Species)
  y = X$Species

  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(dependent.variable.name = "Species", probability = FALSE, num.trees = 50, verbose = TRUE, classification = FALSE, mtry = 2, min.node.size = 10, num.threads = 2, importance = 'permutation')

  params_features = list(keep_number_feat = NULL, union = F)

  testthat::expect_error(wrapper_feat_select(X, y, params_glmnet = NULL, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,

                                             CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features))
})



testthat::test_that("it works in BINOMIAL classification. The labels should be in c(0,1), so that xgboost works. It should return a

                    list of two when union = T. The data frames in the first list and in the second list should be non-empty. Case binary classification", {

  y = iris[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'virginica'
  y = as.numeric(as.factor(y)) - 1
  X = iris[, -5]

  params_glmnet = list(alpha = 1, family = 'binomial', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:logistic", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  testthat::expect_true(is.list(res) & all(unlist(lapply(res$all_feat, function(x) sum(dim(x)))) > 0) & sum(dim(res$union_feat)) > 0)
})


testthat::test_that("it works in BINOMIAL classification. The labels should be in c(0,1), so that xgboost works. It should return a

                    list of two when union = T. The data frames in the first list and in the second list should be non-empty. Case binary classification, sorted by Cover in xgboost", {

  y = iris[, 5]
  y = as.character(y)
  y[y == 'setosa'] = 'virginica'
  y = as.numeric(as.factor(y)) - 1
  X = iris[, -5]

  params_glmnet = list(alpha = 1, family = 'binomial', nfolds = 3, parallel = TRUE)

  params_xgboost = list( params = list("objective" = "reg:logistic", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = "Cover",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  testthat::expect_true(is.list(res) & all(unlist(lapply(res$all_feat, function(x) sum(dim(x)))) > 0) & sum(dim(res$union_feat)) > 0)
})


testthat::test_that("it works in MULTICLASS classification. The labels should be in c(0 to Inf), so that xgboost works. It should return a

                    list of two when union = T. The data frames in the first and in the second list should be non-empty. Case multiclass classification", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_glmnet = list(alpha = 1, family = 'multinomial', nfolds = 3, parallel = F)

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, multiclass_xgboost, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  testthat::expect_true(is.list(res) & all(unlist(lapply(res$all_feat, function(x) sum(dim(x)))) > 0) & sum(dim(res$union_feat)) > 0)
})



testthat::test_that("it works in MULTICLASS classification. The labels should be in c(0 to Inf), so that xgboost works. It should return a

                    list of two when union = T. The data frames in the first and in the second list should be non-empty. Case multiclass classification, sorted by Frequence in xgboost", {

  y = iris[, 5]
  multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
  X = iris[, -5]

  params_glmnet = list(alpha = 1, family = 'multinomial', nfolds = 3, parallel = F)

  params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                         nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

  params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

  params_features = list(keep_number_feat = NULL, union = T)

  res = wrapper_feat_select(X, multiclass_xgboost, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = "Frequence",

                            CV_folds = 3, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)

  testthat::expect_true(is.list(res) & all(unlist(lapply(res$all_feat, function(x) sum(dim(x)))) > 0) & sum(dim(res$union_feat)) > 0)
})
