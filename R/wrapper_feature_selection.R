#' wrapper of all three methods
#'
#' This function is a wrapper for the feature_selection function
#'
#' @param X a sparse Matrix, a matrix or a data frame
#' @param y a vector of length representing the response variable
#' @param params_glmnet a list of parameters for the glmnet model
#' @param params_xgboost a list of parameters for the xgboost model
#' @param params_ranger a list of parameters for the ranger model
#' @param xgb_sort sort the xgboost features by "Gain", "Cover" or "Frequence" ( defaults to "Frequence")
#' @param CV_folds a number specifying the number of folds for cross validation
#' @param stratified_regr a boolean determining if the folds in regression should be stratified
#' @param scale_coefs_glmnet if TRUE, less important coefficients will be smaller than the more important ones (ranking/plotting by magnitude possible)
#' @param cores_glmnet an integer determining the number of cores to register in glmnet
#' @param params_features is a list of parameters for the wrapper function
#' @param verbose outputs info
#' @return a list containing the important features of each method. If union in the params_feature list is enabled, then it also returns the average importance of all methods.
#' @details
#' This function returns the importance of the methods specified and if union in the params_feature list is TRUE then it also returns the average importance of all methods.
#' Furthermore the user can limit the number of features using the keep_number_feat parameter of the params_feature list.
#' @export
#' @importFrom dplyr group_by mutate_each summarize summarize_each funs n %>%
#' @examples
#'
#' # regression
#'
#' # data(iris)
#' # X = iris[, -5]
#' # y = X[, 1]
#' # X = X[, -1]
#'
#' # params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)
#'
#' # params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.01, "subsample" = 0.65, "max_depth" = 5, "colsample_bytree" = 0.65, "nthread" = 2),
#' #                        nrounds = 100, print.every.n = 50, verbose = 0, maximize = FALSE)
#'
#' # params_ranger = list(probability = FALSE, num.trees = 100, verbose = TRUE, classification = FALSE, mtry = 3, min.node.size = 10, num.threads = 2, importance = 'permutation')
#'
#' # params_features = list(keep_number_feat = NULL, union = TRUE)
#'
#' # feat = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,
#' #                            CV_folds = 10, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)




wrapper_feat_select = function(X, y, params_glmnet = NULL, params_xgboost = NULL, params_ranger = NULL, xgb_sort = NULL, CV_folds = 5,

                               stratified_regr = FALSE, scale_coefs_glmnet = FALSE, cores_glmnet = NULL, params_features = NULL, verbose = FALSE) {

  method = c('glmnet-lasso', 'xgboost', 'ranger')
  met = list(params_glmnet, params_xgboost, params_ranger)
  met1 = unlist(lapply(met, function(x) !is.null(x)))

  if (sum(met1) == 0) stop('at least one of the methods should be non-null')

  method = method[met1]

  if (!is.null(params_ranger) && ("dependent.variable.name" %in% names(params_ranger)) && sum(met1) > 1) stop("ranger can not be used simoultaneously with the other methods if 'dependent.variable.name' is in the params_ranger")

  if (CV_folds < 2) {

    stop(simpleError("CV_folds should be > 1"))
  }

  if (('ranger' %in% method) && (params_ranger$importance == 'none')) {

    stop(simpleError("enable importance in ranger using one of the arguments 'impurity' or 'permutation'"))
  }

  if (params_features$union == TRUE && length(method) == 1) {

    stop(simpleError("run union = TRUE only in case of more than one method"))
  }

  out_meth = list()

  for (meth in method) {

    out_meth[[meth]] = feature_selection(X, y, method = meth, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = xgb_sort,

                                         CV_folds = CV_folds, stratified_regr = stratified_regr, scale_coefs_glmnet = scale_coefs_glmnet, cores_glmnet = cores_glmnet, verbose = verbose)
  }


  if (is.null(params_features$keep_number_feat)) {

    out = out_meth}

  else {

    out = lapply(out_meth, function(x) x[1:params_features$keep_number_feat, ])
  }


  if (params_features$union == TRUE) {

    out_union = list()

    suppressMessages(library(dplyr))

    for (k in method) {

      if (k == 'glmnet-lasso' && (!params_glmnet$family %in% c('binomial', 'multinomial'))) {

        un_glmnet = out_meth[[k]][, -2]                          # exclude coefficients column
        un_glmnet[, 2] = normalized(un_glmnet[, 2])              # normalize the frequency so that is in same scale with the other algorithms
        colnames(un_glmnet) = c('features', 'importance')
        un_glmnet[, 1] = as.character(un_glmnet[, 1])
        out_union[[k]] = un_glmnet}

      else if (k == 'glmnet-lasso' && params_glmnet$family %in% c('binomial', 'multinomial')) {

        un_glmnet = out_meth[[k]]
        un_glmnet[, 2] = normalized(un_glmnet[, 2])
        colnames(un_glmnet) = c('features', 'importance')
        un_glmnet[, 1] = as.character(un_glmnet[, 1])
        out_union[[k]] = un_glmnet}

      else if (k == 'xgboost') {

        if (is.null(xgb_sort)) {

          un_xgboost = out_meth[[k]][, c(1, 4)]
          un_xgboost[, 2] = normalized(un_xgboost[, 2])
          colnames(un_xgboost) = c('features', 'importance')
          un_xgboost[, 1] = as.character(un_xgboost[, 1])
          out_union[[k]] = un_xgboost}

        else {

          un_xgboost = data.frame(features = out_meth[[k]][, 1], importance = out_meth[[k]][, xgb_sort])
          un_xgboost[, 2] = normalized(un_xgboost[, 2])
          un_xgboost[, 1] = as.character(un_xgboost[, 1])
          out_union[[k]] = un_xgboost}
      }

      else if (k == 'ranger') {

        un_ranger = out_meth[[k]]
        un_ranger[, 2] = normalized(un_ranger[, 2])
        colnames(un_ranger) = c('features', 'importance')
        un_ranger[, 1] = as.character(un_ranger[, 1])
        out_union[[k]] = un_ranger}
    }

    modify_lst = lapply(out_union, function(x) data.frame(feature = x$features, rank = normalized(length(x$features):1)))

    modify_lst1 = data.frame(do.call(rbind, modify_lst))

    tbl_x = data.frame(modify_lst1 %>% group_by(feature) %>% summarize(importance = sum(rank, na.rm = TRUE), Frequency = n()))

    tbl1 = tbl_x[order(tbl_x$importance, decreasing = TRUE), ]

    tbl1$importance = normalized(tbl1$importance)

    return(list(all_feat = out, union_feat = tbl1))}

  else {

    return(all_feat = out)
  }
}
