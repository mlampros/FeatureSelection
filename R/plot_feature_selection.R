#' plot important features
#'
#' This function takes the result of the feature_selection function or the wrapper_feat_select function and returns a barplot with the important features
#'
#' @param obj either a data frame or a list from the functions : feature_selection, wrapper_feat_select
#' @param params_barplot a list of parameters needed for plotting the important features
#' @param xgb_sort sort the xgboost features by "Gain", "Cover" or "Frequence" ( defaults to "Frequence")
#' @return a barplot with the important features of each method
#' @details
#' This function takes a data frame (from the feature_selection function) or a list (from the wrapper_feat_select function) and returns a barplot of the important features.
#' If union is TRUE in the params_barplot vector it returns also the average importance of all methods
#' @export
#' @importFrom graphics barplot par
#' @importFrom grDevices dev.cur dev.off
#' @examples
#'
#' # data(iris)
#' # X = iris[, -5]
#' # y = X[, 1]
#' # X = X[, -1]
#'
#'
#' # plot for the wrapper function
#'
#' # feat = wrapper_feat_select(X, y, params_glmnet = params_glmnet, params_xgboost = params_xgboost, params_ranger = params_ranger, xgb_sort = NULL,
#' #                            CV_folds = 10, stratified_regr = FALSE, cores_glmnet = 2, params_features = params_features)
#'
#' # params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 1.0)
#'
#' # barplot_feat_select(feat, params_barplot, xgb_sort = NULL)
#'
#'
#'
#' # plot for the feature_selection function
#'
#' # res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = 5)
#'
#'
#' # params_barplot = list(keep_features = 5, horiz = TRUE, cex.names = 0.8)
#'
#' # barplot_feat_select(res, params_barplot, xgb_sort = NULL)



barplot_feat_select = function(obj, params_barplot, xgb_sort = NULL) {


  if (is.null(params_barplot$keep_features)) {

    stop(simpleError("specify a maximum number of features to be plotted using the parameter keep_features in the params_barplot list"))
  }

  if (is.data.frame(obj)) {

    if (dev.cur() != 1) {

      dev.off()                          # reset par()
    }

    par(las = 2)                         # make label text perpendicular to axis
    par(mar = c(5, 8, 4, 2))             # increase y-axis margin.

    if (dim(obj)[2] == 3) {                                  # glmnet-lasso , CV_folds > 1

      barplot(obj[params_barplot$keep_features:1, 3], main = "glmnet-importance", horiz = params_barplot$horiz, names.arg = obj[params_barplot$keep_features:1, 1],

              cex.names = params_barplot$cex.names)}

    else if (dim(obj)[2] == 4) {                              # xgboost

      if (is.null(xgb_sort) || xgb_sort == 'Frequence') {

        obj = obj[order(obj[, 4], decreasing = TRUE), ]
        obj = obj[, c(1, 4)]}

      else if (xgb_sort == 'Cover') {

        obj = obj[order(obj[, 3], decreasing = TRUE), ]
        obj = obj[, c(1, 3)]}

      else if (xgb_sort == 'Gain') {

        obj = obj[order(obj[, 2], decreasing = TRUE), ]
        obj = obj[, c(1, 2)]}

      else {

        stop(simpleError("not a valid method for xgb_sort"))
      }

      barplot(obj[params_barplot$keep_features:1, 2], main = "xgboost-importance", horiz = params_barplot$horiz, names.arg = obj[params_barplot$keep_features:1, 1],

              cex.names = params_barplot$cex.names)}

    else if (dim(obj)[2] == 2 && (colnames(obj)[2] %in% c('impurity', 'permutation'))) {

      barplot(obj[params_barplot$keep_features:1, 2], main = "ranger-importance", horiz = params_barplot$horiz, names.arg = obj[params_barplot$keep_features:1, 1],

              cex.names = params_barplot$cex.names)}

    else if (dim(obj)[2] == 2) {                                  # glmnet-lasso, CV_folds = 1

      barplot(obj[params_barplot$keep_features:1, 2], main = "glmnet-importance", horiz = params_barplot$horiz, names.arg = obj[params_barplot$keep_features:1, 1],

              cex.names = params_barplot$cex.names)
    }
  }

  else {

    if (length(names(obj)) < 3 && ("all_feat" %in% names(obj))) {          # union = TRUE

      if (dev.cur() != 1) {

        dev.off()                          # reset par()
      }

      par(las = 2)                         # make label text perpendicular to axis
      par(mar = c(5, 8, 4, 2))             # increase y-axis margin.

      par(mfrow = c(1, length(names(obj$all_feat)) + 1))

      if (is.null(xgb_sort)) xgb_sort = 'Frequence'

      for (i in names(obj$all_feat)) {

        if (i == 'glmnet-lasso') {

          obj_g = obj$all_feat[['glmnet-lasso']]

          barplot(obj_g[params_barplot$keep_features:1, 3], main = "glmnet-importance", horiz = params_barplot$horiz, names.arg = obj_g[params_barplot$keep_features:1, 1],

                  cex.names = params_barplot$cex.names)}

        else if (i == 'xgboost') {

          obj_x = obj$all_feat[['xgboost']]
          obj_x = obj_x[order(obj_x[, xgb_sort], decreasing = TRUE), ]
          tmp_xgb = obj_x[params_barplot$keep_features:1, xgb_sort]

          barplot(tmp_xgb, main = "xgboost-importance", horiz = params_barplot$horiz, names.arg = obj_x[params_barplot$keep_features:1, 1],

                  cex.names = params_barplot$cex.names)}

        else if (i == 'ranger') {

          obj_r = obj$all_feat[['ranger']]

          barplot(obj_r[params_barplot$keep_features:1, 2], main = "ranger-importance", horiz = params_barplot$horiz, names.arg = obj_r[params_barplot$keep_features:1, 1],

                  cex.names = params_barplot$cex.names)
        }
      }

      barplot(obj$union_feat[params_barplot$keep_features:1, 2], main = "union-importance",

              horiz = params_barplot$horiz, names.arg = obj$union_feat[params_barplot$keep_features:1, 1], cex.names = params_barplot$cex.names)
    }

    else if (length(names(obj)) < 3 && (sum(c("glmnet-lasso", "xgboost", "ranger") %in% names(obj)) > 0)){              # union = FALSE

      if (dev.cur() != 1) {

        dev.off()                          # reset par()
      }

      par(las = 2)                         # make label text perpendicular to axis
      par(mar = c(5, 8, 4, 2))             # increase y-axis margin.

      par(mfrow = c(1, length(names(obj))))

      if (is.null(xgb_sort)) xgb_sort = 'Frequence'

      for (i in names(obj)) {

        if (i == 'glmnet-lasso') {

          obj_g = obj[['glmnet-lasso']]

          barplot(obj_g[params_barplot$keep_features:1, 3], main = "glmnet-importance", horiz = params_barplot$horiz, names.arg = obj_g[params_barplot$keep_features:1, 1],

                  cex.names = params_barplot$cex.names)}

        else if (i == 'xgboost') {

          obj_x = obj[['xgboost']]
          obj_x = obj_x[order(obj_x[, xgb_sort], decreasing = TRUE), ]
          tmp_xgb = obj_x[params_barplot$keep_features:1, xgb_sort]

          barplot(tmp_xgb, main = "xgboost-importance", horiz = params_barplot$horiz, names.arg = obj_x[params_barplot$keep_features:1, 1],

                  cex.names = params_barplot$cex.names)}

        else if (i == 'ranger') {

          obj_r = obj[['ranger']]

          barplot(obj_r[params_barplot$keep_features:1, 2], main = "ranger-importance", horiz = params_barplot$horiz, names.arg = obj_r[params_barplot$keep_features:1, 1],

                  cex.names = params_barplot$cex.names)
        }

        else {

          stop('invalid method')
        }
      }
    }
  }
}
