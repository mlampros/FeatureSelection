#' Feature selection
#'
#' This function uses three different methods (glmnet, xgboost, ranger) in order to select important features.
#'
#' @param X a sparse Matrix, a matrix or a data frame
#' @param y a vector of length representing the response variable
#' @param method one of 'glmnet-lasso', 'xgboost', 'ranger'
#' @param params_glmnet a list of parameters for the glmnet model
#' @param params_xgboost a list of parameters for the xgboost model
#' @param params_ranger a list of parameters for the ranger model
#' @param xgb_sort sort the xgboost features by "Gain", "Cover" or "Frequence" ( defaults to "Frequence")
#' @param CV_folds a number specifying the number of folds for cross validation
#' @param stratified_regr a boolean determining if the folds in regression should be stratified
#' @param scale_coefs_glmnet if TRUE, less important coefficients will be smaller than the more important ones (ranking/plotting by magnitude possible)
#' @param cores_glmnet an integer determining the number of cores to register in glmnet
#' @param verbose outputs info
#' @return a data frame with the most important features
#' @author Lampros Mouselimis
#' @details
#' This function returns the important features using one of the glmnet, xgboost or ranger algorithms. The glmnet algorithm can take either a sparse matrix, a matrix or a data frame
#' and returns a data frame with non zero coefficients. The xgboost algorithm can take either a sparse matrix, a matrix or a data frame and returns the importance of the features in form
#' of a data frame, furthermore it is possible to sort the features using one of the "Gain", "Cover" or "Frequence" methods. The ranger algorithm can take either a matrix or a data frame
#' and returns the important features using one of the 'impurity' or 'permutation' methods.
#' @export
#' @importFrom glmnet cv.glmnet
#' @importFrom dplyr group_by mutate_each summarize summarize_each funs n %>%
#' @importFrom doMC registerDoMC
#' @importFrom xgboost xgb.DMatrix xgb.train xgb.importance
#' @importFrom ranger ranger
#' @importFrom stats as.formula coefficients cor median na.omit predict
#' @importFrom Matrix Matrix
#' @examples
#'
#' # Data
#' # regression
#'
#' # data(iris)
#' # X = iris[, -5]
#' # y = X[, 1]
#' # X = X[, -1]
#'
#' # params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)
#' # res = feature_selection(X, y, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = 5)
#'
#' # binary classification
#'
#' # data(iris)
#' # y = iris[, 5]
#' # y = as.character(y)
#' # y[y == 'setosa'] = 'virginica'
#' # X = iris[, -5]
#'
#' # params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')
#'
#' # res = feature_selection(X, y, method = 'ranger', params_ranger = params_ranger, CV_folds = 5)
#'
#'
#' # multiclass classification
#'
#'
#' # data(iris)
#' # y = iris[, 5]
#' # multiclass_xgboost = ifelse(y == 'setosa', 0, ifelse(y == 'virginica', 1, 2))
#' # X = iris[, -5]
#'
#' # params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
#' #                        nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)
#'
#' # res = feature_selection(X, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 5)




feature_selection = function(X, y, method = NULL, params_glmnet = NULL, params_xgboost = NULL, params_ranger = NULL, xgb_sort = NULL, CV_folds = 5, stratified_regr = FALSE,

                             scale_coefs_glmnet = FALSE, cores_glmnet = NULL, verbose = FALSE) {


  if (is.null(method)) {

    stop("use method = .. to select one of the available methods : xgboost, glmnet-lasso, ranger")
  }

  if (CV_folds < 1) {

    stop("CV_folds should be >= 1")
  }

  if (method == 'glmnet-lasso' && CV_folds == 1) {

    suppressMessages(library(glmnet))
    suppressMessages(library(dplyr))


    if (verbose == TRUE) {
      cat('=====================================================================', '\n')
      cat('glmnet feature selection, starts...', '\n')
      cat('=====================================================================', '\n')
    }


    if (params_glmnet$family == 'binomial' || params_glmnet$family == 'multinomial') {

      y = as.factor(y)
    }

    # replace the NA-values of each column with the median

    isna = as.vector(Matrix::colSums(is.na(X)))

    if (sum(isna) > 0) {

      cat('\n'); cat('Missing values present in glmnet-lasso. They will be replaced with the median.', '\n'); cat('\n');

      func_replace_NAs = function(data, which_isna) {       # function to replace NAs

        for (i in which_isna) {

          tmp_median = median(data[, i], na.rm = T)

          data[which(is.na(data[, i])), i] = tmp_median
        }

        return(data)
      }

      X = func_replace_NAs(X, which(isna > 0))
    }

    Feature = colnames(X)

    if (is.data.frame(X)) {

      X <- as.matrix(X)}

    else if (is.matrix(X) || (class(X) == 'dgCMatrix')) {

      X = X}

    else {

      stop(simpleError("X must be either a data.frame or a (sparse-) matrix"))
    }

    # scale the explanatory variables as explained here : http://stats.stackexchange.com/questions/14853/variable-importance-from-glmnet
    # [ exclude from scaling those predictors that have less than 2 unique values, OTHERWISE error ]
    if (scale_coefs_glmnet == TRUE) X[, -which(as.vector(apply(X, 2, function(x) length(unique(x)))) < 2)] = scale(X[, -which(as.vector(apply(X, 2, function(x) length(unique(x)))) < 2)])

    params_glmnet[['x']] = X
    params_glmnet[['y']] = y
    if (scale_coefs_glmnet == TRUE) params_glmnet[['standardize']] = FALSE    # after using scale() ensure that the variables won't be standardized prior to fitting the model

    cv = do.call('cv.glmnet', params_glmnet)

    pr = predict(cv, type = 'coefficients', s = cv$lambda.min)

    if (is.factor(y)) {                    # in case of classification glmnet returns importance in form of a sparse matrix

      if (length(unique(y)) == 2) {        # in case of binary-classification it returns a single column

        df1 = as.matrix(pr)[-1, ]
        df1 = data.frame(features = names(df1), importance = as.vector(df1))
        if (scale_coefs_glmnet == TRUE) df1[, 2] = abs(df1[, 2])
        if (scale_coefs_glmnet == TRUE) df1 = df1[order(df1[, 2], decreasing = TRUE), ]}
      # in case of multiclass classification it returns a sparse matrix for each class separately
      if (length(unique(y)) > 2) {

        df1 = do.call(rbind, lapply(pr, function(x) as.matrix(x)[-1, ]))
        df1 = colMeans(df1)

        if (any(df1 == 0.0)) {

          df1 = df1[-which(df1 == 0L)]                      # remove zero-coefficient predictors
        }

        df1 = data.frame(features = names(df1), importance = as.vector(df1))
        if (scale_coefs_glmnet == TRUE) df1[, 2] = abs(df1[, 2])                          # after scaling, I take the absolute value in order to plot the important features [ this because many of them have high negative values -- meaning high impact on the response ]
        if (scale_coefs_glmnet == TRUE) df1 = df1[order(df1[, 2], decreasing = TRUE), ]}
    }

    else {

      df = data.frame(Feature, coefficients = pr[2:length(pr)], stringsAsFactors = FALSE)

      df1 = subset(df, df[,2] != 0)
      if (scale_coefs_glmnet == TRUE) df1[, 2] = abs(df1[, 2])
      if (scale_coefs_glmnet == TRUE) df1 = df1[order(df1[, 2], decreasing = TRUE), ]
    }

    return(df1)
  }


  else if (method == 'glmnet-lasso' && CV_folds > 1) {

    suppressMessages(library(glmnet))
    suppressMessages(library(dplyr))

    if (params_glmnet$parallel == TRUE && !is.null(cores_glmnet)) {

      suppressMessages(library(doMC))
      registerDoMC(cores = cores_glmnet)
    }

    if (verbose == TRUE) {
      cat('=====================================================================', '\n')
      cat('glmnet feature selection, starts...', '\n')
      cat('=====================================================================', '\n')
    }


    if (params_glmnet$family == 'binomial' || params_glmnet$family == 'multinomial') {

      y = as.factor(y)
    }

    if (is.factor(y)) {

      folds = class_folds(CV_folds, y, shuffle = TRUE)}

    else {

      folds = regr_folds(CV_folds, y, stratified = stratified_regr)
    }


    get_all_feat = list()

    for (i in 1:CV_folds) {

      if (verbose == TRUE) {
        cat('--------------------------------------------------------------------', '\n')
        cat('Fold ', i, '\n')
        cat('--------------------------------------------------------------------', '\n')
      }


      X_folds = X[unlist(folds[-i]), ]

      y_folds = y[unlist(folds[-i])]


      # replace the NA-values of each column with the median

      isna = as.vector(Matrix::colSums(is.na(X_folds)))

      if (sum(isna) > 0) {

        cat('\n'); cat('Missing values present in glmnet-lasso. They will be replaced with the median.', '\n'); cat('\n');

        func_replace_NAs = function(data, which_isna) {       # function to replace NAs

          for (i in which_isna) {

            tmp_median = median(data[, i], na.rm = T)

            data[which(is.na(data[, i])), i] = tmp_median
          }

          return(data)
        }

        X_folds = func_replace_NAs(X_folds, which(isna > 0))
      }

      Feature = colnames(X_folds)

      if (is.data.frame(X_folds)) {

        X_folds <- as.matrix(X_folds)}

      else if (is.matrix(X_folds) || (class(X) == 'dgCMatrix')) {

        X_folds = X_folds}

      else {

        stop(simpleError("X must be either a data.frame or a (sparse-) matrix"))
      }

      # scale the explanatory variables as explained here : http://stats.stackexchange.com/questions/14853/variable-importance-from-glmnet
      # [ exclude from scaling those predictors that have less than 2 unique values, OTHERWISE error ]
      if (scale_coefs_glmnet == TRUE) X_folds[, -which(as.vector(apply(X_folds, 2, function(x) length(unique(x)))) < 2)] = scale(X_folds[, -which(as.vector(apply(X_folds, 2, function(x) length(unique(x)))) < 2)])

      params_glmnet[['x']] = X_folds
      params_glmnet[['y']] = y_folds
      if (scale_coefs_glmnet == TRUE) params_glmnet[['standardize']] = FALSE    # after using scale() ensure that the variables won't be standardized prior to fitting the model

      cv = do.call('cv.glmnet', params_glmnet)

      pr = predict(cv, type = 'coefficients', s = cv$lambda.min)

      if (is.factor(y)) {                                            # in case of classification glmnet returns importance in form of a sparse matrix

        if (length(unique(y)) == 2) {                                # in case of binary-classification it returns a single column

          get_all_feat[[i]] = as.matrix(pr)[-1, ]}
        # in case of multiclass classification it returns a sparse matrix for each class separately
        if (length(unique(y)) > 2) {

          get_all_feat[[i]] = do.call(rbind, lapply(pr, function(x) as.matrix(x)[-1, ]))

          gc()}
      }

      else {

        df = data.frame(Feature, coefficients = pr[2:length(pr)], stringsAsFactors = FALSE)

        df1 = subset(df, df[,2] != 0)

        get_all_feat[[i]] = df1

        gc()}
    }

    if (is.factor(y)) {

      if (length(unique(y)) == 2) {

        tbl_x = colMeans(data.frame(do.call(rbind, get_all_feat)))
        tbl_x = data.frame(features = names(tbl_x), importance = as.vector(tbl_x))
        if (scale_coefs_glmnet == TRUE) tbl_x[, 2] = abs(tbl_x[, 2])
        if (scale_coefs_glmnet == TRUE) tbl_x = tbl_x[order(tbl_x[, 2], decreasing = TRUE), ]}

      if (length(unique(y)) > 2) {

        df1 = data.frame(add_probs_dfs(get_all_feat), row.names = rownames(get_all_feat[[1]]))
        df1 = colMeans(df1)

        if (any(df1 == 0.0)) {

          df1 = df1[-which(df1 == 0L)]                      # remove zero-coefficient predictors
        }

        tbl_x = data.frame(features = names(df1), importance = as.vector(df1))
        if (scale_coefs_glmnet == TRUE) tbl_x[, 2] = abs(tbl_x[, 2])                          # after scaling, I take the absolute value in order to plot the important features [ this because many of them have high negative values -- meaning high impact on the response ]
        if (scale_coefs_glmnet == TRUE) tbl_x = tbl_x[order(tbl_x[, 2], decreasing = TRUE), ]}
    }

    else {

      all_feat = data.frame(do.call('rbind', get_all_feat))

      tbl_x = data.frame(all_feat %>% group_by(Feature) %>% summarize(coefficients = mean(coefficients, na.rm = TRUE), Frequency = n()))
      if (scale_coefs_glmnet == TRUE) tbl_x[, 2] = abs(tbl_x[, 2])
      tbl_x = tbl_x[order(tbl_x$Frequency, tbl_x$coefficients, decreasing = TRUE),]          # the data.frame in 'glmnet-lasso' is sorted by Frequency (default)
    }

    return(tbl_x)
  }


  else if (method == 'xgboost' && CV_folds == 1) {

    suppressMessages(library(xgboost))
    if (verbose == TRUE) {
      cat('=====================================================================', '\n')
      cat('xgboost feature selection, starts...', '\n')
      cat('=====================================================================', '\n')
    }

    if (is.data.frame(X)) {

      dtrain <- xgb.DMatrix(data = as.matrix(X), label = y, missing = NA)}

    else if (is.matrix(X) || (class(X) == 'dgCMatrix')) {

      dtrain <- xgb.DMatrix(data = X, label = y, missing = NA)}

    else {

      stop(simpleError("X must be either a data.frame or a (sparse-) matrix"))
    }

    params_xgboost[['watchlist']] = list(train = dtrain)
    params_xgboost[['data']] = dtrain

    bst = suppressWarnings(do.call('xgb.train', params_xgboost))

    tbl1 <- data.frame(xgb.importance(colnames(X), model = bst))

    if (is.null(xgb_sort) || (xgb_sort == 'Frequence')) {

      tbl1 = tbl1[order(tbl1$Frequence, decreasing = TRUE),]}

    else if (xgb_sort == 'Gain') {

      tbl1 = tbl1[order(tbl1$Gain, decreasing = TRUE),]}

    else if (xgb_sort == 'Cover') {

      tbl1 = tbl1[order(tbl1$Cover, decreasing = TRUE),]}

    return(tbl1)

  }

  else if (method == 'xgboost' && CV_folds > 1) {

    suppressMessages(library(xgboost))
    suppressMessages(library(dplyr))

    if (verbose == TRUE) {
      cat('=====================================================================', '\n')
      cat('xgboost feature selection, starts...', '\n')
      cat('=====================================================================', '\n')
    }

    if (length(unique(y)) == 2 || ("num_class" %in% names(params_xgboost$params))) {

      folds = class_folds(CV_folds, as.factor(y), shuffle = TRUE)}

    else {

      folds = regr_folds(CV_folds, y, stratified = stratified_regr)
    }

    get_all_feat = list()

    for (i in 1:CV_folds) {

      if (verbose == TRUE) {
        cat('--------------------------------------------------------------------', '\n')
        cat('Fold ', i, '\n')
        cat('--------------------------------------------------------------------', '\n')
      }

      X_folds = X[unlist(folds[-i]), ]

      y_folds = y[unlist(folds[-i])]

      if (is.data.frame(X_folds)) {

        dtrain <- xgb.DMatrix(data = as.matrix(X_folds), label = y_folds, missing = NA)}

      else if (is.matrix(X_folds) || (class(X_folds) == 'dgCMatrix')) {

        dtrain <- xgb.DMatrix(data = X_folds, label = y_folds, missing = NA)}

      else {

        stop(simpleError("X must be either a data.frame or a (sparse-) matrix"))
      }

      params_xgboost[['watchlist']] = list(train = dtrain)
      params_xgboost[['data']] = dtrain

      bst = suppressWarnings(do.call('xgb.train', params_xgboost))

      get_all_feat[[i]] <- data.frame(xgb.importance(colnames(X_folds), model = bst))

      gc()
    }

    tbl_x = data.frame(do.call('rbind', get_all_feat))

    tbl1 = data.frame(tbl_x %>% group_by(Feature) %>% summarize_each(funs(mean(., na.rm = TRUE))))

    if (is.null(xgb_sort) || (xgb_sort == 'Frequence')) {

      tbl1 = tbl1[order(tbl1$Frequence, decreasing = TRUE),]}

    else if (xgb_sort == 'Gain') {

      tbl1 = tbl1[order(tbl1$Gain, decreasing = TRUE),]}

    else if (xgb_sort == 'Cover') {

      tbl1 = tbl1[order(tbl1$Cover, decreasing = TRUE),]}

    return(tbl1)

  }

  else if (method == 'ranger' && CV_folds == 1) {

    suppressMessages(library(ranger))

    if (!(is.matrix(X) || is.data.frame(X))) {

      stop(simpleError("X must be either a data.frame or a matrix"))
    }

    if (params_ranger$classification == TRUE) {

      y = as.factor(y)
    }

    if (verbose == TRUE) {
      cat('=====================================================================', '\n')
      cat('ranger feature selection, starts...', '\n')
      cat('=====================================================================', '\n')
    }

    if (!"dependent.variable.name" %in% names(params_ranger)) {

      form = as.formula(paste0(paste0('y ~ '), paste(colnames(X), collapse = '+')))

      params_ranger[['formula']] = form

      dat = data.frame(y = y, X)}

    else {

      dat = X
    }

    params_ranger[['data']] = dat

    fit = do.call('ranger', params_ranger)

    tbl_x = data.frame(names(fit$variable.importance), as.vector(fit$variable.importance))
    colnames(tbl_x) = c('Feature', params_ranger$importance)

    tbl1 = tbl_x[order(tbl_x[, 2], decreasing = TRUE), ]

    return(tbl1)
  }

  else if (method == 'ranger' && CV_folds > 1) {

    suppressMessages(library(ranger))
    suppressMessages(library(dplyr))

    if (verbose == TRUE) {
      cat('=====================================================================', '\n')
      cat('ranger feature selection, starts...', '\n')
      cat('=====================================================================', '\n')
    }


    if (params_ranger$classification == TRUE) {

      y = as.factor(y)
    }

    if (is.factor(y)) {

      folds = class_folds(CV_folds, y, shuffle = TRUE)}

    else {

      folds = regr_folds(CV_folds, y, stratified = stratified_regr)
    }

    get_all_feat = list()

    for (i in 1:CV_folds) {

      if (!(is.matrix(X) || is.data.frame(X))) {

        stop(simpleError("X must be either a data.frame or a matrix"))
      }

      if (verbose == TRUE) {
        cat('--------------------------------------------------------------------', '\n')
        cat('Fold ', i, '\n')
        cat('--------------------------------------------------------------------', '\n')
      }

      X_folds = X[unlist(folds[-i]), ]

      y_folds = y[unlist(folds[-i])]

      if (!"dependent.variable.name" %in% names(params_ranger)) {

        form = as.formula(paste0(paste0('y ~ '), paste(colnames(X_folds), collapse = '+')))

        params_ranger[['formula']] = form

        dat = data.frame(y = y_folds, X_folds)}

      else {

        dat = X_folds
      }

      params_ranger[['data']] = dat

      fit = do.call('ranger', params_ranger)

      tbl_x = data.frame(names(fit$variable.importance), as.vector(fit$variable.importance))
      colnames(tbl_x) = c('Feature', params_ranger$importance)

      get_all_feat[[i]] <- tbl_x

      gc()
    }

    tbl_x = data.frame(do.call('rbind', get_all_feat))

    tbl1 = data.frame(tbl_x %>% group_by(Feature) %>% summarize_each(funs(mean(., na.rm = TRUE))))

    tbl1 = tbl1[order(tbl1[, 2], decreasing = TRUE), ]

    return(tbl1)
  }
}
