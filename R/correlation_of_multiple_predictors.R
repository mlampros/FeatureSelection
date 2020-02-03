#' secondary function ( used in the func_correlation )
#'
#' this is a secondary function that is used in the func_correlation
#'
#' @keywords internal
#' @importFrom stats na.omit

# use this function if more than one predictor output
second_func_cor = function(dat_frame) {

  lst = list()

  for (i in 1:dim(dat_frame)[2]) {

    coln = colnames(dat_frame)
    temp_df = data.frame(dat_frame[, i], row.names = rownames(dat_frame))
    colnames(temp_df) = coln[i]
    temp_df[temp_df == 0.0] = NA
    lst[[i]] = stats::na.omit(temp_df)
  }

  return(lst)
}


#' function to remove duplicated pairs of predictors ( used in the func_correlation )
#'
#' this is a secondary function that is used in the func_correlation
#'
#' @keywords internal


remove_duplic_func = function(sublist) {

  vec_col = vec_row = vec_prob = rep(NA, dim(sublist)[1])

  for (j in 1:dim(sublist)[1]) {

    vec_col[j] = colnames(sublist)
    vec_row[j] = rownames(sublist)[j]
    vec_prob[j] = sublist[j, 1]
  }

  data.frame(predictor1 = vec_row, predictor2 = vec_col, prob = vec_prob)
}


#' find correlated variables
#'
#' This function takes a data frame or a matrix and returns either a data frame or a list of data frames with correlated variables
#'
#' @param data either a data frame or a matrix
#' @param target either a string (name of the predictor/response in the data set) or a vector of strings (predictor/response names of the data set)
#' @param correlation_thresh a float indicating the correlation threshold
#' @param use_obs one of "everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"
#' @param correlation_method one of "pearson", "kendall", "spearman"
#' @return either a data frame or a list of data frames
#' @details
#' This function takes a data frame or a matrix and returns the correlated variables of the data
#' @export
#' @importFrom stats cor na.omit


func_correlation = function(data, target = NULL, correlation_thresh = NULL, use_obs = NULL, correlation_method = NULL) {

  if (!inherits(data, c("data.frame", "matrix"))) stop('the data should be either a data frame or a matrix')
  if (sum(unlist(lapply(1:dim(data)[2], function(x) is.factor(data[, x]) || is.character(data[, x]))) > 0)) stop(simpleError('data must be numeric'))
  if (is.null(correlation_method)) stop(simpleError('use one of "pearson", "kendall", "spearman" as correlation method'))
  if (is.null(use_obs)) stop(simpleError('use one of "everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs" as use_obs'))
  if (is.null(correlation_thresh) || correlation_thresh <= 0.0 || correlation_thresh >= 1.0) stop(simpleError('the correlation_thresh should be greater than 0.0 and less than 1.0'))

  df = data.frame(stats::cor(data, use = use_obs, method = correlation_method))

  if (length(target) > 1) {                 # in case that target is a vector with multiple predictors, it returns a matrix with those predictors above the correlation_thresh

    df[upper.tri(df)] = diag(df) = 0.0
    df[df < correlation_thresh] = 0.0
    df = df[-as.vector(which(rowSums(df) == 0.0)), -as.vector(which(colSums(df) == 0.0))]               # negate to remove all sparse columns and rows

    if (length(which(!is.na(match(colnames(df), target)))) == 1) {                                      # exception if one of the column-names or row-names do not appear in the end-df

      filt = data.frame(row.names = rownames(df), df[, which(!is.na(match(colnames(df), target)))])
      colnames(filt) = colnames(df)[which(!is.na(match(colnames(df), target)))]
      filt[filt == 0.0] = NA
      filt = stats::na.omit(filt)
    }

    else {

      filt = df[, which(!is.na(match(colnames(df), target)))]
    }
  }

  else if (is.null(target)) {

    df[upper.tri(df)] = diag(df) = 0.0                                                                  # return all predictors in case that is.null(target)
    df[df < correlation_thresh] = 0.0
    filt = df[-as.vector(which(rowSums(df) == 0.0)), -as.vector(which(colSums(df) == 0.0))]             # negate to remove all sparse columns and rows
  }

  else {

    df_names = data.frame(df[, target], row.names = rownames(df))            # in case where target is a single string, I get a single column data.frame out
    colnames(df_names) = target
    df_names[rownames(df_names) == target, ] = NA                            # remove the 'target' column-name
    df_names = stats::na.omit(df_names)
    filt = subset(df_names, df_names[, target] >= correlation_thresh)
    filt = data.frame(features = rownames(filt), filt[, 1])
    filt = filt[order(filt[, 2], decreasing = TRUE), ]
    filt = data.frame(filt[, 2], row.names = filt[, 1])
    colnames(filt) = c(target)
  }

  if (dim(filt)[2] > 1) {

    out = second_func_cor(filt)
    out = out[unlist(lapply(out, function(x) dim(x)[1] != 0))]
    return(list(out_list = out, out_df = do.call(rbind, lapply(out, function(x) remove_duplic_func(x)))))}

  else {

    return(filt)
  }
}


