#' shuffle data
#'
#' this function shuffles the items of a vector
#'
#' @param vec is a vector of indices
#' @param times is a number
#' @return shuffled indices of a vector
#' @export


func_shuffle = function(vec, times = 10) {

  for (i in 1:times) {

    out = sample(vec, length(vec))
  }
  out
}


#' addition of probability-data-frames
#'
#' this function takes a number of probability data frames and returns their average (mainly used in multi-class classification).
#'
#' @param PREDS_LST is a list of data frames
#' @return average of a number of data frames
#' @export


add_probs_dfs = function(PREDS_LST) {

  if (class(PREDS_LST) != "list") stop("PREDS_LST must be a list")

  r = all(unlist(lapply(PREDS_LST, nrow)) == unlist(lapply(PREDS_LST, nrow))[1])
  c = all(unlist(lapply(PREDS_LST, ncol)) == unlist(lapply(PREDS_LST, ncol))[1])

  if (!all(c(r,c))) stop("the dimensions of the included data.frames or matrices differ")

  init_df = data.frame(matrix(rep(0, dim(PREDS_LST[[1]])[1]*dim(PREDS_LST[[1]])[2]), nrow = dim(PREDS_LST[[1]])[1], ncol = dim(PREDS_LST[[1]])[2]))

  for (i in 1:length(PREDS_LST)) {

    init_df = init_df + PREDS_LST[[i]]
  }

  init_df = init_df/length(PREDS_LST)
  colnames(init_df) = colnames(PREDS_LST[[1]])

  return(as.matrix(init_df))
}


#' normalize data
#'
#' this function normalizes the feature importance of the algorithms, so that data between algorithms is in the same scale
#'
#' @param x is a numeric vector
#' @return normalized data in form of a vector
#' @export


normalized = function(x) {

  out = (x - min(x))/(max(x) - min(x))

  out
}


