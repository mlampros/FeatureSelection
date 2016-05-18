#' stratified folds (in classification)
#'
#' this function creates stratified folds in binary and multiclass classification
#'
#' @param folds is an integer specifying the number of folds
#' @param RESP is the response variable
#' @param shuffle is a boolean specifying if the vector of indices should be shuffled or not
#' @return a list of indices
#' @export
#' @importFrom utils combn
#' @examples
#'
#' # data(iris)
#' # y = as.factor(iris[, 5])
#'
#' # folds = class_folds(10, y, shuffle = TRUE)


class_folds = function(folds, RESP, shuffle = FALSE) {

  if (!is.factor(RESP)) {

    stop(simpleError("RESP must be a factor"))
  }

  clas = lapply(unique(RESP), function(x) which(RESP == x))

  len = lapply(clas, function(x) length(x))

  samp_vec = rep(1/folds, folds)

  prop = lapply(len, function(y) sapply(1:length(samp_vec), function(x) round(y * samp_vec[x])))

  repl = unlist(lapply(prop, function(x) sapply(1:length(x), function(y) rep(paste0('fold_', y), x[y]))))

  spl = suppressWarnings(split(1:length(RESP), repl))

  sort_names = paste0('fold_', 1:folds)

  spl = spl[sort_names]

  if (length(table(unlist(lapply(spl, function(x) length(x))))) > 1) {

    warning('the folds are not equally split')            # the warning appears when I divide the unique labels to the number of folds and instead of an integer I get a float
  }

  if (shuffle == TRUE) {

    spl = lapply(spl, function(x) func_shuffle(x))           # the indices of the unique levels will be shuffled
  }

  ind = t(combn(1:folds, 2))

  ind1 = apply(ind, 1, function(x) length(intersect(spl[x[1]], spl[x[2]])))

  if (sum(ind1) > 0) {

    stop(simpleError("there is an intersection between the resulted indexes of the folds"))

  }

  if (length(unlist(spl)) != length(RESP)) {

    stop(simpleError("the number of items in the folds are not equal with the response items"))
  }

  spl
}




#' create folds (in regression)
#'
#' this function creates both stratified and non-stratified folds in regression
#'
#' @param folds is an integer specifying the number of folds
#' @param RESP is the response variable
#' @param stratified is a boolean specifying if the folds should be stratfied
#' @return a list of indices
#' @export
#' @examples
#'
#' # data(iris)
#' # y = X[, 1]
#'
#' # folds = regr_folds(5, y, stratified = FALSE)




regr_folds = function(folds, RESP, stratified = FALSE) {

  if (is.factor(RESP)) {

    stop(simpleError("this function is meant for regression for classification use the 'class_folds' function"))
  }

  samp_vec = rep(1/folds, folds)

  sort_names = paste0('fold_', 1:folds)


  if (stratified == TRUE) {

    stratif = cut(RESP, breaks = folds)

    clas = lapply(unique(stratif), function(x) which(stratif == x))

    len = lapply(clas, function(x) length(x))

    prop = lapply(len, function(y) sapply(1:length(samp_vec), function(x) round(y * samp_vec[x])))

    repl = unlist(lapply(prop, function(x) sapply(1:length(x), function(y) rep(paste0('fold_', y), x[y]))))

    spl = suppressWarnings(split(1:length(RESP), repl))}

  else {

    prop = lapply(length(RESP), function(y) sapply(1:length(samp_vec), function(x) round(y * samp_vec[x])))

    repl = func_shuffle(unlist(lapply(prop, function(x) sapply(1:length(x), function(y) rep(paste0('fold_', y), x[y])))))

    spl = suppressWarnings(split(1:length(RESP), repl))
  }

  spl = spl[sort_names]

  if (length(table(unlist(lapply(spl, function(x) length(x))))) > 1) {

    warning('the folds are not equally split')            # the warning appears when I divide the unique labels to the number of folds and instead of an ingeger I get a float
  }

  if (length(unlist(spl)) != length(RESP)) {

    stop(simpleError("the length of the splits are not equal with the length of the response"))
  }

  spl
}



#' partition of data (train-test-split)
#'
#' @param y is a numeric vector (response variable)
#' @param TrainRatio is the percentage of train-data after the partition
#' @param regression is a boolean (TRUE, FALSE) indicating if it's a regression or classification task
#' @param shuffle is a boolean (TRUE, FALSE) indicating if the data should be shuffled or not (by default 5 times)
#' @return a list of indices (train-test)
#' @export
#' @examples
#'
#' # data(iris)
#' # y = X[, 1]
#'
#' # split = DataSplit(y, TrainRatio = 0.75, regression = FALSE, shuffle = FALSE)


DataSplit = function(y, TrainRatio = 0.75, regression = TRUE, shuffle = FALSE) {

  if (TrainRatio >= 1.0 || TrainRatio <= 0.0) stop('TrainRation should be a float number greater than 0 and less than 1.0')

  if (regression) {
    idx_train = sample(1:length(y), size = TrainRatio * length(y))
    idx_test = setdiff(1:length(y), idx_train)
  }

  if (!regression) {
    clas = lapply(unique(y), function(x) which(y == x))
    idx_train = unlist(lapply(clas, function(x) sample(x,
                                                       size = TrainRatio * length(x))))
    idx_test = setdiff(1:length(y), idx_train)
  }

  if (shuffle) {

    for (i in c(1:5)){ idx_train = sample(idx_train, length(idx_train)) }

    for (i in c(1:5)){ idx_test = sample(idx_test, length(idx_test)) }
  }

  list(idx_train = idx_train, idx_test = idx_test)
}
