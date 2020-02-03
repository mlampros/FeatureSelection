
#.......................
# function used in tests
#.......................

func_nas = function(X, col_idx) {                        # randomly adding NA's
  
  idx = sample(1:length(X[, col_idx]), 30, replace = F)
  
  X[idx, col_idx] = NA
  
  return(X[, col_idx])
}
