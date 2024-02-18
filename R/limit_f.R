#
# Function return ylimit used in correlations
#
limit_f <- function (tdpm,l_count) {
  if (tdpm == 0) {
    if( l_count !=  4 ) {
      ylimit <- c(-0.25, 1)
    } else{
      ylimit <- c(-1, 1)
    }
  }else{
    if( l_count !=  4 ) {
      ylimit <- c(-1.0, 0.4)
    } else{
      ylimit <- c(-1, 1)
    }
  }

  return(ylimit)
}