#
# Function return ylimit used in correlations
#

limit_f <- function (tdpm,ll) {

  if (tdpm == 0) {
    if( ll !=  4 ) {
      ylimit <- c(-0.2, 1);
    } else{
      ylimit <- c(-1, 1);
    }
  }else{
    if( ll !=  4 ) {
      ylimit <- c(-1.0, 0.4);
    } else{
      ylimit <- c(-1, 1);
    }
  }
  
  return(ylimit)
}