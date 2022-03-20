# 
# Return the legend position 
#

define_position_f <- function (tdpm,ll) {
  
  if (tdpm == 0) {
    if (ll != 4){
      legendposition <- "bottomright";
      
    }else{
      legendposition <- "bottomright";
    }
  }else {
    legendposition <- "bottomright";
  }
  
  return(legendposition)
}