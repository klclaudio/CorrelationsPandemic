#
# Return the legend position
#
define_position_f <- function (tdpm, l_count) {

   if (tdpm == 0) {
      if (l_count != 4){
         legendposition <- "bottomright"
      }else{
         legendposition <- "bottomright"
      }
   }else {
      legendposition <- "bottomright"
   }

   return(legendposition)
}