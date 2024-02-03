#
#
#

x_label_graph_f <- function(interval_days) {
   x_label <- ""
   if (interval_days == 7){
      x_label <- "Weeks since five deaths"
   }else if (interval_days == 1){
      x_label <- "Days since five deaths"
   }
   return(x_label)
}