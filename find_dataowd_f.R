#
# Verify if exists data ordered from owid ("owid-covid-data19012024.csv")
#
find_dataowd_f <- function(dataowd_file, dataowd_order_file) {

   if( file.exists(dataowd_file)       == TRUE &&
       file.exists(dataowd_order_file) == TRUE){

      print("File Exists - dataowd" )

      return(TRUE)
   }else{

      return(FALSE)
   }
}#
