dependencies <- function(){

   packages_list <- c( "tidyverse",
                       "cluster",
                       "factoextra",
                       "gridExtra",
                       "dplyr",
                       "rafalib",
                       "zoo",
                       "ggcorrplot",
                       "corrplot",
                       "GGally",
                       "stringr")
   installed_packages <- packages_list %in% rownames(installed.packages)
   if ( any(installed_packages == FALSE) ){
      install.packages(packages_list[!installed_packages])
   }
}