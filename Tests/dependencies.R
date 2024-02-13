#
# Install necessary packages for CovidCorrelations
#
dependencies <- function(){

   package_list <- c()
   package_list <- c("tidyverse",
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

   installed_packages <- package_list %in% rownames(installed.packages())

   if (any(installed_packages == FALSE)){
      install.packages(package_list[!installed_packages])
   }

   return(rbind(package_list, installed_packages))
}