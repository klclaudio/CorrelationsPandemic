#
# Install necessary packages for CovidCorrelations
#
dependencies <- function(package_list){

   installed_packages <- package_list %in% rownames(installed.packages())

   if (any(installed_packages == FALSE)){
      install.packages(package_list[!installed_packages])
   }

   return(rbind(package_list, installed_packages))
}


package_list_main <- c( "rafalib",
                        "stringr" )
print(dependencies(package_list_main))

package_list_pca  <- c( "factoextra",
                        "FactoMineR",
                        "ggcorrplot",
                        "corrplot",
                        "dplyr" )
print(dependencies(package_list_pca))
