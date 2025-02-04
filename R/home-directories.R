#
# Work and Result directories
#
if (pca_expanded == 1){
   str_pca <- "pca-"
}else{
   str_pca <- ""
}

#tdpm: 0 - cumulative deaths,  1- total deaths per million
if (tdpm == 0) {
   homeresults <- str_c("Results-cumulative-", str_pca, date_analysis, "/")
   create_dir_f( str_c(homedir, homeresults) )
}else {
    # tdpm == 1
   homeresults <- str_c("Results-permillion-", date_analysis, "/")
   create_dir_f( str_c(homedir, homeresults) )
 }

homepca  <- str_c( homeresults, "PCA/" )
create_dir_f( str_c(homedir, homepca) )

homedaily <- str_c( homeresults, "Daily/" )
create_dir_f( str_c(homedir, homedaily) )

homecsv <- str_c( homeresults, "CSV/" )
create_dir_f( str_c(homedir, homecsv) )

homefit <- str_c( homeresults, "Fitting/" )
create_dir_f( str_c(homedir, homefit) )

homeCounEvol <- str_c( homeresults, "CountriesEvolutions/" )
create_dir_f( str_c(homedir, homeCounEvol) )

homebld <- str_c( homeresults, "BloodTypes/" )
create_dir_f( str_c(homedir, homebld) )

homemov <- str_c( homeresults, "Movies/" )
create_dir_f( str_c(homedir, homemov) )

homecum <- str_c( homeresults, "Cumulative/" )
create_dir_f( str_c(homedir, homecum) )

homenorm <- str_c( homeresults, "Normality/" )
create_dir_f( str_c(homedir, homenorm) )

homehist <- str_c( homeresults, "Histograms/" )
create_dir_f( str_c(homedir, homehist) )

homecorr <- str_c( homeresults, "Correlations/" )
create_dir_f( str_c(homedir, homecorr) )

homepval <- str_c( homeresults, "Pvalues/" )
create_dir_f( str_c(homedir, homepval) )

homecoeff <- str_c( homeresults, "Coefficients/" )
create_dir_f( str_c(homedir, homecoeff) )

homedata <- str_c( "../Data/" )
create_dir_f( str_c(homedir,homedata) )

# End home-directories
