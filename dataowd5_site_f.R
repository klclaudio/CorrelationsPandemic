#
# Date file with data since 5 deaths
#

dataowd5_site_f <- function (date_analysis, home_site, homedir, homedata) {
# Create dataowd5_DATE from owid-covid-data, organize  total_deaths and eliminate data with less 4 deaths
# 
# 1 - owid-covid-data_ ordered by total_deaths 
# 2 - eliminates data wiyh less than 4 deaths
# 3 - ordering by countries 
# 4 - ordering by DaysSince5 and rename to dataowd5_data_order
#

library( "curl" ) 

auxdata <- c();
  curl_download( home_site, str_c(homedir, homedata, 'owid-covid-data', date_analysis, '.csv') );
     auxdata <- read.csv( str_c(homedir, homedata, 'owid-covid-data', date_analysis, '.csv') );


dimauxdata <- dim(auxdata);
  auxrow <- dimauxdata[1];
      auxcol <- dimauxdata[2];

auxdatam <- c();
  auxdatam <- as.matrix(auxdata);


dataowd5_aux <- cbind(auxdata[, 1:6], auxdata[, 8], c(1:auxrow), auxdata[, 9:auxcol]);


auxnames <- colnames(dataowd5_aux);
  auxnames[8] <- "DaysSince5";
    auxnames[7] <- "total_deaths";
      colnames(dataowd5_aux) <- auxnames;
datowd5_aux <- dataowd5_aux[ order(dataowd5_aux$total_deaths), ];


dataowd5 <- c();
    dataowd5 <- subset( dataowd5_aux, dataowd5_aux[, 7] > 4 );
#head(dataowd5)
    
datowd5 <- dataowd5[ order(dataowd5$location), ];

dimensao <- dim(dataowd5);
  N <- dimensao [1]; 
    M <- dimensao [2];

dataowd5[2, 8] <- 1
  i <- 2
    k <- 2
dataowd5[1, 8] = 1


while( i <= N ) {
      if( dataowd5[i, 3] == dataowd5[i-1, 3] ) {
           dataowd5[i, 8] = k;
           
      }
      else {
           k = 1
           dataowd5[i, 8] = k;
      }
      i = i+1;
      k = k+1;

}


write.csv( dataowd5, str_c(homedir, homedata, "dataowd5-", date_analysis, ".csv"), row.names = TRUE );
   dataowd5 <- dataowd5[ order( dataowd5$DaysSince5 ), ];
      write.csv( dataowd5, str_c(homedir, homedata, "dataowd5-", date_analysis, "-order.csv"), row.names = TRUE );


return()  
} # End dataowd5_site_f
