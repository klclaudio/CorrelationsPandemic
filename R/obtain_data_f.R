#
# Obtaining data from owid site
#
obtain_data_f <- function(owid_file, data_site, date_analysis, homedir, homedata) {
   library("curl")
   library("stringr")

   source( str_c(homedir, "dataowd5_site_f.R") )
   source( str_c(homedir, "find_dataowd_f.R") )


   owd_date           <- date_analysis
   dataowd_file       <- str_c(homedir, homedata, "dataowd5-", owd_date, ".csv")
   dataowd_order_file <- str_c(homedir, homedata, "dataowd5-", owd_date, "-order.csv")

   if( data_site == 1 ) {

      home_site <- "https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv"
      owid_file <- str_c('owid-covid-data', date_analysis, '.csv' )
      curl_download( home_site, str_c(homedir, homedata, owid_file ) )
      dataowd5_site_f( owid_file,
                       date_analysis,
                       homedir,
                       homedata,
                       dataowd5,
                       dataowd5_days )

   }else if ( find_dataowd_f(dataowd_file, dataowd_order_file)){
      dataowd5      <<- read.csv(dataowd_file)
      dataowd5_days <<- read.csv(dataowd_order_file)
      date_analysis <<- owd_date
   }else{
      dataowd5_site_f( owid_file,
                       date_analysis,
                       homedir,
                       homedata,
                       dataowd5,
                       dataowd5_days )  # data from owd site
      }
      return()
}

#}# End obtain_data