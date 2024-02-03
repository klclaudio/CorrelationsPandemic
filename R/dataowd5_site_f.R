#
# Create data file since 5 deaths and ordering data
#
dataowd5_site_f <- function (owid_file, date_analysis, homedir, homedata, dataowd5, dataowd5_days) { # nolint
   #library( "curl" )

   auxdata <- c()
   auxdata <- read.csv( str_c(homedir, homedata, owid_file) )


   dim_auxdata <- dim(auxdata)
   auxrow      <- dim_auxdata[1]
   auxcol      <- dim_auxdata[2]

   #auxdatam <- c()
   #auxdatam <- as.matrix(auxdata)
   #versão 2024
   dataowd5_aux <- cbind(auxdata[, 1:6], auxdata[, 8], c(1:auxrow), auxdata[, 9:auxcol])


   #dataowd5_aux <- cbind(auxdata[, 1:6], auxdata[, "total_deaths"], c(1:auxrow), auxdata[, 9:auxcol])

   auxnames    <- colnames(dataowd5_aux)
   auxnames[8] <- "DaysSince5"
   auxnames[7] <- "total_deaths"
   colnames(dataowd5_aux) <- auxnames

   datowd5_aux <- dataowd5_aux[ order(dataowd5_aux$total_deaths), ]
   dataowd5    <- c()
   dataowd5    <- subset( dataowd5_aux, dataowd5_aux[, "total_deaths"] > 4 )
   #datowd5 <- dataowd5[ order(dataowd5$location), ]

   dimensao <- dim(dataowd5)
   N        <- dimensao [1]
   #M <- dimensao [2]

   dataowd5[2, 8] <- 1
   dataowd5[1, 8] <- 1

   i_count <- 2
   k       <- 2

   while(i_count <= N) {
      if( dataowd5[i_count, "location"] == dataowd5[i_count-1, "location"] ) { #nomes dos países
         dataowd5[i_count, "DaysSince5"] <- k
         }
      else {
         k <- 1
         dataowd5[i_count, "DaysSince5"] <- k
      }
      i_count <- i_count + 1
      k <- k + 1
   }

   #print("Final dataowd")
   dataowd5 <<- dataowd5
   write.csv( dataowd5, str_c(homedir, homedata, "dataowd5-", date_analysis, ".csv"), row.names = TRUE )

   dataowd5_days <<- dataowd5[ order( dataowd5$DaysSince5 ), ]
   write.csv( dataowd5_days, str_c(homedir, homedata, "dataowd5-", date_analysis, "-order.csv"), row.names = TRUE )

   return()
} # End dataowd5_site_f
