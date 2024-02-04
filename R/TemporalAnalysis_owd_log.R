#
# This is the a log version refactored, the preliminary results were presented in:
#
# Claudio, Kleucio; Viviani Thomazini Luis Fernando;
# Silva-Santos, Carlos Henrique; Sasaki, Eduardo Noboru;
#
#                   ABO/Rh Blood Groups and COVID-19:
# Temporal Analyses Point Out Rh-negative with the Greatest Correlations
#                          (February 21, 2021).
# Available at SSRN:
#    https://ssrn.com/abstract=3794044 or http://dx.doi.org/10.2139/ssrn.3794044
#
# v0.0 - base code.
# v0.1 - refactored and new comments.
# v0.2 - add analysis option for death rates per million and refactorings.
# v1.0 - use of 2024 owid data (weekly),
#        PCA,
#        log option only,
#        column name instead of number,
#        refactorings and
#        new structure.
#
rm(list = ls(all = TRUE))
# Libraries in use
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
invisible(lapply(packages_list, library, character.only = TRUE))

homedir <-  str_c(getwd(), "/")
homedir <- str_c(homedir,"R/")
# Functions
source( str_c(homedir, "pca_analysis_f.R") )
source( str_c(homedir, "discontinuity_f.R") )
source( str_c(homedir, "maxmin_values_f.R") )
source( str_c(homedir, "correlations_f.R") )
source( str_c(homedir, "create_dir_f.R") )
source( str_c(homedir, "limit_f.R") )
source( str_c(homedir, "define_position_f.R") )
source( str_c(homedir, "obtain_data_f.R") )
source( str_c(homedir, "x_label_graph_f.R") )
source( str_c(homedir, "type_blood_f.R") )

# Configure choices like log, escale, data ...
source( str_c(homedir, "conf-options.R") )

# Defining date of analysis
if (data_site == 1) {
   datetemp      <- Sys.Date() # max date of the analysis
   date_analysis <- format(datetemp, format="%d%m%Y")
}else{
   date_analysis <- strsplit((strsplit(owid_file, split ="data")[[1]][2]), ".csv")
}

# Create result subdirectories
source( str_c(homedir,"home-directories.R") )

# Obtaining data from owid site or reading from
dataowd5      <- c()
dataowd5_days <- c()
print(" Data reading")
obtain_data_f(owid_file, data_site, date_analysis, homedir, homedata)
# Data sorted by countries
#dataowd5 <- read.csv( str_c(homedir, homedata, "dataowd5-", date_analysis, ".csv") )
# Data sorted by numbers days since 5th death.
#dataowd5_days <- read.csv( str_c(homedir, homedata, "dataowd5-", date_analysis, "-order.csv") )
print(" Read files - Ok")

# Many countries maybe excluded from the analysis when use PCA
# cols: 45-populations, 44-stringency_index, 46-populations_density, 46-cardiovasc_death_rate,
#       47-diabetes_prevalence, 51-hospital_beds_per_thousand, 52-life_expectancy,
#       53-human_development_index

pval       <- c()
errors_a   <- c()
wstat      <- c()
errors_lin <- c()

covid_ll   <- c()
coeff_aux  <- c()
coeff_a    <- c()
coeff_lin  <- c()

r2                <- c()
mat_coeffs_csv    <- c()
mat_coefilin_csv  <- c()
mat_coefia_csv    <- c()
mat_statslin_csv  <- c()
mat_statsa_csv    <- c()
mat_stderrlin_csv <- c()
mat_stderra_csv   <- c()

# logdata:    0 - without log  transformation,
#             1 - data with log transformation from conf-options.R
if(logdata == 0) {
   type_stat <- ""
}else {
   type_stat <- "Log"
}

# ndata - Sets of countries analyzed
# for l_count...numbers of analysis (l_count)  -  l_count: 1 ... 4
#    for i_days........numbers days (N_i) -  i_days : 1 ... N_i

source( str_c(homedir,"file-names-io.R") )

for( l_count in c(1:ndata) ) {
   # Data and out files
   data_c_percents <- c()
   source( str_c(homedir, "data-inout.R") )

   colnames( data_c_percents)  <- c( "X",
                                     "Pop.",
                                     "O+",
                                     "A+",
                                     "B+",
                                     "AB+",
                                     "O-",
                                     "A-",
                                     "B-",
                                     "AB-",
                                     "Covid" )

   names_col_aux2  <- colnames( data_c_percents )

   data_c_percents <- tibble::column_to_rownames(data_c_percents, var <- "X")

   size_n <- dim(dataowd5)
   days   <- c()
   N      <- size_n[1]
   i_days      <- 1
   k      <- 1
   # Vector positions for countries
   while( k <= N - 1 ) {
      if( dataowd5[k, "location"] == dataowd5[k + 1, "location"] ) {
         i_days <- i_days + 1
         k <- k + 1
      }else {
         days <- c(days, i_days)
         i_days <- 1
         k <- k + 1
      }
   }

   days <- c(days, i_days)

   daysa5       <- c()
   diarydeaths  <- c()
   somadeaths_i <- 0
   i_days       <- 1
   k            <- 1
   # Total daily deaths from owd, aligned  since 5th death
   while(k <= N - 1) {

      if( is.na(dataowd5_days[k, "new_deaths"] ) == TRUE ) {
          dataowd5_days[k, "new_deaths"] <- 0
      }# End if

      if( dataowd5_days[k, "DaysSince5"] ==
          dataowd5_days[k + 1, "DaysSince5"] ) {
         if( dataowd5_days[k, "continent"] != "" &&
             is.na(dataowd5_days[k, "new_deaths"]) != TRUE ) {
            somadeaths_i <- somadeaths_i + dataowd5_days[k, "new_deaths"]
         }
         i_days <- i_days + 1
      }else {
         daysa5 <- c(daysa5, i_days)
         if( dataowd5_days[k, "continent"] != "" &&
             is.na(dataowd5_days[k, "new_deaths"] ) != TRUE) {
            somadeaths_i <- somadeaths_i + dataowd5_days[k, "new_deaths"]
            diarydeaths  <- c(diarydeaths, somadeaths_i)
         }
         i_days <- 1
      }# End if
      k <- k + 1

   }# End k while

   cat ("Analysis: ", l_count, "- Total deaths: ", somadeaths_i, "\n")

   daysa5       <- c(daysa5, i_days)
   somadeaths_i <- somadeaths_i + dataowd5_days[k, "new_deaths"]
   diarydeaths  <- c(diarydeaths, somadeaths_i)

   ftsize <- 1.3
   png( str_c(homedir, homeCounEvol, "Pandemic_", date_analysis, ".png"), width <-500, height <-500 )
   mypar()
   par( new <- FALSE )
   par( mar <- c(4, 4, 4, 4) )
   plot( daysa5, main = str_c("Covid-19 Pandemic in ", date_analysis),
         ylab     = "Number of Countries",
         xlab     = "Days since five deaths",
         cex.main = ftsize,
         cex.lab  = ftsize,
         cex.axis = ftsize,
         type     = "l",
         lty      = 1 )
   grid(lty  = 3, lwd = 1)
   dev.off()

   Nds   <- max( daysa5 )
   Ndays <- c(1:Nds)

   # Percents occurrences of blood types in countries
   namesdatac    <- rownames(data_c_percents) # using data from owd
   dimnamesdatac <- dim( as.matrix(namesdatac) )

   maxk  <- 0
   cores <- c( "aquamarine3", "firebrick1", "darkviolet", "darkorange3", "gold4",
               "darkblue", "green", "darkslategray1", "deepskyblue", "darkseagreen" )

   # Aux matrix with  i_days days since the 5th death
   aux   <- c()
   aux2  <- c()

   vet_ones         <- c(1, 1, 1, 1)
   covid            <- c()
   cumulative_covid <- c()

   correlacoes         <- c();  correlacoest         <- c();  correlacoest_CI        <- c()
   correlacoes_abo_rh  <- c();  correlacoest_abo_rh  <- c();  correlacoest_CI_abo_rh <- c()

   p_values_histogram   <- c()
   Wstatistic_histogram <- c()
   covid_i              <- c()
   coeff_ll             <- c()

 # Analysis coefficients
   # --------------- N_i- total days since 5th death ---------------
   # i_days - numbers days since 5th death
      for( i_days in c(1:N_i) ) {
         cat( "Analysis: ", l_count, "- Days Since Five: ", i_days, "\n")

         covid         <- c()
         covid_r       <- c()
         rate_covid    <- c()
         aux           <- c()
         aux2          <- c()
         vars_pca      <- c()
         names_aux2    <- c()
         aux2_percents <- c()

         maxk <- maxk + daysa5[i_days]

         l <- 1
         # k- numbers of countries with i_days days since 5th death
         for( k in c(maxk - daysa5[i_days] + 1: daysa5[i_days]) ) {
            aux <- rbind( aux, dataowd5_days[k, ] )
            l <- l + 1
         }

         l <- l - 1
         j <- 1
         somacovid <- 0
         # Assembling countries and deaths from owd data
         # k - countries with i_days days since 5th death
         for( k in c(1:l) ) {
            m     <- 1
            teste <- FALSE
            while( aux[k, "location"] != namesdatac[m] && teste == FALSE ) {
               m <- m + 1
               if( m == dimnamesdatac[1] ) {
                  teste <- TRUE
               }
            }
            if( aux[k, "location"] == namesdatac[m] && is.na( aux[k, "total_deaths"] ) == FALSE ) {
               # Using data poopulation from owid
               aux2 <- rbind( aux2, c( as.numeric(aux[k, "population"]),
                              as.numeric(data_c_percents[m, 2:9]) * as.numeric(aux[k, "population"]) ))

               # using data population from bood type table.
               #aux2 <- rbind( aux2,
               #               c( as.numeric(data_c_percents[m, 1]),
               #                 as.numeric(data_c_percents[m, 2:9]) * as.numeric(data_c_percents[m, 1]) ))

               #  P C A
               varnames <-  c( "population",
                               "population_density",
                               "median_age",
                               "aged_65_older",
                               "aged_70_older",
                               "gdp_per_capita",
                               "extreme_poverty",
                               "cardiovasc_death_rate",
                               "diabetes_prevalence",
                               "hospital_beds_per_thousand",
                               "life_expectancy")
                               #"human_development_index")
                               #"people_fully_vaccinated_per_hundred" )
               vars_pca <- rbind( vars_pca, c( as.numeric(aux[k, varnames]) ) )

               names_aux2 <- rbind( names_aux2, aux[k, "location"] )
               covid_r    <- cbind( covid_r, as.matrix(aux[k, "total_deaths"]) )

               covid <- cbind( covid, log(as.matrix(aux[k, "total_deaths"])) )

               # using rate per milion the data normality vanish
               if (tdpm == 1) {
                  rate_covid <- cbind( rate_covid, log(as.matrix(aux[k, "total_deaths"])/(as.numeric(aux[k, "population"])/10^6)) ) # daily rates
                  covid      <- rate_covid
               }

               somacovid <- somacovid + aux[k, "new_deaths"]
               j <- j + 1
            } #end if namesdatac
         } # end for k<-1:l

         rownames(aux2) <- names_aux2
         colnames(aux2) <- names_col_aux2[2:10]
         if( pca_expanded == 1 ) {
            colnames(vars_pca) <- varnames
            dim_vars           <- dim(vars_pca)
            aux2               <- cbind( aux2, vars_pca[, 2:dim_vars[2]] )
            colnames(aux2)     <- c( names_col_aux2[2:10], colnames(vars_pca[, 2:dim_vars[2]]) )
         }

         cat( "[Countries, Pop+Blood Types]\n")
         cat( dim(aux2)," \n\n" )

         # Exclude countries from data
         if(outcountry != "") {
            if(i_days >= 120) {
               dimaux2_antes  <- dim(aux2)
               print( "Excluding countries" )
               aux2           <- discontinuity( aux2, covid, outcountry )
               dimaux2_depois <- dim(aux2)
               if( dimaux2_antes[2] != dimaux2_depois[2] ) {
                  dimaux2 <- dim(aux2)
                  covid   <- aux2[, dimaux2[2]]
                  aux2    <- aux2[, 1:dimaux2[2]-1]
                  covid   <- t(covid)
               }
            }
         }# End if outcountry

         aux2_r <- aux2  # Dados sem transformação logaritimicas

         aux2 <- log(aux2)


         covid_i[i_days] <- somacovid

         aux3      <- cbind( aux2, t(covid) )     # log data
         aux3_r    <- cbind( aux2_r, t(covid_r) ) # original data
         aux3      <- na.omit(aux3)
         aux3_r    <- na.omit(aux3_r)

         aux3      <- aux3[!is.infinite( rowSums(aux3) ), ]
         aux3_r    <- aux3_r[!is.infinite( rowSums(aux3_r) ), ]

         dim_aux2  <- dim(aux2)
         aux2      <- aux3[, 1:(dim_aux2[2])]   # aux3 <- [ Pop, ABO + , ABO-, <6VARS>, covid]
         aux2_r    <- aux3_r[, 1:(dim_aux2[2])]

         covid     <- aux3[, dim_aux2[2] + 1]   # log data
         covid_r   <- aux3_r[, dim_aux2[2] + 1] # original data

         covid   <-  as.numeric(covid)
         covid_r <-  as.numeric(covid_r)
         aux2    <-  as.matrix(aux2)

         crow_c              <- order(covid)
         covid               <- covid[crow_c]
         cumulative_covid    <- rbind( cumulative_covid, sum(covid) )   # log data
         cumulative_expcovid <- rbind( cumulative_covid, sum(covid_r) ) # Original data
         aux2   <- aux2[crow_c, ]
         aux2_r <- aux2_r[crow_c, ]
            #verificantdo o uso covid_r
         covid_r <- covid_r[crow_c]

         correl_1        <- c()
         correltest_1    <- c()
         correltest_CI_1 <- c()

         for( k_count in c(1:9) ) {
            param                  <- c()
            param                  <- correlations_f( aux2[, k_count], covid )
            correl_1 [k_count]     <- param[1]
            correltest_1 [k_count] <- param[2]
            correltest_CI_1        <- cbind( correltest_CI_1, t(c(param[3], param[4])) )
         }

         correlacoes     <- rbind( correlacoes, (correl_1) )            # correlation
         correlacoest    <- rbind( correlacoest, (correltest_1) )       # Pvalue
         correlacoest_CI <- rbind( correlacoest_CI, (correltest_CI_1 ) )# Confidence interval

         # ABO analysis
         # atention log(a + b) ~= loga + log b <- log a*b
         # example: log(aux2_r[, 1:9] %*% c(0, 1, 0, 0, 0, 1, 0, 0, 0)

         type_blood = c()
         type_blood <- type_blood_f(aux2_r)
         type_blood <- log(type_blood)

         correl_2        <- c()
         correltest_2    <- c()
         correltest_CI_2 <- c()
         for( k_count in c(1:6) ) {
            param                  <- c()
            param                  <- correlations_f( (type_blood[k_count,]), covid )
            correl_2 [k_count]     <- param[1]
            correltest_2 [k_count] <- param[2]
            correltest_CI_2        <- cbind( correltest_CI_2, t(c(param[3], param[4])) )
         }

         correlacoes_abo_rh     <- rbind(correlacoes_abo_rh    , (correl_2))        # Correlation
         correlacoest_abo_rh    <- rbind(correlacoest_abo_rh   , (correltest_2))    # Pvalue
         correlacoest_CI_abo_rh <- rbind(correlacoest_CI_abo_rh, (correltest_CI_2)) # Confidence interval

         if(pca_expanded == 1) {
            if(l_count == 1 || l_count == 3 || l_count == 4) {
               source( str_c(homedir, "aux4-pca.R") )
            }
         }
         #
         source( str_c(homedir, "coefficients.R") )

         # Pictures of log blood types from countries and deaths (covid vector)
         if(i_days == 120) {

            source( str_c(homedir, "plot-bloodtypes.R") )

         }

         # Pictures of the pandemic evolution used to make video
         source( str_c(homedir, "movie-covid.R") )

         # Normality test for each i_days day
         source( str_c(homedir, "histograms-tests.R") )

         # Evolutions of covid ordered for each day.
         source( str_c(homedir, "covid-evolution.R") )

      } # End loop i_days for N_i days since 5a death.
      print("End i_days loop" )
   #
   source( str_c(homedir, "colnames-correlations.R") )

   # Coefficientts of the  Covid-19 analysis
   source( str_c(homedir, "coeff-covid19.R") )

   # Cumulative_covid
   source( str_c(homedir, "plot-cumulative-covid.R") )

   # Saving correlations files
   print("Gravando arquivos")
   source( str_c(homedir, "correlation-outputs.R") )

   # Data Normality
   source( str_c(homedir, "evolution-normality.R") )

   # Temporal Correlations Plots
   source( str_c(homedir, "plot-correlations.R") )

   # P_values plots
   source( str_c(homedir, "plot-pvalues.R") )

   # Daily deaths matrix
   covid_ll <- cbind(covid_ll, covid_i)

} # End  l_count loop (countries sets)
print("End l_count loop")


# Daily deaths plots - valid for N_i > 35
if( N_i > 35 ) {
   source( str_c(homedir, "daily-deaths.R") )
}

# Normality plots
#source( str_c(homedir, homenorm, "evolution-normality.R" ) )

# Max and min correlations
maxmin_values_f(homedir, homecsv, N_i, nx, vfilesout, inf_i, sup_i, type_stat, ndata)

# Comparative graphics of pvalues and correlations
source( str_c(homedir, "pvalues-dif.R") )

# Fit polinomial  correlations
source( str_c(homedir, "fitting-correlations.R") )

###################################################################
#*---------------------------------------------------------------*#
                print( "The program finished!" )
#*---------------------------------------------------------------*#
###################################################################