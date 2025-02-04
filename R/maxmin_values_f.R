#
# Defines Max and Min correlations and it saves results in the folder <homecsv>
#
 maxmin_values_f <- function(homedir, homecsv, N_i, nx, vfilesout, inf_i, sup_i, type_stat, ndata) {
 library("stringr")

 # Analysis j_count: {1, 2, 3, 4}
   j_count <- 1
   for( j_count in c(1:ndata) ){
      efective_days       <- c()
      vec         <- c()
      vec_pv      <- c()
      max_correls <- c()
      max_correls <- c()
      mat_aux     <- c()
      correl_pvalues_aux  <- c()

      efective_days      <- c( inf_i[j_count] : sup_i[j_count] )
      correl_pvalues_aux <- read.csv( vfilesout[j_count] )
      correl_pvalues     <- correl_pvalues_aux[, 2:31]

      mat_aux <- read.csv( str_c(homedir,
                                 homecsv,
                                 "CI",
                                 type_stat,
                                 "_",
                                  j_count, ".csv") )
      mat_ci  <- mat_aux[, 2 : 31]


      auxc           <- c()
      aux_correls    <- c()
      #max ABO+ ABO- ABO
      for( k_count in c(1:15) ){
         vec         <- correl_pvalues[, (2 * k_count - 1)]
         vec_pv      <- correl_pvalues[, (2 * k_count)]
         max_vec     <- round(max( vec[efective_days] ),2)

         ivec        <- which.max( vec[efective_days] )
         max_CI      <- round( mat_ci [ (ivec + efective_days[1] - 1),
                                   c((2 * k_count - 1 ) : (2 * k_count)) ], 2  )


         auxc        <- c( max_vec,
                           round(vec_pv[ivec + efective_days[1] - 1], 2),
                           ivec + efective_days[1] - 1,
                           max_CI )
         aux_correls <- rbind(aux_correls, auxc)
      }
      max_correls <- cbind(max_correls, aux_correls)
      colnames( max_correls) <- c("Correlation", "Pvalue", "Max_Day","CI_InF", "CI_Sup")

      # "A. = A+, A.. = A-"
      rownames( max_correls) <- c( "Pop.",
                                   "O+",
                                   "A+",
                                   "B+",
                                   "AB+",
                                   "O-",
                                   "A-",
                                   "B-",
                                   "AB-",
                                   "O",
                                   "A",
                                   "B",
                                   "AB",
                                   "Rh+",
                                   "Rh-" )

      write.csv( max_correls,
                 str_c( homedir, homecsv, "Correls_Max", type_stat, "_", j_count, ".csv" ))
      print( str_c("Análise ", j_count) )
      print(max_correls)
   }# end for j_count

return()
} #End maxmin fuction
