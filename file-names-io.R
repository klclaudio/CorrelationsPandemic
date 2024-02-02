#
# File names in and out
#
cluster_files <- c("bloodcovid",
                  "bloodcovid-OutCH-I",
                  "bloodcovid-outlarger",
                  "bloodcovid-larger")

prefix_files <- c( str_c(homedir, homecsv))

sufix_files  <- c( str_c("-correls"     , type_stat, ".csv"),
                   str_c("-correls-hist", type_stat, ".csv" ) )

vfilesout <- matrix(1:8, nrow = 4, ncol = 2)
for (jcount in c(1:2) ) {
   for (icount in 1:ndata){
      vfilesout[icount, jcount] <- str_c( prefix_files ,
                                   cluster_files [icount],
                                   sufix_files   [jcount])
   }
}