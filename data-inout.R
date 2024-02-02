#
# Data files names (in  and  out)
#

# vfilesout <- c()
# #vfilesout <- rbind(vfilesout, str_c(homedir,homecsv,"bloodcovid-correls",type_stat,".csv") )
# vfilesout_hist <- c()
# vfilesout_hist <- rbind(vfilesout, str_c(homedir,homecsv,"bloodcovid-correls-hist",type_stat,".csv") )

# from file names io
fileout      <- vfilesout [l_count,1]
fileout_hist <- vfilesout [l_count,2] #hist

data_c          <- read.csv( str_c( homedir,
                                    homedata,
                                    cluster_files[l_count],
                                    ".csv") )

data_c_percents <- read.csv( str_c( homedir,
                                    homedata,
                                    cluster_files[l_count],
                                    "-percents.csv") )

#    # data_c          <- read.csv( str_c(homedir, homedata,"bloodcovid-percents.csv") )
#    # data_c_percents <- read.csv( str_c(homedir, homedata,"bloodcovid-percents.csv") )


# End data-inout