# Daily deaths plots - valid for N_i > 35
if( N_i > 35 ) {

	  source( str_c(homedir, "daily-deaths.R") )

}

# Normality plots

#   source( str_c(homedir, homenorm, "evolution-normality.R" ) )

# Max and min values of correlations

maxmin_values_f( homedir, homecsv, N_i, nx, vfilesout )

# Comparative graphics of pvalues and correlations

source( str_c(homedir, "pvalues-dif.R") )

# Fit polinomial  correlations

source( str_c(homedir, "fitting-correlations.R") )
