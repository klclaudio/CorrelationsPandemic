#
# Simulating options
#
data_site     <- 0 # 0- use previously downloaded data, 1- download from owd site.

owid_file     <- "owid-covid-data19012024.csv" #Saved data.
#owid_file     <- "owid-covid-data02022024.csv"
#owid_file     <- "owid-covid-data19082020.csv"


tdpm          <- 0  # 0- cumulative deaths, 1- total deaths per million.
logdata       <- 1  # 0- without log  transformation, 1- with log transformation.

datascale     <- 0  # 0- data not standardized, 1- data standardized.
pca_expanded  <- 0  # 0- without pca, 1- use of pca analysis.
outcountry    <- "" # Countries exclude from analysis.

method_correl <- "pearson" # Correlation method: "pearson", "spearman".
errors_correl <- FALSE     # Correlation method option.

# Numbers week/days considered since 5th death - loop i, min four countries for analysi.s
N_i           <- 200 #163 082020  #150 PCA 082020 # 1080 022024

# Numbers week/days considered for fitting.
nx            <- 180 #145         #140 PCA 082020 # 1050 022024

# Sets of countries analyzed.
ndata         <- 4

# Data interval days from owid data, 19012024 uses <7>, data02022024 uses <1> and data19082020 <1>.
interval_days <- 7

# Period to find max correlations.
inf_i <- c(35, 35, 35, 35)
sup_i <- c(1, 1, 1, 1) * nx
#sup_i <- c(120, 120, 120, 145)
getOption("warn")
options(warn = -1)
