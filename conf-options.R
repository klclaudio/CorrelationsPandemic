#
# Simulating options
#
data_site     <- 0 # 0 - use previously downloaded data, 1- download from owd site.
owid_file     <- "owid-covid-data19012024.csv" # Saved data.

tdpm          <- 0 # 0 - cumulative deaths,  1- total deaths per million.
logdata       <- 1 # Data: 0- without log  transformation,  1- data with log transformation.

datascale     <- 0  # 0- data not standardized,  1- data standardized.
pca_expanded  <- 0  # 0 - without pca, 1- use of pca analysis.
outcountry    <- "" # Countries exclude from analysis.

method_correl <- "pearson" # Correlation method:"pearson",  "spearman".
errors_correl <- FALSE     # Correlation method option.

N_i           <- 190 # Numbers week/days considered since 5th death - loop i,  min four countries for analysi.s
nx            <- 180 # Numbers week/days considered for fitting.
ndata         <- 4   #  Sets of countries analyzed.
interval_days <- 7   # Days number from owid data 2024 use 7, 2020-2023 use 1.

# Intervals to find max correlations.
inf_i <- c(35, 35, 35, 35)
sup_i <- c(1, 1, 1, 1) * nx

getOption("warn")
options(warn = -1)
