#
# Simulating options
#
# Erro  na simulação:
#  1 - verificar montagem arquivos dataowd* pois possuem somente 200 dias de análise.
#  2 - o arquivo base baixado do owdi está ok quanto aos dias.
#  3 - verificar se houve mudanças nas colunas do arquivo base owdi *.csv .
#    - as colunas estão ok
#    - as linhas foram alteradas:
#        os dados forma agrupados por semana nas novas versões dos arquivo da owdi.
#  4- verificar montagem dos arquivos de dados levando em conta o o agrupamento semanal.
#    - esse estavava ok
#    Identificado o problema: o numero de semanas em análise estava sendo considerado errado, assim
#    foram corrigidod os valores de N_i e Nx adequando-se ao número total de semanas.
#

data_site  <- 0 # 0 - use previously downloaded data, 1- download from owd site
owid_file  <- "owid-covid-data19012024.csv"
#owid_file <- "owid-covid-data19082020.csv"
tdpm       <- 0 # 0 - cumulative deaths,  1- total deaths per million
logdata    <- 1 # Data: 0- without log  transformation,  1- data with log transformation

datascale    <- 0  # 0- data not standardized,  1- data standardized
pca_expanded <- 1  # 0 - without pca, 1- use of pca analysis
outcountry   <- "" # Countries exclude from analysis

method_correl <- "pearson" # Correlation method:"pearson",  "spearman"
errors_correl <- FALSE     # correlation method option

N_i           <- 190 #190 # 163; 1080 # Numbers days considered since 5th death - loop i,  min four countries for analysis
nx            <- 180 #180 # 145; 1050  # Numbers days considered for fitting
ndata         <- 4 #  Sets of countries analyzed
interval_days <- 7 # days number from owid data 2024 use 7, 2020-2023 use 1

# determinação dos intervalos para encontrar correalações máximas para cada analise
# maxmin_values_f. R
inf_i <- c( 35, 35, 35, 35 )
sup_i <- c( 1, 1, 1, 1 ) * nx


getOption("warn") # warn options
options(warn = -1)

# End conf_options.R

# data_site <- 0 # 0 - use previously downloaded data, 1- download from owd site
#
# tdpm <- 1 # 0 - cumulative deaths,  1- total deaths per million
# ogdata <- 1 # Data: 0- without log  transformation,  1- data with log transformation
#
# datascale = 0 # 0- data not standardized,  1- data standardized
#
# pca_expanded = 0 # 0 - without pca, 1- use of pca analysis
#
# outcountry <- "" # Countries exclude from analysis
#
# method_correl = "pearson" # Correlation method:"pearson",  "spearman"
# errors_correl = FALSE # correlation method option
#
# # warn options
# getOption( "warn" )
# options( warn = -1 )
#
# # Numbers days considered since 5th death - loop i
# N_i = 710 # Min four countries for analysis
#
# # Numbers days considered for fitting
# nx = 680
#
# #  Sets of countries analyzed
# ndata = 4
