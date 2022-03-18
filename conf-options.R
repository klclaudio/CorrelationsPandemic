#
# Simulating options 
#

data_site <- 1 # 0 - use previously downloaded data, 1- download from owd site

  tdpm <- 1; # 0 - cumulative deaths,  1- total deaths per million

    logdata <- 1; # Data: 0- without log  transformation,  1- data with log transformation
  
      datascale = 0; # 0- data not standardized,  1- data standardized 
    
        pca_expanded = 0; # 0 - without pca, 1- use of pca analysis
        
          outcountry <- ""; # Countries exclude from analysis 
          
            method_correl = "pearson"; # Correlation method:"pearson",  "spearman"
            errors_correl = FALSE; # correlation method option
              
              getOption( "warn" ); # warn options 
              options( warn = -1 );
            
                            
N_i = 710; # Numbers days considered since 5th death - loop i,  min four countries for analysis
  
  nx = 680;  # Numbers days considered for fitting
    
    ndata = 4 #  Sets of countries analyzed
  
              
# End conf_options.R 

# data_site <- 0; # 0 - use previously downloaded data, 1- download from owd site
# 
# tdpm <- 1; # 0 - cumulative deaths,  1- total deaths per million
# 
# logdata <- 1; # Data: 0- without log  transformation,  1- data with log transformation
# 
# datascale = 0; # 0- data not standardized,  1- data standardized 
# 
# pca_expanded = 0; # 0 - without pca, 1- use of pca analysis
# 
# outcountry <- ""; # Countries exclude from analysis 
# 
# method_correl = "pearson"; # Correlation method:"pearson",  "spearman"
# errors_correl = FALSE; # correlation method option
# 
# # warn options 
# getOption( "warn" );
# options( warn = -1 );
# 
# # Numbers days considered since 5th death - loop i
# N_i = 710; # Min four countries for analysis
# 
# # Numbers days considered for fitting
# nx = 680;
# 
# #  Sets of countries analyzed
# ndata = 4
