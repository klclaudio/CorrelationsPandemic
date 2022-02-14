#
# This is the general version, the preliminary results were presented in: 
#
# Claudio, Kleucio and
# Viviani Thomazini, Luis Fernando and
# Silva-Santos, Carlos Henrique and 
# Sasaki, Eduardo Noboru, 
# ABO/Rh Blood Groups and COVID-19: 
#    Temporal Analyses Point Out Rh-negative with the Greatest Correlations 
# (February 21, 2021). 
# Available at SSRN: 
#    https://ssrn.com/abstract=3794044 or http://dx.doi.org/10.2139/ssrn.3794044 
#
# v0.0 base code 
# v0.1 refactored and new comments.
# V0.2 next steps: PCA analyses.

rm( list = ls(all=TRUE) );

library( tidyverse );
 library( cluster );
   library( factoextra ); 
    library( gridExtra );
      library( dplyr );
        library( rafalib );

# corrplot
library( zoo );
  library( ggcorrplot );
   library( corrplot );
    library( GGally );

# Must config your homedir here: 
     homedir <- "/home/klclaudio/Documents/CovidCorrelations/";
# homedir <- "<your homedir>";

#functions
source( str_c(homedir, "pca_abo_f.R") ); # Principal Component Analysis
   source( str_c(homedir, "discontinuity_f.R") ); # Eliminate countries from analysis
      source( str_c(homedir, "dataowd5_site_f.R") ); # Data from owd site
         source( str_c(homedir, "maxmin_values_f.R") );
    source( str_c(homedir, "correlations_f.R") );
source( str_c(homedir, "create_dir_f.R") );


# Defining max date of analysis
data_site <- 0; # 0 - use previously downloaded data, 1- download from owd site
  datetemp <- Sys.Date(); # max date of the analysis;
    date_analysis <- format( datetemp, format="%d%m%Y" );
      homeresults <- str_c( "Results", date_analysis );
  
# Result folders  
create_dir_f( str_c(homedir, "/Results", date_analysis) ); 

# Work and results directories

  homepca  <- str_c( homeresults, "/PCA/" );
  create_dir_f( str_c(homedir, homepca) );
  
     homedaily <- str_c( homeresults, "/Daily/" );
     create_dir_f( str_c(homedir, homedaily) );
    
        homecsv <- str_c( homeresults, "/CSV/" );
        create_dir_f( str_c(homedir, homecsv) );
        
           homefit <- str_c( homeresults, "/Fitting/" );
           create_dir_f( str_c(homedir, homefit) );
           
              homeCounEvol <- str_c( homeresults, "/CountriesEvolutions/" );
              create_dir_f( str_c(homedir, homeCounEvol) ); 
              
                 homebld <- str_c( homeresults, "/BloodTypes/" );
                 create_dir_f( str_c(homedir, homebld) );
                 
                    homemov <- str_c( homeresults, "/Movies/" );
                    create_dir_f( str_c(homedir, homemov) );
                 
                  homecum <- str_c( homeresults, "/Cumulative/" );
                  create_dir_f( str_c(homedir, homecum) );
                  
              homenorm <- str_c( homeresults, "/Normality/" );
              create_dir_f( str_c(homedir, homenorm) );
              
           homehist <- str_c( homeresults, "/Histograms/" );
           create_dir_f( str_c(homedir, homehist) );
           
        homecorr <- str_c( homeresults, "/Correlations/" );
        create_dir_f( str_c(homedir, homecorr) );
        
     homepval <- str_c( homeresults, "/Pvalues/" );
     create_dir_f( str_c(homedir, homepval) );  
     
  homecoeff <- str_c( homeresults, "/Coefficients/" );
  create_dir_f( str_c(homedir, homecoeff) );
  
homedata <- "files_aux/";

getOption( "warn" );
options( warn = -1 )


#data

if( data_site == 1 ) {

    home_site <- "https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv";
    dataowd5_site_f( date_analysis, home_site, homedir );  # data from owd site
    
}  

# Ordering countries by days of occurrences since the 5th death.
# source("<dir>/dataowd5.R")

# Data sorted by countries
dataowd5 <- read.csv( str_c(homedir, "dataowd5_", date_analysis, ".csv") );

# Data sorted by numbers days since 5th death.
dataowd5_days <- read.csv( str_c(homedir, "dataowd5_", date_analysis, "_order.csv") );

# Many countries are excluded from the analysis when use pca
# cols: 45-populations, 44-stringency_index, 46-populations_density, 46-cardiovasc_death_rate, 
#       47-diabetes_prevalence, 51-hospital_beds_per_thousand, 52-life_expectancy, 
#       53-human_development_index

head ( dataowd5[, c(4, 8, 9)] ); 
head ( dataowd5_days[, c(4, 8, 9, 45)] ); 

pval  <- c();
wstat <- c();

covid_ll <- c();
  coeff_a <- c();
    coeff_aux <- c();
      coeff_lin <- c();

errors_a <- c(); 
  errors_lin <- c();

r2 <- c();
  mat_coeffs_csv <- c();
  
mat_coefilin_csv  <- c();
   mat_statslin_csv <- c(); 
     mat_stderrlin_csv <- c();
      
mat_coefia_csv <- c();
   mat_statsa_csv <- c();
      mat_stderra_csv <- c();

#-------------------------------------------------------------------------------

# Correlation method
#  method_correl = "spearman";
   method_correl = "pearson";
   errors_correl = FALSE;

# Numbers days considered since 5th death - loop i
   N_i = 696; # Min four countries for anlysis

# Numbers days considered for fitting
   nx = 660;

# Data: 0- without log  transformation,  1- data with log transformation

   logdata <- 1;
    if( logdata == 0 ) {
       type_stat = "";
    }else {
       type_stat = "Log"; 
    }


   datascale = 0; # 0- data not standardized,  1- data standardized 

#  PCA (Principal Component Analysis)
   pca_expanded = 0;# 0- not use PCA, 1- use PCA

# Countries exclude from analysis 
   outcountry <- "";

#  ndata - Sets of countries analyzed
   ndata = 4;

#-------------------------------------------------------------------------------
#
#for ll...numbers of analysis (ll)  -  ll: 1 ...4
# ...
#  for i.........numbers days (N_i) -  i: 1.. N_i
#   
for( ll in c(1:ndata) ) {
 
    data_c <-c();
      data_c_percents <- c();
   
    # Data and out files
    source( str_c(homedir, "data_inout.R") );   
   
    colnames( data_c_percents)  <- c("X", "Pop", "O+", "A+", "B+", "AB+", "O-", 
                                     "A-", "B-", "AB-", "Covid");
       names_col_aux2 <- colnames( data_c_percents );

    data_c <- tibble::column_to_rownames(data_c, var = "X");
    data_c_percents <- tibble::column_to_rownames( data_c_percents, var = "X" );

    # Occurrences of blood types in countries
    aux_data_c <- c();
      aux_data_c <- data_c[, 1:9];
        data_c <- c();

    if( logdata == 1 ) {
        data_c <- log( aux_data_c[, 1:9] ); 
    }else {
        data_c <- aux_data_c[, 1:9];
    }

    dimensao = dim( dataowd5 );
     N <- dimensao[1];
      i = 1;
       k = 1;
        days <- c();

# Vector positions for countries
    while( k <= N-1 ) {
    
      if( dataowd5[k, 4] == dataowd5[k+1, 4] ) {
          i = i+1;
          k = k+1;
      }else {
          days <- c(days, i);
          i = 1;
          k = k+1;
      }
    }
    days <- c(days, i)

    i = 1;
      k = 1;
        daysa5 <- c();
          diarydeaths <- c();
             somadeaths_i = 0;

    # Total daily deaths from owd, aligned  since 5th death
    while( k <= N-1 ) {

    if( is.na(dataowd5_days[k, 10] ) == TRUE ) {
        dataowd5_days[k, 10] <- 0;
    }# End if

      if( dataowd5_days[k, 9] == dataowd5_days[k+1, 9] ) {
         if( dataowd5_days[k, 3] != "" && is.na(dataowd5_days[k, 10]) != TRUE ) {
             somadeaths_i <- somadeaths_i + dataowd5_days[k, 10];
          }
          i = i+1;
          
          
      }else {
         daysa5 <- c(daysa5, i)
         if( dataowd5_days[k, 3] != "" && is.na(dataowd5_days[k, 10] ) != TRUE) {
             somadeaths_i <- somadeaths_i+ dataowd5_days[k, 10];
             diarydeaths <- c(diarydeaths, somadeaths_i);
         }

          i = 1;
       
      }# End if

          k = k+1;

    } # End k while  
    print( "Total deaths" );
      print( c(ll, somadeaths_i) );

    daysa5 <- c(daysa5, i);
      somadeaths_i = somadeaths_i + dataowd5_days[k, 10];
        diarydeaths <- c(diarydeaths, somadeaths_i);

    ftsize = 1.3;
    png( str_c(homedir, homeCounEvol, "Pandemic_", date_analysis, ".png"), width = 500, height = 500 )
        
        mypar(); 
        par( new = FALSE );
        par( mar = c(4, 4, 4, 4) );
   
        plot( daysa5, main = str_c("Covid-19 Pandemic in ", date_analysis), 
              ylab = "Number of Countries", 
              xlab = "Days since five deaths", 
              cex.main = ftsize, cex.lab = ftsize, cex.axis = ftsize, 
              type = "l", 
              lty = 1 
             ); 
        grid( lty = 3, lwd = 1 );
    
    dev.off();


    Nds = max( daysa5 );
    Ndays = c(1:Nds);

    # Percents occurrences of blood types in countries  
    #namesdatac = rownames(data_c);
    namesdatac = rownames(data_c_percents); #using data from owd
      dimnamesdatac = dim(as.matrix(namesdatac));

    # Aux matrix with  i days since the 5th death
    aux <- c();
      aux2 <- c();
        vet_ones <- c(1, 1, 1, 1);
          covid <- c();
            cumulative_covid <- c();

    correlacoes <- c();      correlacoest <- c();      correlacoest_CI <- c();
    correlacoes_rhp <- c();  correlacoest_rhp <- c();  correlacoest_CI_rhp <- c();
    correlacoes_rhn <- c();  correlacoest_rhn <- c();  correlacoest_CI_rhn <- c();
    correlacoes_O <- c();    correlacoest_O <- c();    correlacoest_CI_O <- c();
    correlacoes_A <- c();    correlacoest_A <- c();    correlacoest_CI_A <- c();
    correlacoes_B <- c();    correlacoest_B <- c();    correlacoest_CI_B <- c();
    correlacoes_AB <- c();   correlacoest_AB <- c();   correlacoest_CI_AB <- c();

    p_values_histogram <- c();
    Wstatistic_histogram <- c();

    maxk = 0;
    cores <- c("aquamarine3", "firebrick1", "darkviolet", "darkorange3", "gold4", 
         "darkblue", "green", "darkslategray1", "deepskyblue", "darkseagreen");
         
    covid_i <- c();
    coeff_ll <- c(); #Analysis coefficients 

                 
# --------------- N_i- total days since 5th death ---------------
#i - numbers days since 5th death 
 for( i in c(1:N_i) ) {
    print("Analysis, Days Since 5:");
      print( c(ll, i) );  
  
    covid <- c();
     covid_r <- c();
        aux   <- c();
        aux2 <- c();
         vars_pca <- c();
           names_aux2 <- c();
             aux2_percents <- c();
    maxk <- maxk + daysa5[i];

    l = 1;
    # k- numbers of countries with i days since 5th death
    for( k in c(maxk - daysa5[i]+1: daysa5[i]) ) {
    
        aux <- rbind( aux, dataowd5_days[k, ] );
        l = l+1;
    
    }
    l = l-1;
    j = 1;
    somacovid = 0;

    # Assembling countries and deaths from owd data
    # k - countries with i days since 5th death
    for( k in c(1:l) ) {
       
       m = 1;
       teste <- FALSE
       while( aux[k, 4]!= namesdatac[m] && teste == FALSE ) {
          m = m+1;
          if( m == dimnamesdatac[1] ) {
             teste <- TRUE;
          }
       }
    

      if( aux[k, 4] == namesdatac[m] && is.na( aux[k, 8] ) == FALSE ) {
      # alteração para todos os países: if( is.na(aux[k, 8]) == FALSE) {

         #aux2[j, ] = datac[l, ]
          #using population from rhresus site
            #aux2 <- rbind( aux2, data_c[m, 1:9] );
          
          #using population from owd
              #aux2_percents <- rbind( aux2_percents, c(aux[k, 28], data_c_percents[m, 2:9]*aux[k, 28]) ); 
              aux2 <- rbind( aux2, c( as.numeric(aux[k, "population"]), 
                                      as.numeric(data_c_percents[m, 2:9])* as.numeric(aux[k, "population"]) )); 
              

              #---------------------------- P C A ------------------------------
              #
               varnames <-  c( "population", "population_density", "median_age", "aged_65_older", "aged_70_older", 
                               "gdp_per_capita", "extreme_poverty", "cardiovasc_death_rate", "diabetes_prevalence", 
                               "hospital_beds_per_thousand", "life_expectancy", "human_development_index") #, 
                           #   "people_fully_vaccinated_per_hundred" );
               vars_pca <- rbind( vars_pca, c( as.numeric(aux[k, varnames]) ) );
              #
              #
              #cols: 38-stringency_index, 46-Populatioons, 40-Populations_density, 41- 42-43-444-45-, 
              #      46-cardiovasc_death_rate, 47-diabetes_prevalence, 
              #      51-hospital_beds_per_thousand, 52-life_expectancy, 
              #      53-human_development_index
              #-----------------------------------------------------------------
                
              names_aux2 <- rbind( names_aux2, aux[k, 4] );
              #aux2 <-  aux2_percents;  
       
          covid_r <- cbind( covid_r, as.matrix(aux[k, 8]) );
               
          if( logdata == 1 ) {
             covid <- cbind( covid, log(as.matrix(aux[k, 8])) );
             
          }else {
             covid <- cbind( covid, as.matrix(aux[k, 8]) );
          }   
             
          somacovid <- somacovid + aux[k, 10];
          #print(somacovid);
          
          j = j+1;
       } 
      
    } #end for k<-1:l

    rownames( aux2 ) <- names_aux2;
    colnames( aux2 ) <- names_col_aux2[2:10];
  
    if( pca_expanded == 1 ) {                         
         colnames( vars_pca ) <- varnames;

         #colnames(vars_pca) <- c( "population", "population_density");#, "median_age", "aged_65_older", "aged_70_older", 
         #                         #"gdp_per_capita", "extreme_poverty", "cardiovasc_death_rate", "diabetes_prevalence", 
         #                         #"female_smokers", "male_smokers", "handwashing_facilities", "hospital_beds_per_thousand", 
         #                         #"life_expectancy", "human_development_index", "excess_mortality" );
         #using population from owd need log aux2

         dim_vars = dim(vars_pca) 
       
         aux2 = cbind(aux2, vars_pca[, 2:dim_vars[2]]);
       
         colnames( aux2 ) <- c(  names_col_aux2[2:10], colnames( vars_pca[, 2:dim_vars[2]] )  );
    }

    print( "Dimension aux2" );
      print( dim(aux2) );
        print( "Dimension covid" )
          print( dim(covid) )
  
     # Exclude countries from data
      if( outcountry != "" ) {
        if( i >= 120 ) {
            dimaux2_antes <- dim( aux2 );
            print( "Excluding countries" )
            aux2 <- discontinuity( aux2, covid, outcountry );
            dimaux2_depois <- dim( aux2 );
         
            if( dimaux2_antes[2] != dimaux2_depois[2] ) {
               dimaux2 <- dim( aux2 );
               covid <- aux2[, dimaux2[2]]
               aux2 <- aux2[, 1:dimaux2[2]-1]
               covid <- t( covid )
            }
        }
     } # End if outcountry
     
     aux2_r <- aux2;  
        
        if( logdata == 1 ) {
              aux2 <- log( aux2 );
        }
   
       #rownames(aux2) <- names_aux2;
       #colnames(aux2) <- c(names_col_aux2[2:10], colnames(vars_pca[, 2:dim_vars[2]]));

     covid_i[i] <- somacovid;
        
     aux3 <- cbind( aux2, t(covid) );
       aux3_r <- cbind( aux2_r, t(covid_r) );
    
     aux3 <- na.omit(aux3);
       aux3_r <- na.omit(aux3_r);
       
     aux_covid <- covid;
    
     aux3  <- aux3[!is.infinite( rowSums(aux3) ), ];
      aux3_r  <- aux3_r[!is.infinite( rowSums(aux3_r) ), ];
      
    
     dim_aux2 <- dim(aux2)

     aux2  <- aux3[, 1:(dim_aux2[2])];   #aux3 = [ Pop, ABO+, ABO-, <6VARS>, covid] 
      aux2_r <- aux3_r[, 1:(dim_aux2[2])];
 
     covid   <- aux3[, dim_aux2[2]+1];
     covid_r <- aux3_r[, dim_aux2[2]+1]; # original data
      

     if( datascale == 0 ) {
        covid <-  as.numeric( covid ) ;
        covid_r <-  as.numeric( covid_r ) ;
        aux2  <-  as.matrix( aux2 ) ;
     }else {
        covid   <- scale( as.numeric(covid) );
        covid_r   <- scale( as.numeric(covid_r) );
        aux2    <- scale( as.matrix(aux2) );
        aux2_r  <- scale( as.matrix(aux2_r) ); 
     }
    
     # All blood types  analysis: { O+, A+, B+, AB+,   O-, A-, B-, AB-, Rh+, Rh-} 
     # Correlations for i days since 5 deaths
    
     aux_covid <- c()
     crow_c <- order( covid )
     aux_covid <- covid
    
     covid <- covid[crow_c];
     cumulative_covid <- rbind( cumulative_covid, sum(covid) );
     cumulative_expcovid <- rbind( cumulative_covid, sum(exp(covid)) );
    
     aux2 <- aux2[crow_c, ];
          aux2_r <- aux2_r[crow_c, ];
     rownames(aux2)
     correl_1 <- cor( aux2, covid );
    
     correltest_1 <- c();
      correltest_CI_1 <- c();
     
      for( kk in c(1:9) ) {
      
        param <- c();
        param <- correlations_f( i, ll, aux2[, kk], covid );

        correl_1 [kk] <- param[1];
           correltest_1 [kk] <- param[2];
              correltest_CI_1 <- cbind( correltest_CI_1, t(c(param[3], param[4])) );
       }
      

      correlacoes <- rbind ( correlacoes, t(correl_1) );
         correlacoest <- rbind ( correlacoest, t(correltest_1) );
            correlacoest_CI <- rbind ( correlacoest_CI, t(correltest_CI_1 ) );# Confidence interval
     
      
      # ABO analysis 
      # atention log(a+b) ~= loga + log b = log a*b
      # example: log(aux2_r[, 1:9] %*% c(0, 1, 0, 0, 0, 1, 0, 0, 0)
    
      if( logdata == 1 ) { 
    
           param <- c();
           param <- correlations_f( i, ll, log(aux2_r[, 1:9] %*% c(0, 1, 0, 0, 0, 1, 0, 0, 0)), covid );
             
           correl_O <- param[1];
              correltest_O <- param[2];
                 correltest_CI_O <- c(param[3], param[4]);

           correlacoes_O <- rbind( correlacoes_O, t(correl_O) );
              correlacoest_O <-rbind( correlacoest_O, t(correltest_O) );
                 correlacoest_CI_O <- rbind ( correlacoest_CI_O, t(correltest_CI_O) )
          
                 
          param <- c();
          param <- correlations_f( i, ll, log(aux2_r[, 1:9] %*% c(0, 0, 1, 0, 0, 0, 1, 0, 0)), covid);               
          
          correl_A <- param[1];
             correltest_A <- param[2];
                correltest_CI_A <- c(param[3], param[4]);
          
          correlacoes_A <- rbind( correlacoes_A, t(correl_A) );
             correlacoest_A <- rbind( correlacoest_A, t(correltest_A) );
                correlacoest_CI_A <- rbind ( correlacoest_CI_A, t(correltest_CI_A) );

          
          param <- c();
          param <- correlations_f( i, ll, log(aux2_r[, 1:9] %*% c(0, 0, 0, 1, 0, 0, 0, 1, 0)), covid );
          
          correl_B <- param[1];
             correltest_B <- param[2];
                correltest_CI_B <- c(param[3], param[4]);

          correlacoes_B <- rbind( correlacoes_B, t(correl_B) );
             correlacoest_B <- rbind( correlacoest_B, t(correltest_B) );
                correlacoest_CI_B <- rbind ( correlacoest_CI_B, t(correltest_CI_B) );
           
 
          param <- c();
          param <- correlations_f( i, ll, log(aux2_r[, 1:9] %*% c(0, 0, 0, 0, 1, 0, 0, 0, 1)), covid );
          
          correl_AB <- param[1];
             correltest_AB <- param[2];
                correltest_CI_AB <- c(param[3], param[4]);
          
          correlacoes_AB <- rbind( correlacoes_AB, t(correl_AB) );
             correlacoest_AB <- rbind( correlacoest_AB, t(correltest_AB) );
                correlacoest_CI_AB <- rbind ( correlacoest_CI_AB, t(correltest_CI_AB) );

    
     # Rh factor analysis
        
          param <- c();
          param <- correlations_f( i, ll, log(aux2_r [, 2:5]  %*% vet_ones), covid );
          
          correl_rhp <- param[1];
             correltest_rhp <- param[2];
                correltest_CI_rhp <- c(param[3], param[4]);
          
          correlacoes_rhp <- rbind( correlacoes_rhp, t(correl_rhp) );
             correlacoest_rhp <- rbind( correlacoest_rhp, t(correltest_rhp) );
                correlacoest_CI_rhp <- rbind ( correlacoest_CI_rhp, t(correltest_CI_rhp) );
           
          param <- c();
          param <- correlations_f( i, ll, log(aux2_r[, 6:9]  %*% vet_ones), covid );
          
          correl_rhn <- param[1];
             correltest_rhn <- param[2];
                correltest_CI_rhn <- c(param[3], param[4]);

          correlacoes_rhn  <- rbind( correlacoes_rhn, t(correl_rhn) );
             correlacoest_rhn <- rbind( correlacoest_rhn, t(correltest_rhn) );
                correlacoest_CI_rhn <- rbind ( correlacoest_CI_rhn, t(correltest_CI_rhn) );


      
     }else { # Original data (without log transformations)
       param <- c();
       param <- correlations_f( i, ll, (aux2[, 1:9] %*% c(0, 1, 0, 0, 0, 1, 0, 0, 0)), covid );
       
       correl_O <- param[1];
       correltest_O <- param[2];
       correltest_CI_O <- c(param[3], param[4]);
       
       correlacoes_O <- rbind( correlacoes_O, t(correl_O) );
       correlacoest_O <-rbind( correlacoest_O, t(correltest_O) );
       correlacoest_CI_O <- rbind ( correlacoest_CI_O, t(correltest_CI_O) )
       
       
       param <- c();
       param <- correlations_f( i, ll, (aux2[, 1:9] %*% c(0, 0, 1, 0, 0, 0, 1, 0, 0)), covid);               
       
       correl_A <- param[1];
       correltest_A <- param[2];
       correltest_CI_A <- c(param[3], param[4]);
       
       correlacoes_A <- rbind( correlacoes_A, t(correl_A) );
       correlacoest_A <- rbind( correlacoest_A, t(correltest_A) );
       correlacoest_CI_A <- rbind ( correlacoest_CI_A, t(correltest_CI_A) );
       
       
       param <- c();
       param <- correlations_f( i, ll, (aux2[, 1:9] %*% c(0, 0, 0, 1, 0, 0, 0, 1, 0)), covid );
       
       correl_B <- param[1];
       correltest_B <- param[2];
       correltest_CI_B <- c(param[3], param[4]);
       
       correlacoes_B <- rbind( correlacoes_B, t(correl_B) );
       correlacoest_B <- rbind( correlacoest_B, t(correltest_B) );
       correlacoest_CI_B <- rbind ( correlacoest_CI_B, t(correltest_CI_B) );
       
       
       param <- c();
       param <- correlations_f( i, ll, (aux2[, 1:9] %*% c(0, 0, 0, 0, 1, 0, 0, 0, 1)), covid );
       
       correl_AB <- param[1];
       correltest_AB <- param[2];
       correltest_CI_AB <- c(param[3], param[4]);
       
       correlacoes_AB <- rbind( correlacoes_AB, t(correl_AB) );
       correlacoest_AB <- rbind( correlacoest_AB, t(correltest_AB) );
       correlacoest_CI_AB <- rbind ( correlacoest_CI_AB, t(correltest_CI_AB) );
       
       
       # Rh factor analysis
       
       param <- c();
       param <- correlations_f( i, ll, (aux2 [, 2:5]  %*% vet_ones), covid );
       
       correl_rhp <- param[1];
       correltest_rhp <- param[2];
       correltest_CI_rhp <- c(param[3], param[4]);
       
       correlacoes_rhp <- rbind( correlacoes_rhp, t(correl_rhp) );
       correlacoest_rhp <- rbind( correlacoest_rhp, t(correltest_rhp) );
       correlacoest_CI_rhp <- rbind ( correlacoest_CI_rhp, t(correltest_CI_rhp) );
       
       param <- c();
       param <- correlations_f( i, ll, (aux2[, 6:9]  %*% vet_ones), covid );
       
       correl_rhn <- param[1];
       correltest_rhn <- param[2];
       correltest_CI_rhn <- c(param[3], param[4]);
       
       correlacoes_rhn  <- rbind( correlacoes_rhn, t(correl_rhn) );
       correlacoest_rhn <- rbind( correlacoest_rhn, t(correltest_rhn) );
       correlacoest_CI_rhn <- rbind ( correlacoest_CI_rhn, t(correltest_CI_rhn) );
    
} # End if data with log transformations

      
#---------------------- Out results for each day i -------------------

    if( pca_expanded == 1 ) {
      
        if( ll == 1 | ll == 3 | ll == 4) {
           source( str_c(homedir, "aux4pca.R") );
        }
    
    }
      
    source( str_c(homedir, "coefficients.R") );

# Pictures of log blood types from countries and deaths (covid vector)  
    if( i == 120 ) {
        if( logdata == 1 ) {
            source( str_c(homedir, "plot_bloodtypes.R") );
        }else {
        }        
    }
        
# Pictures of the pandemic evolution used to make video   

    source( str_c(homedir, "movie_covid.R") );  
    
# Normality test for each i day

    source( str_c(homedir, "histograms_tests.R") );            

# Evolutions of covid ordered for each day.
    
    source( str_c(homedir, "covid_evolution.R") );   
   
      
 } # End loop i for N_i days since 5a death.

        
# --------- Data outputs for each analysis ll ---------- 
        
# Coefficientts of the  Covid-19 analysis 
        
    source( str_c(homedir, "coeffcovid19.R") );   

# Cumulative_covid
        
    source( str_c(homedir, "plotcumulativecovid.R") );

# Saving correlations files
        
    print("Gravando arquivos");
    source( str_c(homedir, "correlation_outputs.R") );
   
# Data Normality
        
    source( str_c(homedir, "evolution_normality.R") );
   
# Temporal Correlations Plots 
        
    source( str_c(homedir, "plotcorrelations.R") ); 
  
# P_values plots 
        
    source( str_c(homedir, "plotpvalues.R") );  

# Daily deaths matrix
        
    covid_ll <- cbind( covid_ll, covid_i );

  
} # End  ll loop (countries sets)
   
  print( "End loop ll" );

# Daily deaths plots - valid for N_i > 35
  
 if( N_i > 35 ) {
   
     source( str_c(homedir, "daily_deaths.R") ); 
   
 }
 
# Normality plots 
  
#   source( str_c(homedir, homenorm, "evolution_normality_all.R" ) );

# Max and min values of correlations 
  
   maxmin_values_f( homedir, homecsv, N_i, nx, vfilesout ); 
   
# Comparative graphics of pvalues and correlations 
   
   source( str_c(homedir, "pvalues_dif.R") ); 

# Fit polinomial  correlations
   
   source( str_c(homedir, "fitting_correlations.R") );

   
print( "The program finished!" )
#*----------------------------------------------------------------------------*#   

