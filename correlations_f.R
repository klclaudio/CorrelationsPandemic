#
# This function calculates correlations between vet_test and covid vectors. 
#
# i- days since 5th deaths
# ll- set of countries 

correlations_f <- function( i, ll, vet_test, covid ) {

  auxcor_abo <- cor.test( vet_test , covid, method =  method_correl, exact=errors_correl, use = "complete_obs" ); # O
  correl_abo <- auxcor_abo$estimate;
    correltest_abo <- auxcor_abo$p.value;

  
  if ( auxcor_abo$p.value > 0.05 )  {  
    
     correltest_CI_abo <- c(0.0, 0.0); # Confidence interval
     
  }else {
    
     correltest_CI_abo <- auxcor_abo$conf.int[1:2];
     
  }
    
  param <- c(correl_abo, correltest_abo, correltest_CI_abo)
  
  param <-c();
  
  param <- cbind(param, correl_abo);
  param <- cbind(param, correltest_abo);
  param <- cbind(param, t(correltest_CI_abo));

  return(param)
} # End correlations
