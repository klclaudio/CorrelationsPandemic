#
# This function calculates correlations
#
correlations_f <- function( abo_vector, covid ) {

   auxcor_abo     <- cor.test( abo_vector,
                               covid,
                               method = method_correl,
                               exact  = errors_correl,
                               use    = "complete_obs" ) # O
   correl_abo     <- auxcor_abo$estimate
   correltest_abo <- auxcor_abo$p.value

   if (auxcor_abo$p.value > 0.05) {
      correltest_CI_abo <- c(0.0, 0.0) # Confidence interval
   }else {
      correltest_CI_abo <- auxcor_abo$conf.int[1:2]
   }

   param <- c()
   param <- cbind(param, correl_abo)
   param <- cbind(param, correltest_abo)
   param <- cbind(param, t(correltest_CI_abo))

   return(param)
} # End correlations_f
