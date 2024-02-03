#
# Correlation outputs  saves .csv files  (correlations and confidence intervals)
#
mat_correl_csv <- c()
mat_ci_csv     <- c()
auxmats        <- c()

# ABO correlations data with Rh factors
for( j_count in c(1:9) ) {
   mat_correl_csv <- cbind( mat_correl_csv, correlacoes [, j_count] )
   mat_correl_csv <- cbind( mat_correl_csv, correlacoest[, j_count] )
   mat_ci_csv     <- cbind( mat_ci_csv, correlacoest_CI [, 2 * j_count -1] )
   mat_ci_csv     <- cbind( mat_ci_csv, correlacoest_CI [, 2 * j_count] )
}
# ABO correlations  data without Rh factors

  for( j_count in c(1:6) ) {
     mat_correl_csv <- cbind( mat_correl_csv, correlacoes_abo_rh[, j_count] )
     mat_correl_csv <- cbind( mat_correl_csv, correlacoest_abo_rh[, j_count] )
     mat_ci_csv     <- cbind( mat_ci_csv, correlacoest_CI_abo_rh [, 2 * j_count - 1] )
     mat_ci_csv     <- cbind( mat_ci_csv, correlacoest_CI_abo_rh [, 2 * j_count] )
  }

colnames( mat_correl_csv) = c( "Pop.", "Pvalue_Pop",
                               "O+" , "Pvalue_O+",
                               "A+" , "Pvalue_A+",
                               "B+" , "Pvalue_B+",
                               "AB+", "Pvalue_AB+",
                               "O-" , "Pvalue_O-",
                               "A-" , "Pvalue_A-",
                               "B-" , "Pvalue_B-",
                               "AB-", "Pvalue_AB-",
                               "O"  , "Pvalue_O",
                               "A"  , "Pvalue_A",
                               "B"  , "Pvalue_B",
                               "AB" , "Pvalue_AB",
                               "Rhp", "Pvalue_Rhp",
                               "Rhn", "Pvalue_Rhn" )

colnames( mat_ci_csv) = c( "Inf_Pop"  , "Sup_Pop",
                           "Inf_O+"   , "Sup_O+",
                           "Inf_A+"   , "Sup_A+",
                           "Inf_B+"   , "Sup_B+",
                           "Inf_AB+"  , "Sup_AB+",
                           "Inf_O-"  , "Sup_O-1",
                           "Inf_A-"  , "Sup_A-1",
                           "Inf_B-"  , "Sup_B-1",
                           "Inf_AB-" , "Sup_AB-1",
                           "Inf_O"    , "Sup_O",
                           "Inf_A"    , "Sup_A",
                           "Inf_B"    , "Sup_B",
                           "Inf_AB"   , "Sup_AB",
                           "Inf_Rhp"  , "Sup_Rhp",
                           "Inf_Rhn"  , "Sup_Rhn" )

write.csv(mat_correl_csv, fileout )
write.csv(mat_ci_csv, str_c(homedir, homecsv, "CI", type_stat, "_", l_count,".csv") )

mat_hist_csv            <- c()
mat_hist_csv            <- cbind( mat_hist_csv, p_values_histogram )
mat_hist_csv            <- cbind( mat_hist_csv, Wstatistic_histogram )
colnames( mat_hist_csv) <- c( "Pvalues","Wstatistics" )
write.csv( mat_hist_csv, str_c(homedir, homecsv, "hist_test_", l_count, ".csv") )

# General analyses  y(x,t)=b(t)*exp(a(t)*x)  Graficos de dispersão tipos sanguíneos e covid
auxmats         <- cbind( auxmats, coeff_a, coeff_lin[,2], errors_a[,2], errors_lin[,2], r2[,2] )
mat_coeffs_csv  <- rbind( mat_coeffs_csv, auxmats )

write.csv( auxmats, str_c(homedir, homecsv, "coeff_t_blodtype_covid19.csv") )

# General analyses y(x,t)=b(t)*exp(a(t)*x)  -  a e b   covid
write.csv( coeff_aux, str_c(homedir, homecsv, "coeff_t_covid19.csv") )

# Least squares from a(t) e b(t)
if( l_count == ndata ) {
   mat_coefilin_csv  <- rbind( mat_coefilin_csv,  c(l_count, coefilin) )
   mat_statslin_csv  <- rbind( mat_statslin_csv,  c(l_count, stats_residualslin) )
   mat_stderrlin_csv <- rbind( mat_stderrlin_csv, c(l_count, stderrorslin) )

   write.csv( mat_coefilin_csv , str_c(homedir, homecsv, "stats_coefficientes_covid19_LOG_lin.csv") )
   write.csv( mat_statslin_csv , str_c(homedir, homecsv, "stats_residuals_covid19_LOG_lin.csv") )
   write.csv( mat_stderrlin_csv, str_c(homedir, homecsv, "stats_errors_covid19_LOG_lin.csv") )

   mat_coefia_csv  <- rbind( mat_coefia_csv,  c(l_count, coefia) )
   mat_statsa_csv  <- rbind( mat_statsa_csv,  c(l_count, stats_residualsa) )
   mat_stderra_csv <- rbind( mat_stderra_csv, c(l_count, stderrorsa) )

   write.csv( mat_coefia_csv, str_c(homedir, homecsv, "stats_coefficientes_covid19_LOG_a.csv") )
   write.csv( mat_statsa_csv, str_c(homedir, homecsv, "stats_residuals_covid19_LOG_a.csv") )
   write.csv( mat_stderra_csv,str_c(homedir, homecsv, "stats_errors_covid19_LOG_a.csv") )
}else {
   mat_coefilin_csv  <- rbind( mat_coefilin_csv,  c(l_count, coefilin) )
   mat_statslin_csv  <- rbind( mat_statslin_csv,  c(l_count, stats_residualslin) )
   mat_stderrlin_csv <- rbind( mat_stderrlin_csv, c(l_count, stderrorslin) )

   mat_coefia_csv  <- rbind( mat_coefia_csv,  c(l_count, coefia) )
   mat_statsa_csv  <- rbind( mat_statsa_csv,  c(l_count, stats_residualsa) )
   mat_stderra_csv <- rbind( mat_stderra_csv, c(l_count, stderrorsa) )
} #end if

# End correlation_outputs
