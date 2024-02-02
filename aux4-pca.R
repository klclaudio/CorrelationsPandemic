#
# File to call function pca_analysis - pca_analysis( covid_frame,i_days,l_count,dir )#
aux_covid_i  <- c()
covid_frame  <- c()

if( pca_expanded == 0 ) {
      aux_covid_i = cbind( aux2[,1:9], covid )
   }else {
   aux_covid_i = cbind( aux2[,1:9], aux2[,10:dim_aux2[2]], covid ) # vars: Diabetics, Hearths, IDH ...
 }

# colnames(aux_covid_i) <- col_names_aux2
covid_frame <- as.data.frame( aux_covid_i )

# dirpca <- str_c( homedir, homepca,"analise" )
# write.csv( covid_frame, str_c(homedir, homepca, l_count, "/pca_", i_days, "_", l_count, ".csv") )

# sprintf("%.48f",covid_frame

# print( "Calling pca_analysis function" )
# result <- pca_analysis( covid_frame, i_days, l_count, str_c(homedir, homepca) )

if(i_days == 250 || i_days == 150 || i_days == 100 || i_days == 50 || i_days == 25) {
   corr  <- cor(aux_covid_i)
   p.mat <- cor_pmat(aux_covid_i)

   print(str_c(homedir, homepca, "matcorr_", i_days, "_", l_count, ".csv"))
   write.csv( corr, str_c(homedir, homepca,"matcorr_", i_days, "_", l_count, ".csv") )

   corr <- round(corr, 2)
   ggcorrplot( corr, type = "lower", hc.order = FALSE , lab = TRUE, p.mat = p.mat )
   ggsave( str_c(homedir, homepca, "Corr_Pvalues_Matplot_", l_count, "_", i_days, ".png"), width = 12, height = 12 )

   # PCA Analysis
   result <- pca_analysis_f( covid_frame,  i_days, l_count, str_c(homedir, homepca) )
 }
# End aux4pca