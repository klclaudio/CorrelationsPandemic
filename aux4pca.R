#
# File to call function pca_analysis - pca_analysis( covid_frame,i,ll,dir )
#

aux_covid_i <- c();
  covid_frame  <- c();                   
             
 if( pca_expanded == 0 ) {
   
      aux_covid_i = cbind( aux2[,1:9], covid );
      
    }else {
      
       aux_covid_i = cbind( aux2[,1:9], aux2[,10:dim_aux2[2]], covid ); # vars: Diabetics, Hearths, IDH ...
 }# End if
 
 # colnames(aux_covid_i) <- col_names_aux2;
    covid_frame <- as.data.frame( aux_covid_i );
 
 # dirpca <- str_c( homedir, homepca,"analise" );
    write.csv( covid_frame, str_c(homedir, homepca, ll, "/pca_", i, "_", ll, ".csv") );
 
 # sprintf("%.48f",covid_frame

    print( "Calling pca_analysis function" )
    result <- pca_analysis( covid_frame, i, ll, str_c(homedir, homepca) );


 if( i == 250 | i == 150 | i == 100 | i == 50 | i == 25 ) {

      corr <- round( cor(aux_covid_i),2 );
        p.mat <- cor_pmat( aux_covid_i );
     
      ggcorrplot( corr,type = "lower", hc.order = FALSE , lab = TRUE,  p.mat = p.mat );
        ggsave(str_c( homedir, homepca, ll, "/Corr_Pvalues_Matplot_", ll, "_", i, ".png"), width = 12, height = 12 );
     
      result <- pca_analysis( covid_frame, i, ll, str_c(homedir, homepca) );
  
 }# End if

# End aux4pca