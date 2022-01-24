#saidas para arquivos .csv   
 
mat_correl_csv <- c(); # matriz de correlaçoes rmat_correl_csv <- c(); # matriz de correlaçoes referentes aos dias avaliados após a 5a ocorrência
mat_ci_csv <- c();
auxmats <- c();
 
 #correlaçoes do sistema ABO com distinção dos fatores Rh
 for (jj in c(1:9)) {
      mat_correl_csv <- cbind( mat_correl_csv, correlacoes[,jj]);
      mat_correl_csv <- cbind( mat_correl_csv, correlacoest[,jj]);
      mat_ci_csv <- cbind( mat_ci_csv,  correlacoest_CI[(N_i*(jj-1)+1) :(N_i*jj),] );
 }

      #correlaçoes do sistema ABO sem distinção dos fatores Rh
      mat_correl_csv <- cbind(mat_correl_csv,correlacoes_O);
      mat_correl_csv <- cbind(mat_correl_csv,correlacoest_O);
      mat_ci_csv <- cbind (mat_ci_csv, correlacoest_CI_O);
      
      mat_correl_csv <- cbind(mat_correl_csv,correlacoes_A);
      mat_correl_csv <- cbind(mat_correl_csv,correlacoest_A);
      mat_ci_csv <- cbind (mat_ci_csv, correlacoest_CI_A);
      
      mat_correl_csv <- cbind(mat_correl_csv,correlacoes_B);
      mat_correl_csv <- cbind(mat_correl_csv,correlacoest_B);
      mat_ci_csv <- cbind (mat_ci_csv, correlacoest_CI_B);
      
      mat_correl_csv <- cbind(mat_correl_csv,correlacoes_AB);
      mat_correl_csv <- cbind(mat_correl_csv,correlacoest_AB);
      mat_ci_csv <- cbind (mat_ci_csv, correlacoest_CI_AB);
      
      #correlaçoes dos fatores RH
      mat_correl_csv <- cbind(mat_correl_csv,correlacoes_rhp);
      mat_correl_csv <- cbind(mat_correl_csv,correlacoest_rhp);
      mat_ci_csv <- cbind (mat_ci_csv, correlacoest_CI_rhp);
      
      mat_correl_csv <- cbind(mat_correl_csv,correlacoes_rhn);
      mat_correl_csv <- cbind(mat_correl_csv,correlacoest_rhn);
      mat_ci_csv <- cbind (mat_ci_csv, correlacoest_CI_rhn);
     
     colnames( mat_correl_csv) =
       c("Pop.","Pvalue", "O.", "Pvalue", "A.", "Pvalue", "B.", "Pvalue", "AB.", "Pvalue", "O..1", "Pvalue", "A..1", "Pvalue", "B..1", "Pvalue","AB..1","Pvalue",
         "Correl_O","Pvalue_O","Correl_A","Pvalue_A","Correl_B","Pvalue_B","Correl_AB","Pvalue_AB",
         "Correl_rhp","Pvalue_rhp","Correl_rhn","Pvalue_rhn");

         
     colnames( mat_ci_csv) =
       c("InfPop", "SupPop", "InfO.", "SupO.", "InfA.", "SupA.", "InfB.", "SupB", "InfAB.", "SupAB", "InfO..1", "SupO..1",
         "InfA..1", "SupA..1", "InfB..1", "SupB..1","InfAB..1","SupAB..1", 
         "Inf_O","Sup_O","Inf_A","Sup_A","Inf_B","Sup_B","Inf_AB","Sup_AB",
         "Inf_rhp","Sup_rhp","Inf_rhn","Sup_rhn");

              
     write.csv(mat_correl_csv, fileout);
     write.csv(mat_ci_csv, str_c(homedir,homecsv,"CI",type_stat,"_",ll,".csv") );
 
 
 mat_hist_csv <- c();
     
     mat_hist_csv <- cbind( mat_hist_csv, p_values_histogram );
     mat_hist_csv <- cbind( mat_hist_csv, Wstatistic_histogram );
     colnames( mat_hist_csv) <- c( "Pvalues","Wstatistics" );
     
     write.csv( mat_hist_csv, str_c(homedir,homecsv,"hist_test_",ll,".csv") );

     
     
 #Análise geral da pandemia y(x,t)=b(t)*exp(a(t)*x)  Graficos de dispersão tipos sanguíneos e covid
     auxmats =  cbind( auxmats, coeff_a, coeff_lin[,2], errors_a[,2], errors_lin[,2], r2[,2]);
     mat_coeffs_csv  = rbind( mat_coeffs_csv, auxmats); 
     #teste2 = cbind( teste2, coeff_a, coeff_lin[,2], errors_a[,2], errors_lin[,2], r2[,2]); 
     write.csv(  auxmats, str_c(homedir,homecsv,"coeff_t_BlodTypeCOVID19.csv") );
     
     
     
 #Análise geral da pandemia y(x,t)=b(t)*exp(a(t)*x)  -  a e b   covid
     write.csv( coeff_aux, str_c(homedir,homecsv,"coeff_t_COVID19.csv") );
   
   
 #Mínimos quadrados das curvas a(t) e b(t)
     if (ll==ndata) {
        mat_coefilin_csv <- rbind( mat_coefilin_csv, c(ll, coefilin) );
            mat_statslin_csv <- rbind( mat_statslin_csv, c(ll, stats_residualslin) );
                mat_stderrlin_csv <- rbind( mat_stderrlin_csv, c(ll, stderrorslin) );
        
        write.csv( mat_coefilin_csv, str_c(homedir,homecsv,"stats_coefficientesCovid19_LOG_lin.csv") );
        write.csv( mat_statslin_csv, str_c(homedir,homecsv,"stats_residualsCovid19_LOG_lin.csv") );
        write.csv( mat_stderrlin_csv,str_c(homedir,homecsv,"stats_errorsCovid19_LOG_lin.csv") );
        
         mat_coefia_csv <- rbind( mat_coefia_csv, c(ll, coefia) );
            mat_statsa_csv <- rbind( mat_statsa_csv, c(ll, stats_residualsa) );
                mat_stderra_csv <- rbind( mat_stderra_csv, c(ll, stderrorsa) );
            

        write.csv( mat_coefia_csv, str_c(homedir,homecsv,"stats_coefficientesCovid19_LOG_a.csv") );
        write.csv( mat_statsa_csv, str_c(homedir,homecsv,"stats_residualsCovid19_LOG_a.csv") );
        write.csv( mat_stderra_csv,str_c(homedir,homecsv,"stats_errorsCovid19_LOG_a.csv") );
        
     }else{
     
        mat_coefilin_csv <- rbind( mat_coefilin_csv, c(ll, coefilin) );
            mat_statslin_csv <- rbind( mat_statslin_csv, c(ll, stats_residualslin) );
                mat_stderrlin_csv <- rbind( mat_stderrlin_csv, c(ll, stderrorslin) );
        
       
         mat_coefia_csv <- rbind( mat_coefia_csv, c(ll, coefia) );
            mat_statsa_csv <- rbind( mat_statsa_csv, c(ll, stats_residualsa) );
                mat_stderra_csv <- rbind( mat_stderra_csv, c(ll, stderrorsa) );
     
     }

