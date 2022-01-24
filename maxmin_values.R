
# Entradas para execuatar arquivo em separado

library (tidyverse);
library (cluster);
library (dplyr);
library (factoextra);
library (gridExtra);
library (rafalib);
# corrplot
library (zoo);
library (ggcorrplot);
library (corrplot);
library (GGally);
library (stringr);
#diretórios de trabalho e resultados
homedir <- "/home/klclaudio/Documents/Analises2021_10/";
homepca  <-  "Results/PCA/";
homedaily <- "Results/Daily/";
homecsv <- "Results/CSV/";
homefit <- "Results/Fitting/";
homeCounEvol <- "Results/CountriesEvolutions/";
homebld <- "Results/BloodTypes/";
homemov <- "Results/Movies/";
homecum <-  "Results/Cumulative/";
homenorm <- "Results/Normality/";
homehist <- "Results/Histograms/";
homecorr <- "Results/Correlations/";
homepval <- "Results/Pvalues/";
homecoeff <- "Results/Coefficients/";
homedata <-"files_aux/";
logdata<-1
if (logdata == 0){
  type_stat = "";
}else{
  type_stat = "Log";#indicação de nomes para arquivos e gráficos
}

#--- fim de entradas para execução em separado ---#


#
# Grafico do número diário de Covid

vfilesout=c();               
if (logdata==0){                      #arquivos das correlaçoes dos dados SEM transformações logarítmicas
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid22062020_correls.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidOutCH_I22062020_correls.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid22062020_outlarger_correls.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidlarger_correls.csv") );
}else{                                #arquivos das correlaçoes dos dados COM transformações logarítmicas
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid22062020_correls_Log.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidOutCH_I22062020_correls_Log.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid22062020_outlarger_correls_Log.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidlarger_correls_Log.csv") );
}

inf_I = c( 35, 35, 35, 35 );
if (logdata==1){                  #dados com transformações log
    sup_I = c( 120, 120, 120, 140 );
}else{                             #dados sem transformações log  
    sup_I = c( 140, 130, 125, 130 );
}

if (logdata==1 & N_i==295){                  #dados com transformações log
    sup_I = c( 250, 250, 250, 250 );
}else{                             #dados sem transformações log  
    sup_I = c( 250, 250, 250, 250 );
}

if (logdata==1 & N_i > 580){                  #dados com transformações log
  sup_I = c( 1, 1, 1, 1 ) * nx;
}else{                             #dados sem transformações log  
  sup_I = c( 1, 1,1, 1 )* nx ;
}




I=c();
 
 vec <- c();
 vec_pv <- c();
 auxcorrels <- c();
 max_correls <- c();
 # Analises j: 1, 2, 3 e 4
 jjj=1
 for ( jjj in c( 1:4 ) ){
  max_correls <- c();
  mataux <-c();
  
  I <- c( inf_I[jjj] : sup_I[jjj] )
  correl_pvalues <- read.csv ( vfilesout[jjj] );
  mataux <- read.csv( str_c(homedir,homecsv,"CI",type_stat,"_",jjj,".csv") );
  mat_ci <- mataux[,2:31];
  
  auxc <- c();
  aux_correls <- c();
  #max ABO+ ABO- ABO
   for ( k in c(1:15) ){
   
      vec    <- correl_pvalues[, 2*k];
      vec_pv <- correl_pvalues[, 2*k+1];
      max_vec <- max(vec[I]);
        ivec <- which.max(vec[I]);
        max_CI <-  mat_ci [ivec,(2*k-1):(2*k)]; # Matriz de Intervalos de confiança
      auxc <- c(max_vec, vec_pv[ivec+I[1]-1], ivec+I[1]-1, max_CI);
      aux_correls <- rbind ( aux_correls, auxc );
         
   }
     max_correls <- cbind ( max_correls, aux_correls );
     
     colnames( max_correls) = c("Correlation", "Pvalue", "Max_Day","CI_InF", "CI_Sup")
 
 # "A. = A+, A.. = A-"
     rownames(max_correls) =  c("Pop.", "O.",  "A.", "B.", "AB.", "O..1", "A..1", "B..1", "AB..1",
         "Correl_O", "Correl_A", "Correl_B", "Correl_AB", "Correl_rhp", "Correl_rhn");

     write.csv( max_correls, str_c( homedir, homecsv,"Correls_Max",type_stat,"_",jjj, ".csv" )); #type_stat ="" dados originais, type_stat ="" dados com transformações logarítmicas
  
     print(max_correls)
         
}  

         

