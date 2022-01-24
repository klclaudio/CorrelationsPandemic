# Grafico do número diário de Covid
maxmin_values_f <- function (homedir, homecsv, N_i, nx, type_stat) {

vfilesout=c();               
if (logdata==0){                      #arquivos das correlações dos dados SEM transformações logarítmicas
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid_correls.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidOutCH_I_correls.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid_outlarger_correls.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidlarger_correls.csv") );
}else{                                #arquivos das correlações dos dados COM transformações logarítmicas
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid_correls_Log.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidOutCH_I_correls_Log.csv") );
   vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid_outlarger_correls_Log.csv") );
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
 j=1
 for ( j in c( 1:4 ) ){
  max_correls <- c();
    mataux <-c();
  
  I <- c( inf_I[j] : sup_I[j] )
   correl_pvalues <- read.csv ( vfilesout[j] );
     mataux <- read.csv( str_c(homedir, homecsv, "CI", type_stat, "_", ll, ".csv") );
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
         
   } #end for k
     max_correls <- cbind ( max_correls, aux_correls );
        colnames( max_correls) = c("Correlation", "Pvalue", "Max_Day","CI_InF", "CI_Sup")
 
 # "A. = A+, A.. = A-"
     rownames(max_correls) =  c("Pop.", "O+",  "A+", "B+", "AB+", "O-", "A-", "B-", "AB-",
         "Correl_O", "Correl_A", "Correl_B", "Correl_AB", "Correl_rh+", "Correl_rh-");
        write.csv( max_correls, str_c( homedir, homecsv,"Correls_Max",type_stat,"_",j, ".csv" )); #type_stat ="" dados originais, type_stat ="" dados com transformações logarítmicas
    
     print(str_c("Análise ", j));
       print(max_correls);  
 }  #end for j


teste<-1
return(teste) 
} # finish function maxmin_values
