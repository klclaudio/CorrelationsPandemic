# entrada e saidas de correlações e pvalues
if (logdata==0){
   if (ll==1){
   
      data_c <- read.csv( str_c(homedir,"bloodcovid.csv") );
       data_c_percents <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
       
      fileout = str_c( homedir,homescv,"bloodcovid_correls.csv" );
      fileout_hist = str_c( homedir,homescv,"bloodcovid_hist.csv" );
              
      } else if(ll==2){
         data_c <- read.csv( str_c(homedir,"bloodcovidOutCH_I.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovidOutCH_I_percents.csv") );
         
         fileout = str_c( homedir,homescv,"bloodcovidOutCH_I_correls.csv" );
         fileout_hist = str_c( homedir,homescv,"bloodcovidOutCH_I_hist.csv" );
        
      } else if(ll==3){
      
         data_c <- read.csv( str_c(homedir,"bloodcovid_outlarger.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid_outlarger_percents.csv") );
         
         fileout = str_c(homedir,homecsv,"bloodcovid_outlarger_correls.csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovid_outlarger_hist.csv" );
        
      } else if(ll==4){
      
         data_c <- read.csv( str_c(homedir,"bloodcovidlarger.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovidlarger_percents.csv") );
         
         fileout = str_c(homedir,homecsv,"bloodcovidlarger_correls.csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovidlarger_hist.csv" );
        
      } else{  
      
         data_c <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
         
         fileout = str_c(homedir,homecsv,"bloodcovid_percents_correls.csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovid_percents_hist.csv" );
      }
    }else{
     if (ll==1){
   
      data_c <- read.csv( str_c(homedir,"bloodcovid.csv") );
      data_c_percents <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
      
      fileout = str_c(homedir,homecsv,"bloodcovid_correls_Log.csv" );
      fileout_hist = str_c(homedir,homecsv,"bloodcovid_hist_Log.csv" );
  
      
      } else if(ll==2){
         data_c <- read.csv( str_c(homedir,"bloodcovidOutCH_I.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovidOutCH_I_percents.csv") );
         
         fileout = str_c(homedir,homecsv,"bloodcovidOutCH_I_correls_Log.csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovidOutCH_I_hist_Log.csv" );
        
      } else if(ll==3){
      
         data_c <- read.csv( str_c(homedir,"bloodcovid_outlarger.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid_outlarger_percents.csv") ); 
         
         fileout = str_c(homedir,homecsv,"bloodcovid_outlarger_correls_Log.csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovid_outlarger_hist_Log.csv" );
        
      } else if(ll==4){
      
         data_c <- read.csv( str_c(homedir,"bloodcovidlarger.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovidlarger_percents.csv") );
         
         fileout = str_c(homedir,homecsv,"bloodcovidlarger_correls_Log.csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovidlarger_hist_Log.csv" );
        
      } else{  
      
         data_c <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
         
         fileout = str_c(homedir,homecsv,"bloodcovid_percents_correls_Log.csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovid_percents_hist_Log.csv" );
        
      }    
    }
    


    # Fim entrada de dados
