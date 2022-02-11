#
# Data files names (in  and  out)
# 

vfilesout=c();               
  vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid_correls",type_stat,".csv") );
  vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidOutCH_I_correls",type_stat,".csv") );  
  vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid_outlarger_correls",type_stat,".csv") );
  vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidlarger_correls",type_stat,".csv") );

vfilesout_hist=c(); 
  vfilesout_hist <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid_correls_hist",type_stat,".csv") );
  vfilesout_hist <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidOutCH_I_correls_hist",type_stat,".csv") );
  vfilesout_hist <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid_outlarger_correls_hist",type_stat,".csv") );
  vfilesout_hist <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidlarger_correls_hist",type_stat,".csv") );

  fileout = vfilesout [ll];    
  fileout_hist = vfilesout_hist[ll];

  
   if( ll == 1 ) {
       data_c <- read.csv( str_c(homedir,"bloodcovid.csv") );
       data_c_percents <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
       
     } else if( ll == 2 ) {
         data_c <- read.csv( str_c(homedir,"bloodcovidOutCH_I.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovidOutCH_I_percents.csv") );
         
      } else if( ll == 3 ) {
         data_c <- read.csv( str_c(homedir,"bloodcovid_outlarger.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid_outlarger_percents.csv") );
        
      } else if( ll == 4 ) {
         data_c <- read.csv( str_c(homedir,"bloodcovidlarger.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovidlarger_percents.csv") );
         
        
      } else {  
         data_c <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid_percents.csv") );
         
         fileout = str_c(homedir,homecsv,"bloodcovid_percents_correls",type_stat,".csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovid_percents_hist",type_stat,".csv" );
      }
    

# End data_inout