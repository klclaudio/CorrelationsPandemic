#
# Data files names (in  and  out)
# 

vfilesout=c();               
  vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid-correls",type_stat,".csv") );
  vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidOutCH-I-correls",type_stat,".csv") );  
  vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid-outlarger-correls",type_stat,".csv") );
  vfilesout <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidlarger-correls",type_stat,".csv") );

vfilesout_hist=c(); 
  vfilesout_hist <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid-correls-hist",type_stat,".csv") );
  vfilesout_hist <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidOutCH-I-correls-hist",type_stat,".csv") );
  vfilesout_hist <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovid-outlarger-correls-hist",type_stat,".csv") );
  vfilesout_hist <- rbind(vfilesout, str_c ( homedir,homecsv,"bloodcovidlarger-correls-hist",type_stat,".csv") );

  fileout = vfilesout [ll];    
  fileout_hist = vfilesout_hist[ll];

  
   if( ll == 1 ) {
       data_c <- read.csv( str_c(homedir,"bloodcovid.csv") );
       data_c_percents <- read.csv( str_c(homedir,"bloodcovid-percents.csv") );
       
     } else if( ll == 2 ) {
         data_c <- read.csv( str_c(homedir,"bloodcovid-OutCH-I.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid-OutCH-I-percents.csv") );
         
      } else if( ll == 3 ) {
         data_c <- read.csv( str_c(homedir,"bloodcovid-outlarger.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid-outlarger-percents.csv") );
        
      } else if( ll == 4 ) {
         data_c <- read.csv( str_c(homedir,"bloodcovid-larger.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid-larger-percents.csv") );
         
        
      } else {  
         data_c <- read.csv( str_c(homedir,"bloodcovid-percents.csv") );
         data_c_percents <- read.csv( str_c(homedir,"bloodcovid-percents.csv") );
         
         fileout = str_c(homedir,homecsv,"bloodcovid-percents-correls",type_stat,".csv" );
         fileout_hist = str_c(homedir,homecsv,"bloodcovid-percents-hist",type_stat,".csv" );
      }
    

# End data-inout