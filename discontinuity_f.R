#
# Eliminate countries from analysis
#

discontinuity_f <- function( aux2, covid, outcountry ) {
  
   dimaux2 <- dim( aux2 );
   rn <- rownames( aux2 );
   
   k <- 1;
   n <-  dimaux2[1];
   found <- FALSE;
   while( found == FALSE && k<n ) {
   
        if( rn [k] == outcountry ) {
           found <- TRUE;
           aux2 <- aux2[-k,]
           covid <- covid [-k]
           aux2 <- cbind( aux2,covid ) 
        }
     k <- k+1
     
   }
   
   
return( aux2 ) 
}




