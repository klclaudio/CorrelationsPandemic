#
# Plots pvalues
#

color_l <- c("red",  "blue", "black", "cyan2" );# A, O, B, AB
  color_l2 <- c("brown", "green4"  ); #rhp rhn
     xp <- c(1:N_i)/100;

png( str_c( homedir, homepval, "PvaluesCovidABO_8Plots", type_stat, ll, "_", i, ".png"), width  =  700, height  =  700 );

 mypar( 2, 4 );
 par( mar <- c(3, 3, 3, 3)+0.3 );
   plot(  correlacoest[, 3], main = "Pvalues A+", ylab = "Pvalues", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
          ylim = c(-0.2, 1), xlab = "Days since five deaths (x 10²)", type = "l", lwd = 1, pch = 12 );
   grid( lty = 3, lwd = 1 );
   
   plot( correlacoest[, 2], main = "Pvalues O+", ylab = "Pvalues", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(-0.2, 1), xlab = "Days since five deaths (x 10²)", type = "l", pch = 10 );
   grid( lty = 3, lwd = 1 );
   
   plot( correlacoest[, 4], main = "Pvalues B+", ylab = "Pvalues", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(-0.2, 1), xlab = "Days since five deaths (x 10²)", type = "l", pch = 9 );
   grid( lty = 3, lwd = 1 );  
   
   plot( correlacoest[, 5], main = "Pvalues AB+", ylab = "Pvalues", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(-0.2, 1), xlab = "Days since five deaths (x 10²)", type = "l", pch = 8 );
   grid( lty = 3, lwd = 1 );  
   
   plot( correlacoest[, 7], main = "Pvalues A-", ylab = "Pvalues", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(-0.2, 1), xlab = "Days since five deaths (x 10²)", type = "l", pch = 0 );
   grid( lty = 3, lwd = 1 );
   
   plot( correlacoest[, 6], main = "Pvalues O-", ylab = "Pvalues", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(-0.2, 1), xlab = "Days since five deaths (x 10²)", type = "l", pch = 1 );
   grid( lty = 3, lwd = 1 );   
   
   plot( correlacoest[, 8], main = "Pvalues B-", ylab = "Pvalues", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(-0.2, 1), xlab = "Days since five deaths (x 10²)", type = "l", pch = 5 );
   grid( lty = 3, lwd = 1 ); 
   
   plot( correlacoest[, 9], main = "Pvalues AB-", ylab = "Pvalues", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(-0.2, 1), xlab = "Days since five deaths (x 10²)", type = "l", pch = 4 );
   grid( lty = 3, lwd = 1 );
  
mypar();
dev.off();

   
png( str_c( homedir, homepval, "PvaluesCovidABO", type_stat, ll, "_", i, ".png"), width = 700, height = 700 );

 mypar( 2, 2 ); par( new = FALSE );
 par( mar <- c(4, 4, 4, 4) );

#color_l = c("red", "blue", "black", "cyan2" );
 plot( xp, correlacoest[, 2], main = "ABO System and Rh+ (a)", ylab = "Pvalues", xlab = "Days since five deaths (x 10²)", 
     ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, type = "l", lty = 2, pch = 10, col = color_l[2] ); #O+
 abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
   grid( lty = 3, lwd = 1)
   
   if (logdata == 1) {
      if (ll != 4) {
          legend( "top", legend = c("A +", "O +", "B +", "AB +"), bty = "n", col = color_l, lty = c(1, 2, 3, 6), cex = 1.0 );
       }else{    
          legend( "topright", legend = c("A +", "O +", "B +", "AB +"), bty = "n", col = color_l, lty = c(1, 2, 3, 6), cex = 1.0 );
        }
   }else{
        legend( "top", legend = c("A +", "O +", "B +", "AB +"), bty = "n", col = color_l, lty = c(1, 2, 3, 6), cex = 1.0 );
   }
   
  #pch = c(10, 12, 9, 8, 1, 0, 5, 4))
  
   par( new = TRUE );
   plot( xp, correlacoest[, 3], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,
         ylim = c(0, 1), col = color_l[1], type = "l", lty = 1, pch = 12 ); #A+
   abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
   grid( lty = 3, lwd = 1)
  
   par( new = TRUE );
   plot( xp, correlacoest[, 4], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(0, 1), col = color_l[3], type = "l", lty = 3, pch = 9 ); #B+
   abline(  h = 0.05, col = "grey", lty = 2, lwd = 2 );
   
   par( new = TRUE );
   plot( xp, correlacoest[, 5], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(0, 1), col = color_l[4], type = "l", lty = 6, pch = 8 ); #AB+
   abline(  h = 0.05, col = "grey", lty = 2, lwd = 2 );
  
 plot( xp, correlacoest[, 6], main = "ABO System and Rh- (b)", ylab = "Pvalues", xlab = "Days since five deaths (x 10²)", 
     ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[2], type = "l", lty = 2,  pch = 1 ); #O-
 abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );  
   
   if (ll != 4) {   
       legend( "top", legend = c("A -", "O -", "B -", "AB -"), lty = c(1, 2, 3, 6), col = color_l, bty = "n", cex = 1.0, bg = 'white' );
    }else{
       legend( "topright", legend = c("A -", "O -", "B -", "AB -"), lty = c(1, 2, 3, 6), col = color_l, bty = "n", cex = 1.0, bg = 'white' );
    }
   
   grid( lty = 3, lwd = 1 );
   
   par( new = TRUE );
   plot( xp, correlacoest[, 7], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[1], type = "l", lty = 1, pch = 0 );#A-
    abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
  
   par( new = TRUE );
   plot( xp, correlacoest[, 8], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[3], type = "l", lty = 3, pch = 5 );#B-
    abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
  
   par( new = TRUE );
   plot( xp, correlacoest[, 9], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[4], type = "l", lty = 6, pch = 4 );#AB-
    abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
     

 plot( xp, correlacoest_O,  main = "ABO System (c)", ylab = "Pvalues", xlab = "Days since five deaths (x 10²)", 
       ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[2], type = 'l', lty = 2, pch = "o" );
    abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
   
     if (logdata == 1) {
        if (ll != 4) {  
           legend( "top", legend = c("A", "O", "B", "AB"), lty = c(1, 2, 3, 6), col = color_l, bty = "n", cex = 1.0, bg = 'white' );
        }else{ 
           legend( "topright", legend = c("A", "O", "B", "AB"), lty = c(1, 2, 3, 6), col = color_l, bty = "n", cex = 1.0, bg = 'white' );
        }
    }else{
           legend( "top", legend = c("A", "O", "B", "AB"), lty = c(1, 2, 3, 6), bty = "n", col = color_l, cex = 1.0, bg = 'white' );
    }
       
    grid( lty = 3, lwd = 1 );
    
   par( new = TRUE );

 plot( xp, correlacoest_A, ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
       ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[1], type = "l", lty = 1, pch = "+" );
    abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
   par( new = TRUE );

 plot( xp, correlacoest_B, ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
       ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[3], type = "l", lty = 3, pch = "-" );
    abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
   par( new = TRUE );

 plot( xp, correlacoest_AB, ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
       ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[4], type = "l", lty = 6, pch = "*" );
    abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );

 plot( xp, correlacoest_rhp,  main = "Rhesus (Rh) System (d)", ylab = "Pvalues", xlab = "Days since five deaths (x 10²)", 
       ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[1], type = "l", lty = 1, pch = "+" );
     abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
  
  if (logdata == 1) {
     if (ll != 4) {
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), col = color_l2, bty = "n", cex = 1.0, bg = 'white' );
        grid( lty = 3, lwd = 1 );
     }else{
        legend( "topright", legend = c("Rh +", "Rh -"), lty = c(1, 2), col = color_l2, bty = "n", cex = 1.0, bg = 'white' );
        grid( lty = 3, lwd = 1 );
     }
  }else{
     if (ll != 4) {
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), col = color_l2, bty = "n", cex = 1.0, bg = 'white' );
        grid( lty = 3, lwd = 1 );
     }else{
        legend( "topright", legend = c("Rh +", "Rh -"), lty = c(1, 2), col = color_l2, bty = "n", cex = 1.0, bg = 'white' );
        grid( lty = 3, lwd = 1 );
     }
     
  }# End if
    
  par( new = TRUE );
  plot( xp, correlacoest_rhn, ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
       ylim = c(0, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o" );
    abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );

par( new = FALSE );
dev.off() #fecha saída de arquivo   

#End plotpvalues
