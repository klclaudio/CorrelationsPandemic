#
# Plots correlations between bloodtypes/populations and deaths 
#

ftsizeax = 1.3;
  ftsizelb = 1.5;
    ftsize = 1.5;
    
if( ll !=  4 ) {
  
    ylimite <- c(-0.2, 1);
    
} else{
  
    ylimite <- c(-1, 1);
    
}

color_l <- c( "red", "blue", "black", "cyan2" );# A, O, B, AB
  color_l2 <- c( "brown", "green4" ); #rhp rhn
     xp <- c(1:N_i)/100;

png( str_c( homedir, homecorr, "CorrelationsPopulations", type_stat, ll, "_", i, ".png"), width = 500, height = 500 );

   mypar();
   par( mar = c(4, 4, 4, 4) );

   plot( correlacoes[, 1], main = "Population analysis", ylab = "Correlations", xlab = "Days since five deaths", 
         ylim = ylimite, cex.main = ftsize, cex.lab =  ftsize, cex.axis =  ftsize, type = "l", lty = 1); #Population

   grid( lty = 3, lwd = 1 );

dev.off();



png( str_c( homedir, homecorr, "CorrelationsCovidABO", type_stat, ll, "_", i, ".png"), width = 700, height = 700 );
 
 mypar( 2, 2); par( new = FALSE);
 par( mar = c(4, 4, 4, 4))
 plot( correlacoes[, 2], main = "ABO System and Rh+ (a)", ylab = "Correlations", xlab = "Days since five deaths", 
       ylim = ylimite, cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[2], type = "l", lty = 2, pch = 10 ); #O+
  
   #if (ll ! =  4){
       legend("bottom", legend = c("A +", "O +", "B +", "AB +"), bty = "n",  col = color_l, lty = c(1, 2, 3, 6), cex = 1.0);
   #}else{
   #   legend("topleft", legend = c("A +", "O +", "B +", "AB +"), bty = "n",  col = color_l, lty = c(1, 2, 3, 6), cex = 1.0);
   #}
  
   grid( lty = 3, lwd = 1)
   
   par( new = TRUE );
   plot(  correlacoes[, 3], ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb, 
          cex.axis =  ftsizeax, col = color_l[1], type = "l", lty = 1, pch = 12);  #A+
   grid( lty = 3, lwd = 1)
  
   par( new = TRUE );
   plot(  correlacoes[, 4], ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb, 
          cex.axis =  ftsizeax, col = color_l[3], type = "l", lty = 3, pch = 9);  #B+
   
   par( new = TRUE );
   plot( correlacoes[, 5], ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb,
         cex.axis =  ftsizeax,  col = color_l[4], type = "l", lty = 6, pch = 8); #AB+ 
  
  
 plot( correlacoes[, 6], main = "ABO System and Rh- (b)", ylab = "Correlations", xlab = "Days since five deaths", 
       ylim = ylimite, cex.main = ftsize, cex.lab = ftsizelb,  cex.axis = ftsizeax, col = color_l[2], type = "l", lty = 2,  pch = 1)  #O-
  
   #if (ll ! =  4){
      legend("bottom", legend = c("A -", "O -", "B -", "AB -"), lty = c(1, 2, 3, 6), bty = "n",  col = color_l, cex = 1.0, bg = 'white');
   #}else{
   #  legend("topleft", legend = c("A -", "O -", "B -", "AB -"), lty = c(1, 2, 3, 6), bty = "n",  col = color_l, cex = 1.0, bg = 'white');
   #}
   
   grid( lty = 3, lwd = 1 );
   
   par( new = TRUE );
   plot( correlacoes[, 7], ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb, 
         cex.axis =  ftsizeax, col = color_l[1], type = "l", lty = 1, pch = 0);    #A-
  
   par( new = TRUE );
   plot( correlacoes[, 8], ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb,  
         cex.axis =  ftsizeax, col = color_l[3], type = "l", lty = 3, pch = 5 ); #B-
  
   par( new = TRUE );
   plot( correlacoes[, 9], ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb, 
         cex.axis =  ftsizeax, col = color_l[4], type = "l", lty = 6, pch = 4);   #AB-
     

 plot( correlacoes_O,  main = "ABO System (c)", ylab = "Correlations", xlab = "Days since five deaths", 
       ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb,  cex.axis =  ftsizeax, col = color_l[2], type = 'l', lty = 2, pch = "o")
     
   # if (ll ! =  4){
       legend("bottom", legend = c("A", "O", "B", "AB"), lty = c(1, 2, 3, 6), bty = "n",  col = color_l, cex = 1.0, bg = 'white');
   #  }else{
   #  legend("topleft", legend = c("A", "O", "B", "AB"), lty = c(1, 2, 3, 6), bty = "n",  col = color_l, cex = 1.0, bg = 'white');
   #}
   
   grid( lty = 3, lwd = 1 );
  
   par( new = TRUE );

   plot( correlacoes_A, ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb, 
         cex.axis =  ftsizeax, col = color_l[1], type = "l", lty = 1, pch = "+");    #A
   par( new = TRUE );

   plot( correlacoes_B, ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb, 
         cex.axis =  ftsizeax, col = color_l[3], type =  "l", lty = 3, pch = "-");    #B
   par( new = TRUE );

   plot( correlacoes_AB, ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb, 
         cex.axis =  ftsizeax, col = color_l[4], type =  "l", lty = 6, pch = "*");   #AB
    

 plot( correlacoes_rhp,  main = "Rhesus(Rh) System (d)", ylab = "Correlations", xlab = "Days since five deaths", 
      ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb,  cex.axis =  ftsizeax, col = color_l2[1], type = "l", lty = 1, pch = "+"); 
 
   #if (ll ! =  4){
      legend("bottom", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", col = color_l2, cex = 1.0, bg = 'white');
   #}else{
   #  legend("topleft", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", col = color_l2, cex = 1.0, bg = 'white');
   #}
   grid( lty = 3, lwd = 1 );
  
   par( new = TRUE );
   plot( correlacoes_rhn, ylab = "", xlab = "", ylim = ylimite, cex.main = ftsize, cex.lab =  ftsizelb,  
         cex.axis =  ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o");#rhn
   
 par( new = FALSE );
        
dev.off()
        
#End plotcorrelations
