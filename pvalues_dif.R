#
# Plot comparative analysis and it saves in the folder <homedaily>
#

color_l = c("red", "blue", "black", "cyan2");  # A, O, B, AB
   color_l2 = c("brown", "green4"); #rhp rhn


png( str_c(homedir, homedaily, "ComparativeAnalysis", ".png"), width = 700, height = 500 )

mypar( 1, 1 ); 
 par( new = FALSE )
 par( mar = c(4, 5, 4, 5) );
 
 plot( correlacoest_rhp[c(13:166)],  main = "Comparative Analysis", ylab = "", xlab = "Days since five deaths", 
       ylim = c(-1, 1), xlim = c(1, 166), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = "grey", 
       type = "l", lty = 1, lwd = 2, pch = "+" 
     );
     abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
   if (logdata == 1){
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
       #legend( "topleft", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        legend( "bottomright", legend = c( "Deaths"), lty = c(1, 2), lwd = 2, bty = "n", cex = 1.3, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }
   }else{
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
         legend( "bottomright", legend = c( "Deaths"), lty = c(1, 2), lwd = 2, bty = "n", cex = 1.3, bg = 'white' );
        #grid(lty = 3, lwd = 1);
     }
     
   }
     mtext( "Statistics", cex = ftsizelb, side = 2, line = 3 ) 
   par( new = TRUE );
   plot( correlacoest_rhn[1:166], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, xlim = c(1, 166), 
         ylim = c(-1, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o" 
       );
   abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
 
 
#Comparative plots 12 days
  par( new = TRUE );
  plot(covid_ll[, 4], 
       #main = str_c("", idg[iii]), 
        xlab = "Days since five deaths", 
        ylab = "", cex.main = ftsize, cex.lab = ftsizelb,  axes = F, cex.axis = ftsizeax, 
        ylim = c(0, 7500), 
        xlim = c(1, 166), 
        type = "l", 
        lty = 1, 
        lwd = 2
        ); 
  #grid(lty = 3, lwd = 1);
  abline( v = 12, col = "grey", lty = 2, lwd = 2 );
  axis( side = 4, cex.axis = ftsizeax );    
  mtext( "Deaths", cex = ftsizelb, side = 4, line = 3 );
  
  par( new = TRUE );
  plot( correlacoest_rhp[c(1:166)],  main = "", ylab = "", xlab = "Days since five deaths", 
        ylim = c(-1, 1), xlim = c(1, 166), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  axes = F, col = color_l2[1], 
        type = "l", lty = 1, pch = "+" 
       );
     abline(h = 0.05, col = "grey", lty = 2, lwd = 2);
   if (logdata == 1){
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );      
        grid(lty = 3, lwd = 1);
     }else{
        #legend( "topleft", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        legend( 120, 0.55, legend = c("Rh +", "Rh -", "Rh+ (-12 days)"), lwd = c(1, 1, 2), col = c(color_l2[1], color_l2[2], "grey"), 
                lty = c(1, 2, 1), bty = "n", cex = 1.3, bg = 'white' 
              );
        grid(lty = 3, lwd = 1);
     }
   }else{
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
       legend( 120, 0.55, legend = c("Rh +", "Rh -", "Rh+ (-12 days)"), lwd = c(1, 1, 2), col = c("black", "black", "grey"), 
               lty = c(1, 2, 1), bty = "n", cex = 1.3, bg = 'white' 
             );
        grid(lty = 3, lwd = 2);
     }
     
  }
  abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );
  mtext( "", cex = ftsizeax*0.9, side = 2, line = 3 ); 
  par( new = TRUE );

  
  plot( correlacoest_rhn[1:166], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, xlim = c(1, 166), 
        ylim = c(-1, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o" 
      );
  
      mtext( "", cex = ftsizeax*0.9, side = 2, line = 3) 
      par( new = TRUE );
      
   plot( correlacoes_rhn[1:166], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, xlim = c(1, 166), 
         ylim = c(-1, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o" 
       );
      par(new = TRUE);
      plot( correlacoes_rhp[c(1:166)],  main = "", ylab = "", xlab = "Days since five deaths", 
            ylim = c(-1, 1), xlim = c(1, 166), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  axes = F, col = color_l2[1], 
            type = "l", lty = 1, pch = "+" 
          );
       par( new = TRUE );
      
      plot( correlacoes_rhp[c(13:166)],  main = "", ylab = "", xlab = "Days since five deaths", 
            ylim = c(-1, 1), xlim = c(1, 166), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  axes = F, col = "grey", 
            type = "l", lwd = 2, lty = 1, pch = "+" 
          );
     
      abline( v = 30, col = "grey", lty = 2, lwd = 2 );
      abline( v = 42, col = "grey", lty = 2, lwd = 2 );

  
mypar(); 
par( new = FALSE );
dev.off(); 
      
#
#-------------------------------Comparative Analysis N_i days----------------------------------
#
png( str_c(homedir, homedaily, "ComparativeAnalysis_", N_i, ".png"), width = 700, height = 500 )

mypar(1, 1);
 par( new = FALSE )
 par ( mar = c(4, 5, 4, 5) );
xlimite = N_i;
 plot( correlacoest_rhp[c(13:xlimite)],  main = "Comparative Analysis", ylab = "", xlab = "Days since five deaths", 
       ylim = c(-1, 1), xlim = c(1, xlimite), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = "grey", type = "l", 
       lty = 1, lwd = 2, pch = "+" 
     );
     abline(h = 0.05, col = "grey", lty = 2, lwd = 2);
   if (logdata == 1){
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
       #legend( "topleft", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        legend( "bottomright", legend = c( "Deaths"), lty = c(1, 2), lwd = 2, bty = "n", cex = 1.3, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }
   }else{
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
         legend( "bottomright", legend = c( "Deaths"), lty = c(1, 2), lwd = 2, bty = "n", cex = 1.3, bg = 'white' );
        #grid(lty = 3, lwd = 1);
     }

   }
     mtext( "Statistics", cex = ftsizelb, side = 2, line = 3 );
   par( new = TRUE );
   plot( correlacoest_rhn[1:xlimite], ylab = "", xlab = "", cex.main = ftsize, 
         cex.lab = ftsizelb, cex.axis = ftsizeax, xlim = c(1, xlimite), ylim = c(-1, 1), 
         cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o" );
   abline( h = 0.05, col = "grey", lty = 2, lwd = 2 );


#Comparative plots 12 days
  par( new = TRUE );
  plot( covid_ll[, 4], 
       #main = str_c("", idg[iii]), 
        xlab = "Days since five deaths", 
        ylab = "", cex.main = ftsize, cex.lab = ftsizelb,  axes = F, cex.axis = ftsizeax, 
        ylim = c(0, 11000), 
        xlim = c(1, xlimite), 
        type = "l", 
        lty = 1, 
        lwd = 2
      );
  #grid(lty = 3, lwd = 1);
  abline(v = 12, col = "grey", lty = 2, lwd = 2);
  axis( side = 4, cex.axis = ftsizeax );
  mtext( "Deaths", cex = ftsizelb, side = 4, line = 3);

  par(new = TRUE);
  plot( correlacoest_rhp[c(1:xlimite)],  main = "", ylab = "", xlab = "Days since five deaths", 
        ylim = c(-1, 1), xlim = c(1, xlimite), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  
        axes = F, col = color_l2[1], type = "l", lty = 1, pch = "+" );
        abline(h = 0.05, col = "grey", lty = 2, lwd = 2
       );
   if (logdata == 1){
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
        #legend( "topleft", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        legend( 80, 0.7, legend = c("Rh +", "Rh -", "Rh+ (-12 days)"), lwd = c(1, 1, 2), col = c(color_l2[1], color_l2[2], 
                "grey"), lty = c(1, 2, 1), bty = "n", cex = 1.3, bg = 'white' 
               );
              grid( lty = 3, lwd = 1 );
     }
   }else{
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
       legend( 80, 0.55, legend = c("Rh +", "Rh -", "Rh+ (-12 days)"), lwd = c(1, 1, 2), col = c("black", "black", "grey"), 
               lty = c(1, 2, 1), bty = "n", cex = 1.3, bg = 'white' );
        grid(lty = 3, lwd = 2);
     }

  }
  abline(h = 0.05, col = "grey", lty = 2, lwd = 2);
  mtext ( "", cex = ftsizeax*0.9, side = 2, line = 3)
  par(new = TRUE);


  plot( correlacoest_rhn[1:xlimite], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
        xlim = c(1, xlimite), ylim = c(-1, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], 
        type = "l", lty = 2, pch = "o" 
      );

      mtext ( "", cex = ftsizeax*0.9, side = 2, line = 3)
      par(new = TRUE);

   plot( correlacoes_rhn[1:xlimite], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, 
         xlim = c(1, xlimite), ylim = c(-1, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], 
         type = "l", lty = 2, pch = "o" 
        );
      par(new = TRUE);
      plot( correlacoes_rhp[c(1:xlimite)],  main = "", ylab = "", xlab = "Days since five deaths", 
            ylim = c(-1, 1), xlim = c(1, xlimite), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  axes = F, col = color_l2[1], 
            type = "l", lty = 1, pch = "+" 
          );
       par(new = TRUE);

      plot( correlacoes_rhp[c(13:xlimite)],  main = "", ylab = "", xlab = "Days since five deaths", 
            ylim = c(-1, 1), xlim = c(1, xlimite), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  axes = F, col = "grey", 
            type = "l", lwd = 2, lty = 1, pch = "+" 
          );

      abline( v = 30, col = "grey", lty = 2, lwd = 2 );
      abline( v = 42, col = "grey", lty = 2, lwd = 2 );


mypar();
par( new = FALSE );
dev.off(); 
      
#
#-------------------------------Comparative Analysis 50 days----------------------------------
#
png( str_c(homedir, homedaily, "ComparativeAnalysis_50", ".png"), width = 700, height = 500 )
mypar(1, 1); 
 par( new = FALSE );
 par( mar = c(4, 5, 4, 5) );

# Rh positivo - teste
 plot( correlacoest_rhp[c(13:63)],  main = "Comparative Analysis", ylab = "", xlab = "Days since five deaths", 
       ylim = c(-1, 1), xlim = c(1, 50), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = "grey", lwd = 2, 
       type = "l", lty = 1, pch = "+" 
     );
       abline(h = 0.05, col = "grey", lty = 2, lwd = 2
     );
  if (logdata == 1){
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
#        legend( "topleft", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
          legend( "bottomright", legend = c( "Deaths"), lty = c(1, 2), lwd = 2, bty = "n", cex = 1.3, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }
  }else{
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
         legend( "bottomright", legend = c( "Deaths"), lty = c(1, 2), lwd = 2, bty = "n", cex = 1.3, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }
     
  }
    mtext( "Statistics", cex = ftsizelb, side = 2, line = 3);

      par( new = TRUE );
  
    plot( correlacoest_rhp[c(1:50)],  main = "", ylab = "", xlab = "Days since five deaths", 
          ylim = c(-1, 1), xlim = c(1, 50), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  axes = F, col = color_l2[1], 
          type = "l", lty = 1, pch = "+" 
        );
#     abline(h = 0.05, col = "grey", lty = 2, lwd = 2);
   
 
# Rh positivo  
  par(new = TRUE);
      
      plot( correlacoes_rhp[c(13:63)],  main = "", ylab = "", xlab = "Days since five deaths", 
            ylim = c(-1, 1), xlim = c(1, 50), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  axes = F, col = "grey", lwd = 2, 
            type = "l", lty = 1, pch = "+" 
          );
      
      par(new = TRUE);
      plot( correlacoes_rhp[c(1:50)],  main = "", ylab = "", xlab = "Days since five deaths", 
            ylim = c(-1, 1), xlim = c(1, 50), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax,  axes = F, col = color_l2[1], 
            type = "l", lty = 1, pch = "+" 
          );
      
      if (logdata == 1){
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
#        legend( "topleft", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
         legend( 8, 1.1, legend = c("Rh+", "Rh-", "Rh+ (-12 days)"), lwd = c(1, 1, 2), col = c("brown", "green4", "grey"), 
                 lty = c(1, 2, 1), bty = "n", cex = 1.3, bg = 'white' 
                );
         grid( lty = 3, lwd = 1 );
     }
  }else{
     if (ll!=4){
        legend( "top", legend = c("Rh +", "Rh -"), lty = c(1, 2), bty = "n", cex = 1.0, bg = 'white' );
        grid(lty = 3, lwd = 1);
     }else{
      # legend( "topright", legend = c("Rh +", "Rh -"), lty = c(1, 2, 1), bty = "n", cex = 1.3, bg = 'white' );
       legend( 8, 1.1, legend = c("Rh +", "Rh -", "Rh+ (-12 days)"), lwd = c(1, 1, 2), col = c("black", "black", "grey"), 
               lty = c(1, 2, 1), bty = "n", cex = 1.3, bg = 'white' 
             );
       
#        grid(lty = 3, lwd = 1);
     }
     
  }
     
         abline( v = 30, col = "grey", lty = 2, lwd = 2 );
         abline( v = 42, col = "grey", lty = 2, lwd = 2 );
 
 # Rh negativo teste  
   par( new = TRUE );
   plot( correlacoest_rhn[1:50], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, xlim = c(1, 50), 
         ylim = c(-1, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o" 
       );
 
   par( new = TRUE );
      
   plot( correlacoes_rhn[1:50], ylab = "", xlab = "", cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, xlim = c(1, 50), 
         ylim = c(-1, 1), cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o" 
       );
  
 
#Deaths per day

par( new = TRUE );
plot( covid_ll[1:50, 4], 
       #main = str_c("", idg[iii]), 
        xlab = "Days since five deaths", 
        ylab = "", cex.main = ftsize, cex.lab = ftsizelb,  axes = F, cex.axis = ftsizeax, 
        ylim = c(0, 7500), 
        xlim = c(1, 50), 
        type = "l", 
        lty = 1, 
        lwd = 2
    );
#  grid(lty = 3, lwd = 1);
  abline (v = 12, col = "grey", lty = 2, lwd = 2);
  axis (side = 4, cex.axis = ftsizeax);
  mtext ( "Deaths", cex = ftsizelb, side = 4, line = 3);


mypar();
par( new = FALSE );
dev.off(); 
  
#colnames(dif) =  c("pvalue_rhp", "pvalue_rhn",  "rhp", "rhn", "defasagen_t");
#rownames(dif) =  c("min", "max");

i_min_trhn <- which.min( correlacoest_rhn[4:25] ) + 3;
min_trhn <- min( correlacoest_rhn[4:25] );
#9
i_min_rhn <- which.min( correlacoes_rhn[4:25] ) + 3;
min_rhn <- min( correlacoes_rhn[4:25] );  #9

i_min_trhp <- which.min( correlacoest_rhp[4:25] ) +3;
min_trhp <- min( correlacoest_rhp[4:25] );

i_min_rhp <- which.min( correlacoes_rhp[4:25] ) + 3;
min_rhp <- min( correlacoes_rhp[4:25] );


i_max_trhn <- which.max( correlacoest_rhn[4:50] ) + 3;
max_trhn <- max( correlacoest_rhn[4:50] );


i_max_trhp <- which.max( correlacoest_rhp[4:50] ) + 3;
max_trhp <- max( correlacoest_rhp[4:50] );


i_max_rhn <- which.max( correlacoes_rhn[4:460] ) + 3;
max_rhn <- max( correlacoes_rhn[4:460] );


i_max_rhp <- which.max(correlacoes_rhp[4:460]) + 3;
max_rhp <- max(correlacoes_rhp[4:460]);

defasagem_i <- i_max_trhp - i_max_trhn;

dif_ro <- c();

dif_ro <- c( i_min_trhn, i_min_trhp, i_min_rhn, i_min_rhp, min_trhn, min_trhp, min_rhn, min_rhp, 0 );
dif_ro <- rbind( dif_ro, c(i_max_trhn, i_max_trhp, i_max_rhp, i_max_rhp, max_trhn, max_trhp, max_rhn, max_rhp, defasagem_i) );

colnames(dif_ro) = c( "ipvalue_rhn", "ip_value_rhp", "irhn", "irhp", "pvalue_rhn", "p_value_rhp", "rhn", "rhp", "defasagem" );
rownames(dif_ro) = c( "min", "max" )

#End pvalues_dif.R
