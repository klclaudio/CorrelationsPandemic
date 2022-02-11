#
# Plots data blood types and deaths
#
 
 xlimite <- c(1.5, 21);
   ylimite <- c(1.5, 21);
     xlimite2 <- c(1.5, 21);
       ylimite2 <- c(1.5, 13.5);
 
 main_title <- c("Population", "O+", "A+", "B+", "AB +", "O-", "A-", "B -", "AB-", "COVID-19 deaths");
 
 main_title2 <- c("Population", "O+", "A+", "B+", "AB +", "O-", "A-", "B -", "AB-", "Deaths");
  
 ylabels <- c("log( Population )", "log( O + )", "log( A + )", "log( B + )", "log( AB + )", 
             "log( O - )", "log( A - )", "log( B - )", "log( AB - )", "log( Deaths )" );
            
 ftsize = 1.3;          
   ftsize2 = 1.2;
 ftsizeax = 1.3;
   ftsizelb = 1.5;
      ftsize  = 1.5;
 
 crow_w <- order( covid );#indices of ordered vector

# Plots population ordered data,blood types and covid(deaths) x vector indices 
png( str_c(homedir, homebld, "Bldtype_Log_orderby_bldtype_", ll, "_", i, ".png"), width = 800, height = 800 )
  par( new = TRUE ); 
  marg <- c(3, 3, 2, 3)
 
  mypar();
  mypar( 5, 2 );
    
    for( jj in c(1:9) ) {
        
       crow_w <- order( aux2[, jj] )
       par( mar = marg );
       plot( aux2[crow_w, jj], 
             main = str_c("Ordered by ", main_title[jj]), 
             ylab = main_title2[jj],  
             ylim = c(2, 22), 
             cex.axis = ftsize2, 
             cex.lab = ftsize 
            );
      grid( lty = 3, lwd = 1 );
      if( jj  ==  1 ) {
            abline( lm1_bt, col = "red" )
          } else if(jj  ==  2) {
            abline( lm2_bt, col = "red" )
          } else if(jj  ==  3) {
            abline( lm3_bt, col = "red" )
          } else if(jj  ==  4) {
            abline( lm4_bt, col = "red" )
          } else if(jj  ==  5) {
            abline( lm5_bt, col = "red" )
          } else if(jj  ==  6) {
            abline( lm6_bt, col = "red" )
          } else if(jj  ==  7) {
            abline( lm7_bt, col = "red" )
          } else if(jj  ==  8) {
            abline( lm8_bt, col = "red" )
          } else { 
            abline( lm9_bt, col = "red" )
          }       
            
            
    }
       crow_w <- order( covid )
       plot( covid[crow_w], 
             main = str_c("Ordered by ", main_title[10]), 
             ylab = main_title2[10],  
             ylim = c(2, 22), 
             cex.axis = ftsize2, 
             cex.lab = ftsize 
            );
       grid( lty = 3, lwd = 1 );
       abline( lm10_bt, col = "red" )

par( new = FALSE ); 
dev.off();


# Plots population ordered data,blood types and covid(deaths) 
    xlabels <- c();
    xlabels <- ylabels;
    crow_w2 <- c();  
    
for( kk in c(1:9) ) {
       crow_w2 <- order( aux2[, kk] );
    png( str_c( homedir, homebld, "Bldtype_Log_orderby_", main_title[kk], "_", ll, "_", i, ".png"), width = 800, height = 800 )
    par (new = TRUE);  
    mypar();
    mypar(5, 2); 
       
       for( jj in c(1:9) ) {
       
          par( mar = marg );
          plot( aux2[crow_w2, jj], aux2[crow_w2, kk], 
                main = str_c(main_title[jj], "  X  ", main_title[kk]), 
                xlab = xlabels[jj], 
                ylab = ylabels[kk], 
                cex.axis = ftsize2, 
                cex.lab = ftsize 
              );
          grid( lty = 3, lwd = 1 );
       }
            #Gráficos dos dados ordenados por população e tipos sanguíneos
          plot( covid[crow_w2], aux2[crow_w2, kk], 
                main = str_c(main_title[10], "  X  ", main_title[kk]), 
                xlab = xlabels[10], 
                ylab = ylabels[kk], 
                cex.axis = ftsize2, 
                cex.lab = ftsize 
               );
           grid( lty = 3, lwd = 1 );
           
    
             
   par( new = FALSE ); 
   dev.off();       
}

#Plots from data ordered by deaths
png( str_c( homedir, homebld, "Bldtype_Log_orderby_", main_title[10], "_", ll, "_", i, ".png"), width = 800, height = 800 )
par( new = TRUE ); 

    mypar();
    crow_w2 <- order( covid );
    mypar( 4, 2 );
    for( jj in c(2:9) ) {
         par(mar = marg);
         plot( aux2[, jj], covid, 
               main = str_c( main_title[jj], "  X  ", main_title[10] ), 
               xlab = main_title[jj], 
               ylab = "Deaths", 
               cex.axis = ftsize2, 
               cex.lab = ftsize, 
              );
         grid( lty = 3, lwd = 1 );
#          if(jj  ==  1) {
            crow_teste <- order( aux2[, jj] );
            
            fit <- lm( covid[crow_teste] ~ aux2[crow_teste, jj] ); # lm(y~x)
            
            #lines( aux2[crow_teste, jj], predict(fit, data.frame(x = aux2[crow_teste, jj])), col = "green" );
            
            abline( fit, col = "red", lwd = 1.0 );
    }

         #plot( covid, covid,  #
         #      main = str_c(main_title[10], " X ", main_title[10]), 
         #      xlab = xlabels[10], 
         #      ylab = ylabels[10], 
         #      cex.axis = ftsize2, 
         #      cex.lab = ftsize, 
         #      lty = 2, pch = "o", 
         #    );
        
            #crow_teste <- order( covid );
            #fit <- lm( covid ~ covid ); # lm(y~x)
            #lines( aux2[crow_teste, jj], predict(fit, data.frame(x = aux2[crow_teste, jj])), col = "green" );
            #abline( fit, col = "red" );
          
        grid( lty = 3, lwd = 1 );
     
             
par( new = FALSE ); 
dev.off();   
    
# Plots original data ordered by deaths

png( str_c( homedir, homebld, "Bldtype_OriginalData_", ll, "_", i, ".png"), width = 800, height = 800 )
    
    par( new = TRUE );   
    crow_w2 <- order( covid );
    mypar( 4, 2 );
    for( jj in c(2:9) ) {
        par(mar = marg);
        plot( exp(aux2[crow_w2, jj]), exp (covid[crow_w2] ), 
              main = str_c(main_title[jj], "  X  ", main_title[10]), 
              xlab = main_title[jj], 
              ylab = "Deaths", 
              cex.axis = ftsize2, 
              cex.lab = ftsize 
             );
         grid( lty = 3, lwd = 1 );
    };    
             #plot( covid[crow_w2], covid[crow_w2], 
             #      main = str_c(main_title[10], "  X  ", main_title[10]), 
             #      xlab = xlabels[10], 
             #      ylab = ylabels[10], 
             
             #      cex.axis = ftsize2, 
             #      cex.lab = ftsize 
             #    );
            #grid(lty = 3, lwd = 1);
    
    mypar();
par( new = FALSE ); 
dev.off();


# Plots original data ordered: population, blood types and (covid) deaths x indices

png( str_c( homedir, homebld, "Bldtype_OrinalData_ordered_", ll, "_", i, ".png"), width = 800, height = 800 )

    par( new = TRUE ); 
    mypar();
    mypar( 5, 2 );
    
    for( jj in c(1:9) ) {
         par( mar = marg );
         crow_w2 <- order( aux2[, jj] );
         plot( exp(aux2[crow_w2, jj]), 
               main = str_c(main_title[jj], "  X  ", main_title[10]), 
               ylab = ylabels[10], 
               cex.axis = ftsize2, 
               cex.lab = ftsize 
              );
         grid( lty = 3, lwd = 1 );
    }
         crow_w2 <- order( covid );
         plot( exp(covid[crow_w2]), 
               main = str_c(main_title[10], "  X  ", main_title[10]), 
               ylab = ylabels[10], 
               cex.axis = ftsize2, 
               cex.lab = ftsize 
             );
         grid( lty = 3, lwd = 1 );
             
par( new = FALSE ); 
dev.off()  

# End logbloodtypes 
