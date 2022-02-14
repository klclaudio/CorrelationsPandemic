#
# Calculates coefficients
#

 aux_btcovid_i <- c();
   aux2_btcovid_i <- c();
 btcovid_frame  <- c();                   
   btcovid2_frame  <- c();  
  
            #Least square Fitting
            dimaux2 <- dim( aux2 );
            aux_btcovid_i <- cbind( aux2, aux_covid );
            aux2_btcovid_i <- cbind( aux2, aux_covid );
            
            aux_btcovid_i <- cbind( aux_btcovid_i, c(1:dimaux2[1]) ); # col 22
            crow_w2 <- c();
            
            for( jjj in c(1:9) ) {
               crow_w2 <- order( aux2[, jjj] );
               aux_btcovid_i [, jjj] <- aux_btcovid_i [crow_w2, jjj];
                 aux2_btcovid_i [, jjj] <- aux2_btcovid_i [crow_w2, jjj]; # ordena por jjj
            }
            
            
            crow_w <- order( aux_covid );
               aux_btcovid_i [, 10] <- aux_btcovid_i [crow_w, 10]; # ordena por numero de mortes 
                
#            nomes_auxbt <- c("Popul", "Op", "Ap", "Bp", "ABp", "On", "An", "Bn", "ABn", "COVID19", "Countries");
#            colnames(aux_btcovid_i) <- nomes_auxbt;
  
            btcovid_frame <- as.data.frame( aux_btcovid_i );
            btcovid2_frame <- as.data.frame( aux2_btcovid_i );
            btcovid_frame_line <- btcovid_frame [5:dimaux2[1]-5, ];
            
            if( ll==1 & i==1 ) { 
               attach( btcovid_frame );
                attach( btcovid2_frame );
            }
  
            #head(btcovid_frame)
  
#  coefs=c();
#  for iii in c(1:4){
#     coefs[iii]<-lm( str_c("analise", iii)~dias5) 
#  }
#  print(coefs)
     
#            lm1_bt <- lm( Popul~Countries);
#            lm2_bt <- lm( Op~Countries );
#            lm3_bt <- lm( Ap~Countries );
#            lm4_bt <- lm( Bp~Countries );
#            lm5_bt <- lm( ABp~Countries );
#            lm6_bt <- lm( On~Countries );
#            lm7_bt <- lm( An~Countries );
#            lm8_bt <- lm( Bn~Countries );
#            lm9_bt <- lm( ABn~Countries );
#            lm10_bt <- lm( COVID19~Countries );

#fitting  - covid  and blood types  
            lm1_bt <- lm( btcovid_frame[, 1]~btcovid_frame[, 11] );
            lm2_bt <- lm( btcovid_frame[, 2]~btcovid_frame[, 11] );
            lm3_bt <- lm( btcovid_frame[, 3]~btcovid_frame[, 11] );
            lm4_bt <- lm( btcovid_frame[, 4]~btcovid_frame[, 11] );
            lm5_bt <- lm( btcovid_frame[, 5]~btcovid_frame[, 11] );
            lm6_bt <- lm( btcovid_frame[, 6]~btcovid_frame[, 11] );
            lm7_bt <- lm( btcovid_frame[, 7]~btcovid_frame[, 11] );
            lm8_bt <- lm( btcovid_frame[, 8]~btcovid_frame[, 11] );
            lm9_bt <- lm( btcovid_frame[, 9]~btcovid_frame[, 11] );
            
            lm10_bt <- lm( btcovid_frame[, 10]~btcovid_frame[, 11] );
            
#fitting  - covid  and blood types            
            lm1_aux2 <- lm(  btcovid2_frame[, 1]~btcovid2_frame[, 10] );
            lm2_aux2 <- lm(  btcovid2_frame[, 2]~btcovid2_frame[, 10] );
            lm3_aux2 <- lm(  btcovid2_frame[, 3]~btcovid2_frame[, 10] );
            lm4_aux2 <- lm(  btcovid2_frame[, 4]~btcovid2_frame[, 10] );
            lm5_aux2 <- lm(  btcovid2_frame[, 5]~btcovid2_frame[, 10] );
            lm6_aux2 <- lm(  btcovid2_frame[, 6]~btcovid2_frame[, 10] );
            lm7_aux2 <- lm(  btcovid2_frame[, 7]~btcovid2_frame[, 10] );
            lm8_aux2 <- lm(  btcovid2_frame[, 8]~btcovid2_frame[, 10] );
            lm9_aux2 <- lm(  btcovid2_frame[, 9]~btcovid2_frame[, 10] );
            
#            lm10_aux2 <- lm(  btcovid2_frame[, 10]~ btcovid2_frame[, 10] );         
            
#  angular coefficients
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm1_bt)$coef[[2]]) ); # population
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm2_bt)$coef[[2]]) );
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm3_bt)$coef[[2]]) );
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm4_bt)$coef[[2]]) );
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm5_bt)$coef[[2]]) );
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm6_bt)$coef[[2]]) );
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm7_bt)$coef[[2]]) );
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm8_bt)$coef[[2]]) );
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm9_bt)$coef[[2]]) );
            
            coeff_a <- rbind( coeff_a, c(ll, i, summary(lm10_bt)$coef[[2]]) ); # covid
        
# linear coefficients (intercept)
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm1_bt)$coef[[1]]) ); # population
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm2_bt)$coef[[1]]) );
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm3_bt)$coef[[1]]) );
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm4_bt)$coef[[1]]) );
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm5_bt)$coef[[1]]) );
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm6_bt)$coef[[1]]) );
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm7_bt)$coef[[1]]) );
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm8_bt)$coef[[1]]) );
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm9_bt)$coef[[1]]) );
            
            coeff_lin <- rbind( coeff_lin, c(ll, summary(lm10_bt)$coef[[1]]) );          

#  angular errors
            errors_a <- rbind( errors_a, c(ll, summary(lm1_bt)$coef[[4]]) ); # population
            errors_a <- rbind( errors_a, c(ll, summary(lm2_bt)$coef[[4]]) );
            errors_a <- rbind( errors_a, c(ll, summary(lm3_bt)$coef[[4]]) );
            errors_a <- rbind( errors_a, c(ll, summary(lm4_bt)$coef[[4]]) );
            errors_a <- rbind( errors_a, c(ll, summary(lm5_bt)$coef[[4]]) );
            errors_a <- rbind( errors_a, c(ll, summary(lm6_bt)$coef[[4]]) );
            errors_a <- rbind( errors_a, c(ll, summary(lm7_bt)$coef[[4]]) );
            errors_a <- rbind( errors_a, c(ll, summary(lm8_bt)$coef[[4]]) );
            errors_a <- rbind( errors_a, c(ll, summary(lm9_bt)$coef[[4]]) );
            
            errors_a <- rbind( errors_a, c(ll, summary(lm10_bt)$coef[[4]]) ); 
            
            
# linear errors (intercept)
           errors_lin <- rbind( errors_lin, c(ll, summary(lm1_bt)$coef[[3]]) ); # population
           errors_lin <- rbind( errors_lin, c(ll, summary(lm2_bt)$coef[[3]]) );
           errors_lin <- rbind( errors_lin, c(ll, summary(lm3_bt)$coef[[3]]) );
           errors_lin <- rbind( errors_lin, c(ll, summary(lm4_bt)$coef[[3]]) );
           errors_lin <- rbind( errors_lin, c(ll, summary(lm5_bt)$coef[[3]]) );
           errors_lin <- rbind( errors_lin, c(ll, summary(lm6_bt)$coef[[3]]) );
           errors_lin <- rbind( errors_lin, c(ll, summary(lm7_bt)$coef[[3]]) );
           errors_lin <- rbind( errors_lin, c(ll, summary(lm8_bt)$coef[[3]]) );
           errors_lin <- rbind( errors_lin, c(ll, summary(lm9_bt)$coef[[3]]) );
           
           errors_lin <- rbind( errors_lin, c(ll, summary(lm10_bt)$coef[[3]]) ); 
           
# Coeficiente de determinação
            r2 <- rbind( r2, c(ll, summary(lm1_bt)$r.squared) ); # population
            r2 <- rbind( r2, c(ll, summary(lm2_bt)$r.squared) );
            r2 <- rbind( r2, c(ll, summary(lm3_bt)$r.squared) );
            r2 <- rbind( r2, c(ll, summary(lm4_bt)$r.squared) );
            r2 <- rbind( r2, c(ll, summary(lm5_bt)$r.squared) );
            r2 <- rbind( r2, c(ll, summary(lm6_bt)$r.squared) );
            r2 <- rbind( r2, c(ll, summary(lm7_bt)$r.squared) );
            r2 <- rbind( r2, c(ll, summary(lm8_bt)$r.squared) );
            r2 <- rbind( r2, c(ll, summary(lm9_bt)$r.squared) );
            
            r2 <- rbind( r2, c(ll, summary(lm10_bt)$r.squared) );            
 
               
    coeff_ll <- rbind(  coeff_ll, c(i, summary(lm10_bt)$coef[[2]], summary(lm10_bt)$coef[[1]]) ); 
    coeff_aux <- rbind(  coeff_aux, c(ll, i, summary(lm10_bt)$coef[[2]], summary(lm10_bt)$coef[[1]]) )

    
# End coefficients


