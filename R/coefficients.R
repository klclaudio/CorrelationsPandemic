#
# Calculates coefficients
#
aux_btcovid_i  <- c()
aux2_btcovid_i <- c()

btcovid_frame  <- c()
btcovid2_frame <- c()

   #Least square Fitting
   dimaux2        <- dim(aux2)
   aux_btcovid_i  <- cbind(aux2, covid)
   aux2_btcovid_i <- cbind(aux2, covid)

   aux_btcovid_i  <- cbind( aux_btcovid_i, c(1:dimaux2[1]) )  # col 22
   crow_w2        <- c()

   for( j_count in c(1:9) ) {
      crow_w2                    <- order( aux2[, j_count] )
      aux_btcovid_i[, j_count]  <- aux_btcovid_i [crow_w2, j_count]
      aux2_btcovid_i[, j_count] <- aux2_btcovid_i [crow_w2, j_count]  # ordena por j_count
   }

   crow_w               <- order( covid )
   aux_btcovid_i[, 10] <- aux_btcovid_i [crow_w, 10]  # ordena por numero de mortes

   # nomes_auxbt <- c("Popul", "Op", "Ap", "Bp", "ABp", "On", "An", "Bn", "ABn", "COVID19", "Countries")
   # colnames(aux_btcovid_i) <- nomes_auxbt

   btcovid_frame      <- as.data.frame( aux_btcovid_i )
   btcovid2_frame     <- as.data.frame( aux2_btcovid_i )
   btcovid_frame_line <- btcovid_frame[5:dimaux2[1]-5, ]

   if(l_count == 1 && i_days == 1) {
      attach(btcovid_frame)
      attach(btcovid2_frame)
   }

#  coefs=c()
#  for i_count in c(1:4){
#     coefs[i_count]<-lm( str_c("analise", i_count)~dias5)
#  }
#  print(coefs)

#   lm1_bt <- lm( Popul~Countries)
#   lm2_bt <- lm( Op~Countries )
#   lm3_bt <- lm( Ap~Countries )
#   lm4_bt <- lm( Bp~Countries )
#   lm5_bt <- lm( ABp~Countries )
#   lm6_bt <- lm( On~Countries )
#   lm7_bt <- lm( An~Countries )
#   lm8_bt <- lm( Bn~Countries )
#   lm9_bt <- lm( ABn~Countries )
#   lm10_bt <- lm( COVID19~Countries )

# Fitting  - covid  and blood types
   lm1_bt <- lm( btcovid_frame[, 1]~btcovid_frame[, (dimaux2[2] +2)] )
   lm2_bt <- lm( btcovid_frame[, 2]~btcovid_frame[, (dimaux2[2] +2)] )
   lm3_bt <- lm( btcovid_frame[, 3]~btcovid_frame[, (dimaux2[2] +2)] )
   lm4_bt <- lm( btcovid_frame[, 4]~btcovid_frame[, (dimaux2[2] +2)] )
   lm5_bt <- lm( btcovid_frame[, 5]~btcovid_frame[, (dimaux2[2] +2)] )
   lm6_bt <- lm( btcovid_frame[, 6]~btcovid_frame[, (dimaux2[2] +2)] )
   lm7_bt <- lm( btcovid_frame[, 7]~btcovid_frame[, (dimaux2[2] +2)] )
   lm8_bt <- lm( btcovid_frame[, 8]~btcovid_frame[, (dimaux2[2] +2)] )
   lm9_bt <- lm( btcovid_frame[, 9]~btcovid_frame[, (dimaux2[2] +2)] )

   lm10_bt <- lm( btcovid_frame[, dimaux2[2] +1]~btcovid_frame[, (dimaux2[2] +2)] )

# Fitting  - covid  and blood types
   lm1_aux2 <- lm( btcovid2_frame[, 1]~btcovid2_frame[, 10] )
   lm2_aux2 <- lm( btcovid2_frame[, 2]~btcovid2_frame[, 10] )
   lm3_aux2 <- lm( btcovid2_frame[, 3]~btcovid2_frame[, 10] )
   lm4_aux2 <- lm( btcovid2_frame[, 4]~btcovid2_frame[, 10] )
   lm5_aux2 <- lm( btcovid2_frame[, 5]~btcovid2_frame[, 10] )
   lm6_aux2 <- lm( btcovid2_frame[, 6]~btcovid2_frame[, 10] )
   lm7_aux2 <- lm( btcovid2_frame[, 7]~btcovid2_frame[, 10] )
   lm8_aux2 <- lm( btcovid2_frame[, 8]~btcovid2_frame[, 10] )
   lm9_aux2 <- lm( btcovid2_frame[, 9]~btcovid2_frame[, 10] )

#  lm10_aux2 <- lm(  btcovid2_frame[, 10]~ btcovid2_frame[, 10] )

#  Angular coefficients
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm1_bt)$coef[[2]]) )  # population
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm2_bt)$coef[[2]]) )
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm3_bt)$coef[[2]]) )
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm4_bt)$coef[[2]]) )
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm5_bt)$coef[[2]]) )
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm6_bt)$coef[[2]]) )
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm7_bt)$coef[[2]]) )
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm8_bt)$coef[[2]]) )
   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm9_bt)$coef[[2]]) )

   coeff_a <- rbind( coeff_a, c(l_count, i_days, summary(lm10_bt)$coef[[2]]) )  # covid

# Linear coefficients (intercept)
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm1_bt)$coef[[1]]) )  # population
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm2_bt)$coef[[1]]) )
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm3_bt)$coef[[1]]) )
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm4_bt)$coef[[1]]) )
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm5_bt)$coef[[1]]) )
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm6_bt)$coef[[1]]) )
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm7_bt)$coef[[1]]) )
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm8_bt)$coef[[1]]) )
   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm9_bt)$coef[[1]]) )

   coeff_lin <- rbind( coeff_lin, c(l_count, summary(lm10_bt)$coef[[1]]) ) # covid

#  Angular errors
   errors_a <- rbind( errors_a, c(l_count, summary(lm1_bt)$coef[[4]]) )  # population
   errors_a <- rbind( errors_a, c(l_count, summary(lm2_bt)$coef[[4]]) )
   errors_a <- rbind( errors_a, c(l_count, summary(lm3_bt)$coef[[4]]) )
   errors_a <- rbind( errors_a, c(l_count, summary(lm4_bt)$coef[[4]]) )
   errors_a <- rbind( errors_a, c(l_count, summary(lm5_bt)$coef[[4]]) )
   errors_a <- rbind( errors_a, c(l_count, summary(lm6_bt)$coef[[4]]) )
   errors_a <- rbind( errors_a, c(l_count, summary(lm7_bt)$coef[[4]]) )
   errors_a <- rbind( errors_a, c(l_count, summary(lm8_bt)$coef[[4]]) )
   errors_a <- rbind( errors_a, c(l_count, summary(lm9_bt)$coef[[4]]) )

   errors_a <- rbind( errors_a, c(l_count, summary(lm10_bt)$coef[[4]]) )


# Linear errors (intercept)
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm1_bt)$coef[[3]]) )  # population
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm2_bt)$coef[[3]]) )
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm3_bt)$coef[[3]]) )
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm4_bt)$coef[[3]]) )
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm5_bt)$coef[[3]]) )
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm6_bt)$coef[[3]]) )
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm7_bt)$coef[[3]]) )
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm8_bt)$coef[[3]]) )
   errors_lin <- rbind( errors_lin, c(l_count, summary(lm9_bt)$coef[[3]]) )

   errors_lin <- rbind( errors_lin, c(l_count, summary(lm10_bt)$coef[[3]]) )

# Determinations coefficients
   r2 <- rbind( r2, c(l_count, summary(lm1_bt)$r.squared) )  # population
   r2 <- rbind( r2, c(l_count, summary(lm2_bt)$r.squared) )
   r2 <- rbind( r2, c(l_count, summary(lm3_bt)$r.squared) )
   r2 <- rbind( r2, c(l_count, summary(lm4_bt)$r.squared) )
   r2 <- rbind( r2, c(l_count, summary(lm5_bt)$r.squared) )
   r2 <- rbind( r2, c(l_count, summary(lm6_bt)$r.squared) )
   r2 <- rbind( r2, c(l_count, summary(lm7_bt)$r.squared) )
   r2 <- rbind( r2, c(l_count, summary(lm8_bt)$r.squared) )
   r2 <- rbind( r2, c(l_count, summary(lm9_bt)$r.squared) )

   r2 <- rbind( r2, c(l_count, summary(lm10_bt)$r.squared) )

#
coeff_ll  <- rbind( coeff_ll, c(i_days, summary(lm10_bt)$coef[[2]], summary(lm10_bt)$coef[[1]]) )
coeff_aux <- rbind( coeff_aux, c(l_count, i_days, summary(lm10_bt)$coef[[2]], summary(lm10_bt)$coef[[1]]) )


# End coefficients