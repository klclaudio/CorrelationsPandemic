#
# Fitting numbers deaths since 5 deaths
#
aux_covid_ll   <- cbind( covid_ll,  c(1:N_i) )
nomes_analises <- c("analise1", "analise2", "analise3", "analise4", "dias5")
colnames (aux_covid_ll) <- nomes_analises

x_label <- x_label_graph_f(interval_days)

x_low <- 14/interval_days
x_sup <- 35/interval_days

covid_ll_frame   <- as.data.frame( aux_covid_ll )
covid_ll_frame50 <- covid_ll_frame[x_low:x_sup, ]
attach( covid_ll_frame50 )

   lm1 <- lm( analise1~dias5 )
   lm2 <- lm( analise2~dias5 )
   lm3 <- lm( analise3~dias5 )
   lm4 <- lm( analise4~dias5 )
   summary( lm1 )
   lm( analise1~dias5 )$dias5 / lm( analise3~dias5 )$dias5
   lm( analise2~dias5 )$dias5 / lm( analise3~dias5 )$dias5
   lm( analise3~dias5 )$dias5 / lm( analise3~dias5 )$dias5
   lm( analise4~dias5 )$dias5 / lm( analise3~dias5 )$dias5

percents_covid_ll <- c()
somac_ll          <- c()
for( i_count in c(1:4) ) {
   percents_covid_ll[i_count] <- sum( covid_ll[, i_count])/sum(covid_ll[, 1] )
   somac_ll[i_count] <- sum( covid_ll[, i_count] )
}

write.csv( covid_ll, str_c(homedir, homedaily, "total_covid_14.csv") )

# Plots daily deaths
png( str_c( homedir, homedaily, "daily_deaths.png"), width  =  700, height  =  700 )
   mypar( 2, 2 )
   par(new  =  TRUE)
   par( mar <- c(4, 4, 4, 4) )

   idg <- c("(1)", "(2)", "(3)", "(4)")

   for( i_count in c(1:ndata) ) {
      ylim_sup <- max(covid_ll[,i_count])
      plot( covid_ll[, i_count],
            main      =  str_c("Total daily deaths ", idg[i_count]),
            xlab      =  str_c(x_label," since five deaths"),
            ylab      =  "Deaths",
            cex.main  =  ftsize,
            cex.lab   =  ftsizelb,
            cex.axis  =  ftsizeax,
            ylim      =  c(0, ylim_sup),
            type      =  "l",
            lty       =  1,
            lwd       =  2 )
      grid(lty   =  3, lwd  =  1)
   }
   mypar()
   par(new  =  FALSE)
dev.off()

png( str_c( homedir, homedaily, "daily_deaths_log.png"), width  =  700, height  =  700 )
   mypar(2, 2)
   par( new  =  TRUE )
   par( mar  =  c(4, 4, 4, 4) )

   for( i_count in c(1:ndata) ) {
      ylim_sup <- max(ceiling( log(covid_ll[,i_count]) ))
      plot( log(covid_ll[, i_count]),
            main     = str_c("Total daily log deaths ", idg[i_count]),
            xlab     =  str_c(x_label, " since five deaths"),
            ylab     = "Deaths",
            ylim     = c(2.5, ylim_sup),
            cex.main =  ftsize,
            cex.lab  = ftsizelb,
            cex.axis = ftsizeax,
            type     = "l",
            lty      = 1,
            lwd      = 2 )
      grid(lty  = 3, lwd = 1)
   }
   mypar()
   par( new = FALSE )
dev.off()

png( str_c( homedir, homedaily, "daily_deaths50.png"), width = 700, height = 700 )
   mypar(2, 2)
   par(new = TRUE)
   par( mar = c(4, 4, 4, 4) )

   idg <- c("(1)", "(2)", "(3)", "(4)")
   min_x = ceiling(50/interval_days)
   ylim_sup = max(covid_ll[1:min(c(min_x, N_i)),] )
   for( i_count in c(1:ndata) ) {
      plot( covid_ll[1:min(c(min_x, N_i)), i_count],
            main     = str_c("Total daily deaths ", idg[i_count]),
            xlab     =  str_c(x_label, " since five deaths"),
            ylab     = "Deaths",
            ylim     = c(0, ylim_sup),
            cex.main = ftsize,
            cex.lab  = ftsizelb,
            cex.axis = ftsizeax,
            type     = "l",
            lty      = 1,
            lwd      = 2 )
      grid(lty  = 3, lwd = 1)

      if( i_count == 1 ) {
         abline( lm1, col = "red" )
      } else if( i_count == 2 ){
         abline( lm2, col = "red" )
      } else if( i_count == 3 ){
         abline( lm3, col = "red" )
      } else{
         abline( lm4, col = "red" )
      }
   }
    mypar()
    par(new = FALSE)
dev.off()

png( str_c( homedir, homedaily, "daily_deaths_log.png"), width = 700, height = 700 )
   mypar(2, 2)
   par(new = TRUE)
   par(mar <- c(4, 4, 4, 4))

   for( i_count in c(1:ndata) ) {
      ylim_sup <- ceiling ( log( covid_ll[1: min(c(80, N_i)),i_count]) )
      plot( log( covid_ll[1: min(c(80, N_i)), i_count] ),
            main     = str_c("Total daily log deaths ", idg[i_count]),
            xlab     =  str_c(x_label, " since five deaths"),
            ylab     = "Deaths",
            ylim     = c(2.5, 13),
            cex.main = ftsize,
            cex.lab  = ftsizelb,
            cex.axis = ftsizeax,
            type     = "l",
            lty      = 1,
            lwd      = 2 )
      grid(lty = 3, lwd = 1)
   }
   mypar()
   par(new = FALSE)
dev.off()

# End daily_deaths
