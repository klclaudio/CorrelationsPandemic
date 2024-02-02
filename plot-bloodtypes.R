#
# Plots data blood types and deaths
#
xlimite  <- c(1.5, 21)
ylimite  <- c(1.5, 21)
xlimite2 <- c(1.5, 21)
ylimite2 <- c(1.5, 13.5)

main_title  <- c("Population", "O+", "A+", "B+", "AB +", "O-", "A-", "B -", "AB-", "COVID-19 deaths")
main_title2 <- c("Population", "O+", "A+", "B+", "AB +", "O-", "A-", "B -", "AB-", "Deaths")
ylabels     <- c("log( Population )", "log( O + )", "log( A + )", "log( B + )", "log( AB + )",
                 "log( O - )", "log( A - )", "log( B - )", "log( AB - )", "log( Deaths )" )

ftsize   <- 1.3
ftsize2  <- 1.2
ftsizeax <- 1.3
ftsizelb <- 1.5
ftsize   <- 1.5

crow_w <- order(covid)#indices of ordered vector

# Plots population ordered data,blood types and covid(deaths)x vector indices
png( str_c(homedir, homebld, "Bldtype_Log_orderby_bldtype_", l_count, "_", i_days, ".png"), width = 800, height = 800 )
   par(new = TRUE)
   marg <- c(3, 3, 2, 3)
   mypar()
   mypar( 5, 2 )
   for( j_count in c(1:9) ){
      crow_w <- order( aux2[, j_count] )
      par( mar = marg )
      plot( aux2[crow_w,
            j_count],
            main     = str_c("Ordered by ", main_title[j_count]),
            ylab     = main_title[j_count],
            ylim     = c(2, 22),
            cex.axis = ftsize2,
            cex.lab  = ftsize )
      grid(lty = 3, lwd = 1)

      if (j_count  ==  1){
            abline( lm1_bt, col = "red" )
         } else if(j_count  ==  2){
            abline( lm2_bt, col = "red" )
         } else if(j_count  ==  3) {
            abline( lm3_bt, col = "red" )
         } else if(j_count  ==  4) {
            abline( lm4_bt, col = "red" )
         } else if(j_count  ==  5) {
            abline( lm5_bt, col = "red" )
         } else if(j_count  ==  6) {
            abline( lm6_bt, col = "red" )
         } else if(j_count  ==  7) {
            abline( lm7_bt, col = "red" )
         } else if(j_count  ==  8) {
            abline( lm8_bt, col = "red" )
         } else {
            abline( lm9_bt, col = "red" )
      }
   }# end for
   crow_w <- order( covid )
   plot( covid[crow_w],
         main     = str_c("Ordered by ", main_title[10]),
         ylab     = main_title2[10],
         ylim     = c(2, 22),
         cex.axis = ftsize2,
         cex.lab  = ftsize
       )
   grid(lty = 3, lwd = 1)
   abline(lm10_bt, col = "red")

par(new = FALSE)
dev.off()


# Plots population ordered data,blood types and covid(deaths)
   xlabels <- c()
   xlabels <- ylabels
   crow_w2 <- c()

for( k_count in c(1:9) ){
   crow_w2 <- order( aux2[, k_count] )
   png( str_c( homedir, homebld, "Bldtype_Log_orderby_", main_title[k_count], "_", l_count, "_", i_days, ".png"), width = 800, height = 800 )
      par (new = TRUE)
      mypar()
      mypar(5, 2)

      for( j_count in c(1:9) ){
         par( mar = marg )
         plot( aux2[crow_w2, j_count],
               aux2[crow_w2, k_count],
               main     = str_c(main_title[j_count], "  X  ", main_title[k_count]),
               xlab     = xlabels[j_count],
               ylab     = ylabels[k_count],
               cex.axis = ftsize2,
               cex.lab  = ftsize)
         grid(lty = 3, lwd = 1)
      }
      #Gráficos dos dados ordenados por população e tipos sanguíneos
      plot( covid[crow_w2], aux2[crow_w2, k_count],
            main     = str_c(main_title[10], "  X  ", main_title[k_count]),
            xlab     = xlabels[10],
            ylab     = ylabels[k_count],
            cex.axis = ftsize2,
            cex.lab  = ftsize )
      grid(lty = 3, lwd = 1 )
   par(new = FALSE)
   dev.off()
}

#Plots from data ordered by deaths
png( str_c( homedir, homebld, "Bldtype_Log_orderby_", main_title[10], "_", l_count, "_", i_days, ".png"), width = 800, height = 800 )
   par( new = TRUE )
   mypar()
   crow_w2 <- order( covid )
   mypar( 4, 2 )
   for( j_count in c(2:9) ){
      par(mar = marg)
      plot( aux2[, j_count],
            covid,
            main     = str_c( main_title[j_count], "  X  ", main_title[10] ),
            xlab     = main_title[j_count],
            ylab     = "Deaths",
            cex.axis = ftsize2,
            cex.lab  = ftsize )
      grid(lty = 3, lwd = 1 )
#          if(j_count  ==  1){
      crow_teste <- order( aux2[, j_count] )

      fit <- lm( covid[crow_teste] ~ aux2[crow_teste, j_count] )# lm(y~x)

      #lines( aux2[crow_teste, j_count], predict(fit, data.frame(x = aux2[crow_teste, j_count])), col = "green" )
      abline( fit, col = "red", lwd = 1.0 )
    }
    #plot( covid, covid,  #
    #      main     = str_c(main_title[10], " X ", main_title[10]),
    #      xlab     = xlabels[10],
    #      ylab     = ylabels[10],
    #      cex.axis = ftsize2,
    #      cex.lab = ftsize,
    #      lty = 2, pch = "o",
    #
    #crow_teste <- order( covid )
    #fit <- lm( covid ~ covid )# lm(y~x)
    #lines( aux2[crow_teste, j_count], predict(fit, data.frame(x = aux2[crow_teste, j_count])), col = "green" )
    #abline( fit, col = "red" )
    grid(lty = 3, lwd = 1 )
par(new = FALSE)
dev.off()

# Plots original data ordered by deaths
png( str_c( homedir, homebld, "Bldtype_OriginalData_", l_count, "_", i_days, ".png"), width = 800, height = 800 )
   par( new = TRUE )
   mypar( 4, 2 )
   for( j_count in c(2:9) ){
      par(mar = marg)
      plot( aux2_r[, j_count],
            covid_r[crow_w2],
            main     = str_c(main_title[j_count], "  X  ", main_title[10]),
            xlab     = main_title[j_count],
            ylab     = "Deaths",
            cex.axis = ftsize2,
            cex.lab  = ftsize
           )
      grid(lty = 3, lwd = 1 )
   }
   #plot( covid[crow_w2], covid[crow_w2],
   #      main     = str_c(main_title[10], "  X  ", main_title[10]),
   #      xlab     = xlabels[10],
   #      ylab     = ylabels[10],

   #      cex.axis = ftsize2,
   #      cex.lab = ftsize
   #    )
   #grid(lty = 3, lwd = 1)
   mypar()
par(new = FALSE)
dev.off()


# Plots original data ordered: population, blood types and (covid)deaths x indices

png( str_c( homedir, homebld, "Bldtype_OrinalData_ordered_", l_count, "_", i_days, ".png"), width = 800, height = 800 )
   par(new = TRUE)
   mypar()
   mypar( 5, 2 )
   for( j_count in c(1:9) ){
      par( mar = marg )
      crow_w2 <- order( aux2_r[, j_count] )
      plot( aux2_r[crow_w2, j_count],
            main     = main_title[j_count],
            ylab     = main_title2[j_count],
            cex.axis = ftsize2,
            cex.lab  = ftsize )
      grid(lty = 3, lwd = 1)
   }

   plot( covid_r,
         main     = main_title[10],
         ylab     = main_title2[10],
         cex.axis = ftsize2,
         cex.lab  = ftsize )
         grid(lty = 3, lwd = 1)
   par(new = FALSE)
dev.off()

# End plot_bloodtypes
