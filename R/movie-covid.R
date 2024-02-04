#
# Create pictures of countries covid evolution.
#
xlimite  <- c(8, 20.5)
ylimite  <- c(1.5, 13)

ftsizeax <- 1.3
ftsizelb <- 1.5
ftsize   <- 1.5

x_label <- x_label_graph_f(interval_days)


png( str_c(homedir, homemov, type_stat,"CovidBlood_ABO+", l_count,"_", i_days, ".png"), width = 500, height = 500 )
   for( j_count in c(2:5) ) {
      crow_j = order( aux2[,j_count] )
      par(mar = c(6, 6, 6, 6))
      plot( aux2[crow_j, j_count], covid[crow_j],
            main     = str_c (" Log deaths x blood type","(", i_days, " ", x_label, ")"),
            ylab     = "Covid",
            xlab     = "Countries population - ABO blood types",
            xlim     = xlimite,
            ylim     = ylimite,
            col      = cores[j_count],
            cex.axis = ftsizeax,
            cex.lab  = ftsizelb,
            cex.main = ftsize,
            pch      = 19 )

      legend( "topleft",
              legend = c("A +","O +","B +","AB +"),
              col    = cores[2:5],
              pt.cex = 2,
              pt.lwd = 1,
              pch    = c(19,19,19,19),
              bty    = "n", cex=1.5,
              bg     = 'white' )
      par(new = TRUE)
   }
dev.off()

png (str_c(homedir,homemov,type_stat,"CovidBlood_ABO-", l_count ,"_", i_days,".png"), width = 500, height = 500 )
   for(j_count in c(6:9)) {
      crow_j = order( aux2[,j_count] )
      par(mar = c(6, 6, 6, 6))
      plot( aux2[crow_j,j_count],covid[crow_j],
            main     = str_c (" Log deaths x blood types"," (", i_days, " ", x_label, ")"),
            ylab     = "Covid",
            xlab     = "Countries population - ABO blood types",
            xlim     = xlimite,
            ylim     = ylimite,
            col      = cores[j_count],
            cex.axis = ftsizeax,
            cex.lab  = ftsizelb,
            cex.main = ftsize,
            pch      = 19 )

      legend( "bottomright",
               legend = c("A -","O -","B -","AB -"),
               col    = cores[6:9],
               pt.cex = 2,
               pt.lwd = 1,
               pch    = c(19,19,19,19),
               bty    = "n",
               cex    = 1.5,
               bg     = 'white' )
      par(new = TRUE)
   }
dev.off()

# End movie_covid