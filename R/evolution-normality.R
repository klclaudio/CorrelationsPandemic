#
# Plot normality tests, saves plots in the folder <homenorm>
#
png( str_c(homedir, homenorm, "Normality_tests", type_stat, l_count, ".png"), width = 500, height = 500 )
   mypar()
   par( mar = c(5, 4, 4, 4) + 0.3 )

   plot( p_values_histogram,
         ylab     = "",
         xlab     = "Days since 5 deaths",
         main     = str_c ("Normality tests for ",i_days," days "), type = "l",
         cex.main = ftsize,
         cex.lab  = ftsizelb,
         cex.axis = ftsizeax,
         ylim     = c(0,1),
         lty      = 1 )
   abline(h = 0.05, col = "grey", lty = 2, lwd = 2)
   grid(lty = 3, lwd = 1)

   mtext( "P-values", cex = ftsize, side = 2, line = 3 )

   par(new = TRUE)
       plot( Wstatistic_histogram,
             xlab     = "",
             ylab     = "",
             axes     = F,
             type     = "l",
             cex.main = ftsize ,
             cex.lab  = ftsizelb,
             cex.axis = ftsizeax,
             ylim     = c(0.7,1),
             lty      = 2 )
       axis( side = 4 )
       mtext( "Wstatistics", cex =  ftsize, side = 4, line = 3)
       legend( "bottomright", legend = c("P-values","W statistics"), lty = c(1,2), bty = "n", cex = 1.0, bg = 'white' )
   par(new =  FALSE)
dev.off()

pval  <- cbind(pval, p_values_histogram)
wstat <- cbind(wstat, Wstatistic_histogram)

# End evolution_normality
