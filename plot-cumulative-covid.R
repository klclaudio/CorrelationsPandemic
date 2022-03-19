#
# Plots cumulative death
#

    png( str_c( homedir, homecum, "CumulativeCovid", type_stat,"_", ll, ".png"), width = 500, height = 500 )
    
           mypar(); 
           par( mar = c(4, 4, 4, 4) );
           plot( cumulative_covid, 
                 main = str_c("Pandemic - ", type_stat," Cumulative Covid"),
                 ylab = str_c("Cumulative ", type_stat, "Covid"), 
                 xlab = str_c(type_stat, "Days since five deaths"),
                 cex.main = ftsize , cex.lab = ftsize, cex.axis = ftsize,
                );
           grid( lty = 3, lwd = 1 );
               
       dev.off()
       
#
