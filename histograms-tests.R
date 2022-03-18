#
# Calculates normality test with shapiro wilk and plots histograms         
#

ftsizeax = 1.3;
  ftsizelb = 1.5;
    ftsize = 1.5;

         
crow_d <- order(covid) 
  dados <- covid[crow_d]
 
png( str_c(homedir, homehist, "Normality", type_stat, "_", ll,"_", i, ".png"), width = 500, height = 500 )
 
      par( mar = c(6, 6, 6, 1) )
      hist( dados,
            main = str_c (" Histogram ", "(", i, " days)"), 
            xlab = str_c(type_stat, " Covid "),
            cex.main = ftsize, 
            cex.lab = ftsizelb, 
            cex.axis = ftsizeax,
            breaks = 10
           );
       
     teste_N <- shapiro.test( dados );
     p_values_histogram[i] <- teste_N$p.value;
     Wstatistic_histogram[i] <- as.numeric( teste_N$statistic );

dev.off()

# End histograms_tests
