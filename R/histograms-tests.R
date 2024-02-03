#
#Normality test with Shapiro Wilk
#Shapiro, S. S., and Wilk, M. B. (1965), An analysis of variance test for normality (complete samples), Biometrika 52, 591–611.
#
ftsizeax <- 1.3
ftsizelb <- 1.5
ftsize   <- 1.5

crow_d <- order(covid)
dados  <- covid[crow_d]
png( str_c(homedir, homehist, "Normality", type_stat, "_", l_count,"_", i_days, ".png"), width = 500, height = 500 )
   par( mar = c(6, 6, 6, 1) )
   hist( dados,
         main     = str_c (" Histogram ", "(", i_days, " days)"),
         xlab     = str_c(type_stat, " Covid "),
         cex.main = ftsize,
         cex.lab  = ftsizelb,
         cex.axis = ftsizeax,
         breaks   = 10 )
   teste_N <- shapiro.test( dados )

   p_values_histogram[i_days]   <- teste_N$p.value
   Wstatistic_histogram[i_days] <- as.numeric( teste_N$statistic )
dev.off()

# End histograms_tests
