#
# Create pictures since 5 deaths from the end.
#
xlimite <- c(8, 20.5)
ylimite <- c(1.5, 13)
crow_w  <- c()

x_label <- x_label_graph_f(interval_days)


png( str_c(homedir, homeCounEvol ,"CovidCountries", type_stat, "_", l_count, "_", i_days, ".png"), width = 500, height = 500 )
   #crow_w = order(covid)
   par( mar = c(6, 6, 6, 6) )
   plot( covid, main = str_c(i_days ," ", x_label, " since five deaths"),
         ylab     = "Covid",
         xlab     = "Countries",
         ylim     = c( 1.5, max(ceiling(covid)) ),
         cex.axis = ftsizeax,
         cex.lab  = ftsizelb,
         cex.main = ftsize
        )
dev.off()

# End covid_evolution
