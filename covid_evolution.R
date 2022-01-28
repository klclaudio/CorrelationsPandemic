# Create pictures since 5 deaths from the end.

 xlimite = c(8,20.5)
 ylimite = c(1.5,13)
 crow_w <- c()


png(str_c(homedir,homeCounEvol ,"CovidCountries",type_stat,"_",ll,"_",i,".png"), width = 500, height = 500)

         #crow_w = order(covid);
            par(mar = c(6, 6, 6, 6))
         plot(covid,main = str_c(i ," Days since five deaths"), 
              ylab = "Covid",
              xlab = "Countries",
              ylim = c(1.5,13.5),
              cex.axis = ftsizeax, 
              cex.lab  = ftsizelb ,
              cex.main = ftsize
             )

dev.off()
