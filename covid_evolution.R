# Create pictures since 5 deaths from the end.

#png(str_c("LogCovidBlood_",Btyp[j],"_",i,".png"), width = 1300, height = 900)
 #xlimite = c(2.0,3),
 #ylimite = c(0.45,2.5),
 
 xlimite = c(8,20.5);
 ylimite = c(1.5,13);
 crow_w <- c();
 crow_w <- order(covid);

png(str_c(homedir,homeCounEvol ,"CovidCountries",type_stat,"_",ll,"_",i,".png"), width = 500, height = 500)

         crow_w = order(covid);
            par(mar = c(6, 6, 6, 6))
         plot(covid,main = str_c(i ," Days since five deaths"), 
              ylab = "Covid",
              xlab = "Countries",
              ylim = c(1.5,13.5),
              cex.axis = ftsizeax, 
              cex.lab  = ftsizelb ,
              cex.main = ftsize,
             );

dev.off(); 
