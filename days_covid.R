#Least square Fitting

  aux_covid_ll = cbind(covid_ll, c(1:N_i));
  nomes_analises = c("analise1","analise2","analise3","analise4", "dias5");
  colnames(aux_covid_ll) = nomes_analises
  
  covid_ll_frame <- as.data.frame(aux_covid_ll)
  covid_ll_frame50 = covid_ll_frame[14:35,]
  attach(covid_ll_frame50)
  
  head(covid_ll_frame)
  
#  coefs=c();
#  for iii in c(1:4){
#     coefs[iii]<-lm(str_c("analise",iii)~dias5) 
#  }
#  print(coefs)
     
     lm1<-lm(analise1~dias5) 
     lm2<-lm(analise2~dias5)
     lm3<-lm(analise3~dias5)
     lm4<-lm(analise4~dias5)

     summary(lm1)
     
     lm(analise1~dias5)$dias5/lm(analise3~dias5)$dias5
     lm(analise2~dias5)$dias5/lm(analise3~dias5)$dias5
     lm(analise3~dias5)$dias5/lm(analise3~dias5)$dias5
     lm(analise4~dias5)$dias5/lm(analise3~dias5)$dias5
     
     
     percents_covid_ll<-c();
     somac_ll <- c();
     
     for ( iii in c(1:4)){
 
         percents_covid_ll[iii]= sum(covid_ll[,iii])/sum(covid_ll[,1])
         somac_ll[iii] = sum(covid_ll[,iii])
 
     }

write.csv(covid_ll, str_c( homedir, homedaily,"total_covid_14.csv") );


# Gráfico do número diários de Covid

png(str_c( homedir, homedaily,"daily_deaths.png"), width = 700, height = 700);
mypar(2,2); par(new = TRUE);
par(mar = c(4, 4, 4, 4));

idg = c("(1)", "(2)", "(3)","(4)");
iii=1
for (iii in c(1:ndata)){

   plot(covid_ll[,iii],
        main=str_c("Total daily deaths ",idg[iii]),
        xlab="Days since five deaths",
        ylab="Deaths",cex.main = ftsize , cex.lab = ftsizelb,  cex.axis = ftsizeax,
        ylim= c(0,16000),
        type="l",
        lty=1, 
        lwd=2
        ); 
  grid(lty=3, lwd=1);
        
        
}
mypar();
par(new = FALSE);
dev.off();


png(str_c( homedir, homedaily,"daily_deaths_log.png"), width = 700, height = 700);
mypar(2,2); par(new = TRUE);
par(mar = c(4, 4, 4, 4));

iii=1
for (iii in c(1:ndata)){

   plot(log(covid_ll[,iii]),
        main=str_c("Total daily log deaths ",idg[iii]),
        xlab="Days since five deaths",
        ylab="Deaths",
        ylim= c(2.5,10),cex.main = ftsize , cex.lab = ftsizelb,  cex.axis = ftsizeax,
        type="l",
        lty=1, 
        lwd=2
        ); 
  grid(lty=3, lwd=1);
        
        
}
mypar();
par(new = FALSE);
dev.off();


# Gráfico do número diários de Covid

png(str_c( homedir, homedaily,"daily_deaths50.png"), width = 700, height = 700);
mypar(2,2); par(new = TRUE);
par(mar = c(4, 4, 4, 4));

idg = c("(1)", "(2)", "(3)","(4)");
iii=1
for (iii in c(1:ndata)){
 
   plot(covid_ll[1:min(c(50,N_i)),iii],
        main=str_c("Total daily deaths ",idg[iii]),
        xlab="Days since five deaths",
        ylab="Deaths",
        ylim= c(0,10000), cex.main = ftsize , cex.lab = ftsizelb,  cex.axis = ftsizeax,
        type="l",
        lty=1, 
        lwd=2
        ); 
  grid(lty=3, lwd=1);
  
  if (iii == 1){
    abline(lm1,col = "red" )
    } else if(iii==2){
      abline(lm2, col = "red")
    } else if(iii==3){
      abline(lm3, col = "red")
    } else{
      abline(lm4,col = "red")
    }  
            
}
mypar();
par(new = FALSE);
dev.off();

png(str_c( homedir, homedaily,"daily_deaths_log.png"), width = 700, height = 700);
mypar(2,2); par(new = TRUE);
par(mar = c(4, 4, 4, 4));

iii=1
for (iii in c(1:ndata)){

   plot(log( covid_ll[1: min(c(80,N_i)),iii] ),
        main=str_c("Total daily log deaths ",idg[iii]),
        xlab="Days since five deaths",
        ylab="Deaths",
        ylim= c(2.5,10),cex.main = ftsize , cex.lab = ftsizelb,  cex.axis = ftsizeax,
        type="l",
        lty=1, 
        lwd=2
        ); 
  grid(lty=3, lwd=1);
        
        
}
mypar();
par(new = FALSE);
dev.off()

