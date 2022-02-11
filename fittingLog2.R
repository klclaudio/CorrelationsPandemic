#
# Fitting correlations and saves results files in the folder <homefit>
#

library (tidyverse);
  library (cluster);
    library (factoextra);
      library (gridExtra);
        library (dplyr);
         library (rafalib);


correlacoes1 <- read.csv( vfilesout[1] ) #"bloodcovid_correls_Log.csv"
  correlacoes2 <- read.csv( vfilesout[2] ) #"bloodcovidOutCH_I_correls_Log.csv"
    correlacoes3 <- read.csv( vfilesout[3] ) #"bloodcovid_outlarger_correls_Log.csv" 
      correlacoes4 <- read.csv( vfilesout[4] ) #"bloodcovidlarger_correls_Log.csv"


# Correlations : 
coefi<-c();

nx = N_i - 40
   x <- c(1:nx)/100;         #vector fitting
     xx <- c(1:(N_i))/100;      #plot axis
       q <- x;
   
ftsizeax = 1.3;
  ftsizelb = 1.5;
    ftsize=1.5;
xlimite = c(0.01,(N_i/100));

# 
#     Plots
#


maxresidualspop <- c();
coefipop<-c();
stats_residualspop <- c();
stderrorspop  <- c();


# ftsizeax = 1.3;
# ftsizelb = 1.5;
# ftsize=1.5;
# xlimite = c(0.01,5.00);
color_l = c("red","blue","black","green3");
px_wd_hg <- 700;  #size pixel for pictures  


#Plot populations
png( str_c(homedir, homefit,"Corr_Populations_fit_", nx ,"_",px_wd_hg,".png"), width = px_wd_hg, height = px_wd_hg );
mypar(2,2); 
par(mar = c(4, 4, 4, 4));
for (iiii in c(1:4)){

  if (iiii != 4) {
     ylimite = c(-0.1,1)
  } else{
    ylimite = c(-1,1)
  }

    if(iiii==1){correlacoes <- correlacoes1
      }else if(iiii==2){correlacoes <- correlacoes2
      }else if(iiii==3){correlacoes <- correlacoes3
      }else if(iiii==4){correlacoes <- correlacoes4
    }


   plot( xx,correlacoes[1:N_i,2], main=str_c("Population Analysis - ",iiii), ylab="Correlations", xlab="Days since five deaths (x 10²)",
         xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab = ftsizelb, cex.axis = ftsizeax, pch=1, cex=0.7, lwd=0.5, lty=1) #Population
   grid(lty=3, lwd=1)
    
  par(new=TRUE)
   plot( xx,correlacoes[1:N_i,2], main="", ylab="", xlab="",
          xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab = ftsizelb, cex.axis = ftsizeax, pch=1, cex=0.7, lwd=0.5, lty=1) #Population
         
      model <- lm(correlacoes[1:nx,2]~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10)+ I(x^11) );
      predicted.intervals <- predict( model,data.frame(x=q),interval='confidence', level=0.95 );
      par( new = TRUE);
      plot( q,predicted.intervals[,1],ylab="", xlab="",
             xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[1], type = "l", lty=1, lwd=2 ); 
      maxresidualspop <- rbind( maxresidualspop, max(model[["residuals"]]));
      
      residuospop<-model[[ "residuals" ]];
      mediapop<-sum(abs(residuospop))/nx;   
      desviopop<-sd(abs(residuospop));
      stats_residualspop <- rbind( stats_residualspop, c(mediapop,desviopop) );
      out_modelpop=summary(model)$coef;
      stderrorspop <- rbind( stderrorspop,out_modelpop[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefipop <-  rbind( coefipop, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
      
      
}
dev.off();


#plot blood correlations

maxresiduals <- c();
stats_residuals <- c();
coefi<-c();
stderrors  <- c();


# ftsizeax = 1.3;
# ftsizelb = 1.5;
# ftsize=1.5;
# xlimite = c(0.01,5.00);
color_l = c("red","blue","black","cyan2");


for (iiii in c(1:4)){

if (iiii != 4) {
    ylimite = c(-0.1,1)
} else{
    ylimite = c(-1,1)
}

    if(iiii==1){correlacoes <- correlacoes1
      }else if(iiii==2){correlacoes <- correlacoes2
      }else if(iiii==3){correlacoes <- correlacoes3
      }else if(iiii==4){correlacoes <- correlacoes4
    }

png(str_c(homedir, homefit,"CorrelationsCovidABO_Log_fit",iiii,"_",nx,"_",px_wd_hg,".png"), width = px_wd_hg, height = px_wd_hg);

 mypar(2,2); par(new = FALSE);
 par(mar = c(4, 4, 4, 4))
 plot(xx,correlacoes[1:N_i,4], main="ABO System and Rh+ (a)", ylab="Correlations", xlab="Days since five deaths (x 10²)",
     ylim = ylimite, xlim = xlimite, cex.main = ftsize , cex.lab = ftsizelb, cex.axis = ftsizeax, pch=1, type = "l",  cex=0.01, lwd=0.5, lty=2); #O+
     if (iiii != 4){
      legend("topleft",legend=c("A +","O +","B +","AB +"), col = color_l, bty="n", lwd=2, lty=c(1,2,3,6),cex=1.0);
   }else{
      legend("bottomright",legend=c("A +","O +","B +","AB +"), col = color_l, bty="n", lwd=2, lty=c(1,2,3,6),cex=1.0);
   }
   grid(lty=3, lwd=1); #pch = c(10,12,9,8,1,0,5,4))
   
   #fitting
      model <- lm(correlacoes[1:nx,4] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict( model,data.frame(x=q),interval='confidence', level=0.95 );
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",ylim = ylimite, xlim = xlimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax,  type = "l", col = color_l[2], lty=2, lwd=1.5 );
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
      stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
      
   
   par(new = TRUE)
   plot(xx,correlacoes[1:N_i,6], ylab="", xlab="", xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb,
        cex.axis= ftsizeax, type="l", pch=1, cex=0.7, lwd=0.5,lty=1); #A+
   grid(lty=3, lwd=1)
   #fitting
      model <- lm(correlacoes[1:nx,6] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[1],lty=1,lwd=1.5)
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
  
   par(new = TRUE)
   plot(xx,correlacoes[1:N_i,8], ylab="", xlab="", xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=3);  #B+
   #fitting
      model <- lm(correlacoes[1:nx,8] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[3],lty=3,lwd=1.5)
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
      stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
   
   par(new = TRUE)
   plot(xx,correlacoes[1:N_i,10], ylab="", xlab="", xlim = xlimite, ylim = ylimite, cex.main = ftsize, cex.axis= ftsizeax,  pch=1, type="l",  cex=0.01, lwd=0.5, lty=6);  #AB+
      #fitting
      model <- lm(correlacoes[1:nx,10] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[4], lty=6, lwd=1.5)
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
   
    #mtext ( "P-values", cex = ftsizeax*0.9, side = 2, line = 3) 
      par (new = FALSE); 
  
  
 plot(xx,correlacoes[1:N_i,12], main="ABO System and Rh- (b)", ylab="Correlations", xlab="Days since five deaths (x 10²)",
     xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab = ftsizelb,  cex.axis = ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=2) #O-
  
   
   if (iiii != 4){
      legend("topleft", legend=c("A -","O -","B -","AB -"), col = color_l, lty=c(1,2,3,6), bty="n", lwd=2, cex=1.0, bg='white');
   }else{
     legend("bottomright", legend=c("A -","O -","B -","AB -"), col = color_l, lty=c(1,2,3,6), bty="n", lwd=2, cex=1.0, bg='white');
   }
   grid(lty=3, lwd=1)
      model <- lm(correlacoes[1:nx,12] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[2], lty=2, lwd=1.5)
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
   
   
   
   par(new = TRUE)
   plot(xx,correlacoes[1:N_i,14], ylab="", xlab="",xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=1); #A-
      model <- lm(correlacoes[1:nx,14] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[1], lty=1, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
      
   par(new = TRUE)
   plot(xx,correlacoes[1:N_i,16], ylab="", xlab="", xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab= ftsizelb,  cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=3); #B-
      model <- lm(correlacoes[1:nx,16] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[3], lty=3, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresidualbottomrights <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
      
   par(new = TRUE)
   plot(xx,correlacoes[1:N_i,18], ylab="", xlab="", xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=6); #AB-
      model <- lm(correlacoes[1:nx,18] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[4], lty=6, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
    
  par(new = FALSE)
 plot(xx,correlacoes[1:N_i,20],  main="ABO System (c)", ylab="Correlations",xlab="Days since five deaths (x 10²)",
     xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab= ftsizelb,  cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=2)
     

    if (iiii != 4){
        legend("topleft", legend=c("A","O","B","AB"), col = color_l, lty=c(1,2,3,6), bty="n", lwd=2, cex=1.0, bg='white');
    }else{
        legend("bottomright", legend=c("A","O","B","AB"), col = color_l, lty=c(1,2,3,6), bty="n", lwd=2, cex=1.0, bg='white');
   }
     
      model <- lm(correlacoes[1:nx,20] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[2], lty=2, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
   
   
   grid(lty=3, lwd=1)
  
   par(new = TRUE)

   plot(xx,correlacoes[1:N_i,22], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb,  cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=1);
  
       
      model <- lm(correlacoes[1:nx,22] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[1], lty=1, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
       par(new = TRUE)

#if (iiii != 4) 
   plot(xx,correlacoes[1:N_i,24], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=3);
     
      model <- lm(correlacoes[1:nx,24] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[3], lty=3, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
   par(new = TRUE)

   plot(xx,correlacoes[1:N_i,26], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.5, lty=6);
      
      model <- lm(correlacoes[1:nx,26] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col = color_l[4], lty=4, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
 par(new = FALSE)
 
 plot(xx,correlacoes[1:N_i,28],  main="Rhesus(Rh) System (d)", ylab="Correlations", xlab="Days since five deaths (x 10²)",
      xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab= ftsizelb,  cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.4, lty=2);
    
      model <- lm(correlacoes[1:nx,28] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );;
      par(new = TRUE)
      plot( q,predicted.intervals[,1], ylab="", xlab="",xlim = xlimite, ylim = ylimite, cex.main = ftsize , cex.lab= ftsizelb, cex.axis= ftsizeax, type = "l", col='brown', lty=1, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
   
   if (iiii != 4){
        legend("topleft", legend=c("Rh +","Rh -"), col = c("brown","green4") ,lty=c(1,2), bty="n", lwd=2, cex=1.0,bg='white');
   }else{
        legend("bottomright", legend=c("Rh +","Rh -"), col = c("brown","green4"),lty=c(1,2), bty="n", lwd=2, cex=1.0,bg='white');
   }
   grid(lty=3, lwd=1);
  
   par(new = TRUE);
   plot(xx,correlacoes[1:N_i,30], ylab="", xlab="", xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab= ftsizelb,  cex.axis= ftsizeax, pch=1, type="l",  cex=0.01, lwd=0.4, lty=2);
    
    model <- lm(correlacoes[1:nx,30] ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) );
    #model <- lm(correlacoes[1:nx,30] ~ poly(q,11) );
      predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.95)
#      coefi <- rbind( coefi, model[["coefficients"]] );
      par(new = TRUE);
      par(new = TRUE)
      plot( q,predicted.intervals[,1],ylab="", xlab="", xlim = xlimite, ylim = ylimite,   cex.main = ftsize , cex.lab= ftsizelb,  cex.axis= ftsizeax, type = "l",col='green4', lty=2, lwd=1.5);
      residuos<-model[[ "residuals" ]];
      media<-sum(abs(residuos))/nx;   
      desvio<-sd(abs(residuos));
      stats_residuals <- rbind( stats_residuals, c(media,desvio) );
      maxresiduals <- rbind( maxresiduals, max(residuos[5:nx]) );
      out_model=summary(model)$coef;
            stderrors <- rbind( stderrors,out_model[,2] );
      
      ResidualStandardError<-sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  );
      CoeficienteDeterminacao<-summary(model)$r.squared;
      coefi <-  rbind( coefi, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) );
   
par(new = FALSE);
dev.off(); 
 
} #End loop iiii



# Statistics from countries sets (1,2,3,4)
# c( "O.","A.","B.","AB.",   "O..1","A..1","B..1","AB..1",  "Correl_O","Correl_A","Correl_B","Correl_AB",  "Correl_rhp","Correl_rhn");

write.csv( coefipop, str_c(homedir, homefit, "stats_correlationspop", type_stat , "_", nx , ".csv") );

   write.csv( coefi, str_c(homedir, homefit, "stats_correlations", type_stat ,"_", nx , ".csv") );

      write.csv( stats_residuals, str_c(homedir, homefit,  "stats_fit_residuals", type_stat , "_", nx , ".csv") );

         write.csv( stderrors, str_c(homedir, homefit,  "stats_errors_coefi", type_stat , "_", nx , ".csv") )

#End fitting
 


 
