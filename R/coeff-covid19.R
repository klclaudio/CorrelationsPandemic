#
# Plots coefficients angular and linear time dependents
#
margin <- c(4, 4, 4, 4)

x  <- c(1:nx) / 100        # used in fitting
xx <- c(1:N_i) / 100       # used from all domain
q  <- x

x_label <- x_label_graph_f(interval_days)

color_l <- c("red","blue","black","green3")

maxresiduals_a   <- c()
coefia           <- c()
stats_residualsa <- c()
stderrorsa       <- c()

maxresiduals_lin   <- c()
coefilin           <- c()
stats_residualslin <- c()
stderrorslin       <- c()

# axis limits
if (tdpm == 0 ){
   ylimitea <- c(0, 1)
   ylimitec <- c(0, 10)
}else{
   if (l_count == 4) {
      ylimitea <- c(0, 0.7)
   }else{
      ylimitea <- c(0, 0.1)
   }
   ylimitec <- c(-4, 8)
}
xlimitec <- c(0,N_i) / 100

# Angular Coefficients
png( str_c( homedir,homecoeff,"CoeffCovid19_Log_Angular",l_count,".png"), width = 500, height = 500 )
   par( new <- TRUE )
   mypar()
   par( mar = margin )
   plot( coeff_ll[, 1] / 100, coeff_ll[, 2],
         main     = str_c( "Temporal Characterization of the COVID-19 (", l_count, ")" ),
         xlim     = xlimitec,
         ylim     = ylimitea,
         xlab     = str_c(x_label, " since five deaths (x 10²)"),
         ylab     = "Angular Coefficients",
         cex.main = ftsize,
         cex.lab  = ftsizelb,
         cex.axis = ftsizeax
       )
   grid(lty = 3, lwd = 2)

   model <- lm( coeff_ll[1:nx,2]~
                 x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) )
   predicted.intervals <- predict( model, data.frame(x = q), interval='confidence', level = 0.95 )

   par( new = TRUE )
   plot( q, predicted.intervals[, 1],
         ylab     ="",
         xlab     ="",
         xlim     = xlimitec,
         ylim     = ylimitea,
         cex.main = ftsize,
         cex.lab  = ftsizelb,
         cex.axis = ftsizeax,
         col      = color_l[1],
         type     = "l",
         lty      = 1,
         lwd      = 2)

   maxresiduals_a   <- rbind( maxresiduals_a, max(model[["residuals"]]) )
   residuosa        <- model[[ "residuals" ]]
   mediaa           <- sum( abs(residuosa) ) / nx
   desvioa          <- sd( abs(residuosa) )

   stats_residualsa <- rbind( stats_residualsa, c(mediaa,desvioa) )
   out_modela       <- summary(model)$coef
   stderrorsa       <- rbind( stderrorsa,out_modela[,2] )

   ResidualStandardError   <- sqrt( sum(model[["residuals"]]**2) / model[["df.residual"]] )
   CoeficienteDeterminacao <- summary(model)$r.squared
   coefia                  <-  cbind( coefia, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) )
   par(new = FALSE)
dev.off()

# Linear Coefficients
png( str_c( homedir,homecoeff, "CoeffCovid19_Log_Linear", l_count, ".png"), width = 500, height = 500 )
   par(new = TRUE)
   mypar()
   par(mar = margin)
   plot( coeff_ll[, 1] / 100, coeff_ll[, 3],
         main     = str_c( "Temporal Characterization of the COVID-19 (",l_count,")" ),
         xlim     = xlimitec,
         ylim     = ylimitec,
         xlab     = str_c(x_label, " since five deaths (x 10²)"),
         ylab     = "Linear Coefficients",
         cex.main = ftsize,
         cex.lab  = ftsizelb,
         cex.axis = ftsizeax )
   grid(lty = 3, lwd = 2)

   model               <- lm( coeff_ll[1:nx, 3] ~
                              x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) )
   predicted.intervals <- predict( model, data.frame(x = q), interval='confidence', level = 0.95 )

   par(new = TRUE)
   plot( q, predicted.intervals[,1],
         ylab     = "",
         xlab     = "",
         xlim     = xlimitec,
         ylim     = ylimitec,
         cex.main = ftsize,
         cex.lab  = ftsizelb,
         cex.axis = ftsizeax,
         col      = color_l[1],
         type     = "l",
         lty      = 1,
         lwd      = 2 )

    maxresiduals_lin <- rbind( maxresiduals_lin, max(model[["residuals"]]) )
    residuoslin      <- model[[ "residuals" ]]
    medialin         <- sum( abs(residuoslin)) / nx
    desviolin        <- sd( abs(residuoslin))

    stats_residualslin <- rbind( stats_residualslin, c(medialin,desviolin) )
    out_modellin       <- summary( model )$coef
    stderrorslin       <- rbind( stderrorslin,out_modellin[,2] )

    ResidualStandardError   <- sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]] )
    CoeficienteDeterminacao <- summary( model )$r.squared
    coefilin                <-  rbind( coefilin, c(model[["coefficients"]], ResidualStandardError, CoeficienteDeterminacao) )
    par(new = FALSE)
dev.off()

# End coeffcovid19