#
# Fitting-stats uded by fitting-correlations.R
#
residuos        <- model[[ "residuals" ]]
media           <- sum(abs(residuos))/nx
desvio          <- sd(abs(residuos))
stats_residuals <- rbind( stats_residuals, c(media,desvio) )
maxresiduals    <- rbind( maxresiduals, max(residuos[5:nx]) )
out_model       <- summary(model)$coef
stderrors       <- rbind( stderrors,out_model[,2] )

ResidualStandardError   <- sqrt( sum(model[["residuals"]]**2)/model[["df.residual"]]  )
CoeficienteDeterminacao <- summary(model)$r.squared

coefi                   <- rbind( coefi,
                                  c( model[["coefficients"]],
                                     ResidualStandardError,
                                     CoeficienteDeterminacao) )

#