#
# Plots correlations between bloodtypes/populations and deaths
#
ftsizeax <- 1.3
ftsizelb <- 1.5
ftsize   <- 1.5

x_label <- x_label_graph_f(interval_days)

legendposition <- define_position_f (tdpm,l_count)

ylimit   <- limit_f(tdpm,l_count)
color_l  <- c( "red", "blue", "black", "cyan2", "black" ) # A, O, B, AB, Pop
type_l   <- c( 1, 2, 3, 6, 1 )
color_l2 <- c( "brown", "green4", "black" )  # Rh+, Rh-, Pop
xp       <- c(1:N_i)/100

 dir_corr = ""
 ext_png  = ""

plot_title   <- str_c( "CorrelationsPopulations" )
ext_png      <- str_c( type_stat, l_count, "_", i_days, ".png" )
dir_corr     <- str_c( homedir, homecorr )
dir_corr_png <- str_c(dir_corr, plot_title, ext_png)
dir_corr_png %>% png(width = 500, height = 500)
   mypar()
   par( mar = c(4, 4, 4, 4) )
   plot( correlacoes[, 1], main = "Population analysis", ylab = "Correlations", xlab = x_label,
         ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb, cex.axis =  ftsizeax, type = "l", lty = 1 )  # Population
   grid(lty = 3, lwd = 1)
dev.off()

plot_title   <- "CorrelationsCovidABO"
dir_corr_png <- str_c(dir_corr, plot_title, ext_png)
dir_corr_png %>% png(width = 700, height = 700)
#png( str_c( homedir, homecorr, "CorrelationsCovidABO", type_stat, l_count, "_", i_days, ".png"), width = 700, height = 700 )
   mypar( 2, 2 )
   par(new = FALSE)
   par( mar = c(4, 4, 4, 4) )

      plot( correlacoes[, "O+"], main = "ABO System and Rh+ (a)", ylab = "Correlations", xlab = x_label,
            ylim = ylimit, cex.main = ftsize, cex.lab = ftsizelb, cex.axis = ftsizeax, col = color_l[2], type = "l", lty = 2, pch = 10 )  # O+
      legend(legendposition, legend = c("A +", "O +", "B +", "AB +", "Pop"), bty = "n",  col = color_l, lty = type_l, cex = 1.0 )
      grid( lty = 3, lwd = 1)

   par(new = TRUE)
      plot( correlacoes[, "A+"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax, col = color_l[1], type = "l", lty = 1, pch = 12 )   # A+
      grid( lty = 3, lwd = 1)

   par(new = TRUE)
      plot( correlacoes[, "B+"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax, col = color_l[3], type = "l", lty = 3, pch = 9 )   # B+

   par(new = TRUE)
      plot( correlacoes[, "AB+"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax,  col = color_l[4], type = "l", lty = 6, pch = 8 )  # AB+

   par(new = TRUE)
      plot( correlacoes[, "Pop."], ylab = "", xlab = "",
            ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb, cex.axis =  ftsizeax, type = "l", lty = 1 )  # Population
      grid( lty = 3, lwd = 1 )


      plot( correlacoes[, "O-"], main = "ABO System and Rh- (b)", ylab = "Correlations", xlab = x_label,
            ylim = ylimit, cex.main = ftsize, cex.lab = ftsizelb,  cex.axis = ftsizeax, col = color_l[2], type = "l", lty = 2,  pch = 1)  # O-
      legend(legendposition, legend = c("A -", "O -", "B -", "AB -", "Pop"), lty = type_l, bty = "n",  col = color_l, cex = 1.0, bg = 'white' )

      grid( lty = 3, lwd = 1 )

   par(new = TRUE)
      plot( correlacoes[, "A-"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax, col = color_l[1], type = "l", lty = 1, pch = 0 )    # A-

   par(new = TRUE)
      plot( correlacoes[, "B-"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax, col = color_l[3], type = "l", lty = 3, pch = 5 )   # B-

   par(new = TRUE)
      plot( correlacoes[, "AB-"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax, col = color_l[4], type = "l", lty = 6, pch = 4 )    # AB-

   par(new = TRUE)
      plot( correlacoes[, "Pop."], ylab = "", xlab = "",
            ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb, cex.axis =  ftsizeax, type = "l", lty = 1 )  # Population


      plot( correlacoes_abo_rh[, "O"],  main = "ABO System (c)", ylab = "Correlations", xlab = x_label,
            ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,  cex.axis =  ftsizeax, col = color_l[2], type = 'l', lty = 2, pch = "o") # O

      legend(legendposition, legend = c("A", "O", "B", "AB", "Pop"), lty = type_l, bty = "n",  col = color_l, cex = 1.0, bg = 'white' )
      grid( lty = 3, lwd = 1 )

   par(new = TRUE)
      plot( correlacoes_abo_rh[, "A"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax, col = color_l[1], type = "l", lty = 1, pch = "+" )     # A

   par(new = TRUE)
      plot( correlacoes_abo_rh[,"B"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
             cex.axis =  ftsizeax, col = color_l[3], type =  "l", lty = 3, pch = "-" )    # B

   par(new = TRUE)
      plot( correlacoes_abo_rh[, "AB"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax, col = color_l[4], type =  "l", lty = 6, pch = "*" )     # AB

   par(new = TRUE)
      plot( correlacoes[, "Pop."], ylab = "", xlab = "",
            ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb, cex.axis =  ftsizeax, type = "l", lty = 1 )  # Population


   plot( correlacoes_abo_rh[, "Rh+"],  main = "Rhesus(Rh) System (d)", ylab = "Correlations", xlab = x_label,
         ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,  cex.axis =  ftsizeax, col = color_l2[1], type = "l", lty = 1, pch = "+" ) # Rh-
   legend( legendposition, legend = c("Rh +", "Rh -", "Pop"), lty = c(1, 2, 1), bty = "n", col = color_l2, cex = 1.0, bg = 'white' )
   grid( lty = 3, lwd = 1 )

   par(new = TRUE)
      plot( correlacoes_abo_rh[, "Rh-"], ylab = "", xlab = "", ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb,
            cex.axis =  ftsizeax, col = color_l2[2], type = "l", lty = 2, pch = "o" ) # Rh-

   par(new = TRUE)
      plot( correlacoes[, 1], ylab = "", xlab = "",
            ylim = ylimit, cex.main = ftsize, cex.lab =  ftsizelb, cex.axis =  ftsizeax, type = "l", lty = 1 )  # Population

par(new = FALSE)
dev.off()
#End plotcorrelations
