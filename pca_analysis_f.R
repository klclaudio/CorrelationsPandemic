#
# PCA analysis
#
# pca example from:
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
pca_analysis_f <- function(data_covid, i_days, l_count, dirout) {

   packages_pca <- c( "ggplot2",
                      "factoextra",
                      "FactoMineR",
                      "tidyverse",
                      "cluster",
                      "gridExtra",
                      "dplyr",
                      "rafalib" )
   lapply(packages_pca, library, character.only = TRUE)

   dim                 <- dim(data_covid)
   # frame without covid (death) vector.
   data_covid.active <- data_covid[,1:dim[2]-1]

   rownames(data_covid.active) <- rownames (data_covid)

   # In PCA, variables are often scaled (i_days.e. standardized).
   # This is particularly recommended when variables are measured
   # in different scales (e.g: kilograms, kilometers, centimeters, …)
   # otherwise, the PCA outputs obtained will be severely affected.
   # PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

   res.pca <- PCA(data_covid, quanti.sup = dim[2], graph = FALSE)
   print(res.pca)

   # Visualization and Interpretation from: library("factoextra")
   eig.val <- get_eigenvalue(res.pca)


   # An alternative method to determine the number of PCA is to a Screen Plot
   fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

   # Graph of variables
   var <- get_pca_var(res.pca)

   # The different components can be accessed as follow:
   # Coordinates
   head(var$coord)
   # Cos2: quality on the factore map
   head(var$cos2)
   # Contributions to the principal components
   head(var$contrib)

   # Correlation circle
   head(var$coord, 4)  # show 4 variables

   fviz_pca_var(res.pca, col.var = "black")
   circle.plot = fviz_pca_var( res.pca,
                               col.var       = "cos2",
                               gradient.cols = c("#00AFBB",
                                                 "#E7B800",
                                                 "#FC4E07"),
                               repel         = TRUE )

   png( str_c(dirout, "/pca_plot_", l_count, "_", i_days, ".png"), width = 700, height = 700 )
      print(circle.plot)
   dev.off()

   #T he larger the value of the contribution, the more the variable contributes to the component.
   #head(var$contrib, 4)

   library("corrplot")
   #corrplot(var$contrib, is.corr=FALSE)

   # The function fviz_contrib() [factoextra package] can be used to draw
   # a bar plot of variable contributions.
   # Contributions of variables to PC1
   fviz_contrib( res.pca,
                 choice = "var",
                 axes = 1,
                 top = 10 )

   # Contributions of variables to PC2
   fviz_contrib( res.pca,
                 choice = "var",
                 axes = 2,
                 top = 10 )

   # The total contribution to PC1 and PC2 is obtained with the following R code:
   fviz_contrib( res.pca,
                 choice = "var",
                 axes   = 1:2,
                 top    = 10 )

   bi.plot <- fviz_pca_biplot( res.pca,
                               repel    = TRUE,
                               col.ind  = "#6969DF", # Individuals color
                               col.var  = "cos2",
                               gradient.cols = c( "#00AFBB",
                                                  "#E7B800",
                                                  "#FC4E07") )
                               #label ="var" )

   png( str_c(dirout, "/pca_biplot_", l_count, "_", i_days, ".png"), width = 1000, height = 600 )
      print(bi.plot)
   dev.off()
#
# fviz_pca_ind(res.pca, habillage = 3,
#             addEllipses =TRUE, ellipse.type = "confidence",
#             palette = "jco", repel = TRUE)
#
return()
} #End function pca_analysis
