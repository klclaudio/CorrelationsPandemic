#Atualizado em  23032020 -> population use from owd, limite date 23122020 -> pca analysis 01052021
#Atualizado em  21072021 ->  analysis 500 days and PCA (owd ref. 18072021)
#
#Boood Groups Temporal Analisys and PCA analysis  - Autor: Kleucio Claudio
#
#
library (tidyverse);
library (cluster);
library (factoextra); 
library (gridExtra);
library (dplyr);
library (rafalib);
# corrplot
library (zoo);
library (ggcorrplot);
library (corrplot);
library (GGally);

#difinindo data de análise
data_site <- 0
  datetemp <- Sys.Date() #max date of the analysis;
    date_analysis <- format(datetemp, format="%d%m%Y")
      homeresults <- str_c("Results",date_analysis);
      homedir <- "/home/klclaudio/Documents/Analises2021_10/";
 dir.create( str_c(homedir,"/Results",date_analysis) ) 

#diretórios de trabalho e resultados

  homepca  <-  str_c(homeresults,"/PCA/");
  dir.create( str_c(homedir,homepca) );
    homedaily <- str_c(homeresults,"/Daily/");
    dir.create( str_c(homedir,homedaily) );  
      homecsv <- str_c(homeresults,"/CSV/");
      dir.create( str_c(homedir,homecsv) );
        homefit <- str_c(homeresults,"/Fitting/");
        dir.create( str_c(homedir,homefit) );
          homeCounEvol <- str_c(homeresults,"/CountriesEvolutions/");
          dir.create( str_c(homedir,homeCounEvol) ); 
            homebld <- str_c(homeresults,"/BloodTypes/");
            dir.create( str_c(homedir,homebld) );
              homemov <- str_c(homeresults,"/Movies/");
              dir.create( str_c(homedir,homemov) );
            homecum <-  str_c(homeresults,"/Cumulative/");
            dir.create( str_c(homedir,homecum) );
          homenorm <- str_c(homeresults,"/Normality/");
          dir.create( str_c(homedir,homenorm) );
        homehist <- str_c(homeresults,"/Histograms/");
        dir.create( str_c(homedir,homehist) );
      homecorr <- str_c(homeresults,"/Correlations/");
      dir.create( str_c(homedir,homecorr) );
    homepval <- str_c(homeresults,"/Pvalues/");
    dir.create( str_c(homedir,homepval) );  
  homecoeff <- str_c(homeresults,"/Coefficients/");
  dir.create( str_c(homedir,homecoeff) );
homedata <-"files_aux/";

getOption("warn")
options(warn = -1)

#funções

source( str_c(homedir,"pca_abo_f.R") ); # Principal Component Analysis
  source( str_c(homedir,"descontinuidade_f.R") ); # Simulações de descontinuidades na evolução temporal
    source( str_c(homedir, "dataowd5_site_f.R") ); # data from owd site
      source( str_c(homedir, "maxmin_values_f.R" ) );

#obtenção dos dados


if ( data_site ==1 ) {

    home_site <- "https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv"
    dataowd5_site_f(date_analysis, home_site, homedir, homedata);  # data from owd site
    
}  

   
# Ordenando países com relação aos dias decorridos a partir da 5 ocorrencia.
# source("/home/kleucio/Documents/Analises2020_12/dataowd5.R")

#dados ordenados por países
#dataowd5 <- read.csv("/home/kleucio/Documents/Analises2020_12/dataowd5_23122020.csv");

dataowd5 <- read.csv( str_c(homedir,"dataowd5_",date_analysis,".csv") );

#dados ordenados por número de dias desde a 5 ocorrência 
dataowd5_days <- read.csv( str_c(homedir,"dataowd5_",date_analysis,"_order.csv") );

#cols: 45-Populatioons, 44-stringency_index, 46-Populations_density, 46-cardiovasc_death_rate, 47-diabetes_prevalence, 51-hospital_beds_per_thousand, 52-life_expectancy, 53-human_development_index
#dataowd5_days <- na.omit(dataowd5_days);correlacoes <- c(); # elimina-se muitos países da análise

head (dataowd5[,c(4,8,9)]); 
head (dataowd5_days[,c(4,8,9,45)]); 

pval  <- c();
wstat <- c();

covid_ll = c();
 coeff_a = c();
  coeff_aux = c();
    coeff_lin = c();

errors_a = c(); 
  errors_lin = c();

r2=c();
  mat_coeffs_csv = c();
  
mat_coefilin_csv  = c();
   mat_statslin_csv = c(); 
     mat_stderrlin_csv = c();
      
mat_coefia_csv = c();
   mat_statsa_csv = c();
      mat_stderra_csv = c();


#-----------------------------------------------------------------------------------------------

#Metodo aplicado a correlações
 method_correl = "pearson";
#  method_correl = "spearman";
   errors_correl = FALSE;

# número de dias a serem analisados após a 5a morte - loop i
   N_i = 686; #686 Em 22/01/2022 temos 4 países para análise no grupo 4 (mais afetados) ver aqrquivo dataowd5_23012022_order.csv
# dimensão para métodos dos mínimos quadrados
   nx = 650;

# Dados: 0- dados SEM transformações logarítmicas,  1- dados COM transformações logarítmicas 
   logdata <- 1;
    if (logdata == 0){
       type_stat = "";
    }else{
       type_stat = "Log";#indicação de nomes para arquivos e gráficos
    }

#datascale = 1 # padronização dos dados  não tem efeito com transformação log
   datascale = 0 # 0- sem padronização dos dados,  1- com padronização

# Execuçao da analise de dados atraves de PCA (Principal Component Analysis)
   pca_expanded <- 0# 0- sem nalises pca, 1- com análise pca

# paises a serem excluídos no processo temporal para análise de descontinuidades
   outcountry <- "";

#ndata - no. de conjuntos de países em análises
   ndata = 4;

   
#------------------------------------------------------------------------------------------------

#for ll..análises (ll) ref. aos conjuntos de países analisados   ll: 1 ...4
# ...
#  for i.........números de dias (N_i) desde a quinta morte   i: 1.. N_i
for (ll in c(1:ndata)){
 
    data_c <-c()
      data_c_percents <- c()
   
    #dados de entra e nomes de saídas de arquivos: homedir, logdata, ll 
    source( str_c(homedir,"data_inout.R") );   
   
    colnames(data_c_percents)<- c("X", "Pop", "O+", "A+", "B+", "AB+", "O-", "A-", "B-", "AB-", "Covid");
       names_col_aux2 <- colnames(data_c_percents);

    data_c <- tibble::column_to_rownames(data_c, var = "X");
    data_c_percents <- tibble::column_to_rownames(data_c_percents, var = "X");

#Visualização dos dados
    aux_data_c <- c();
      aux_data_c <- data_c[,1:9];
        data_c <- c();

    if (logdata == 1) {
        data_c <- log (aux_data_c[,1:9]); #Distribuição dos tipos sanguíneos dos países
    }else{
        data_c <- aux_data_c[,1:9];
    }

    dimensao = dim(dataowd5)
    N = dimensao[1]
    i <- 1
    k <- 1
    days <- c()

#vetor crow para países
    while(k <= N-1){
    
      if ( dataowd5[k,4] == dataowd5[k+1,4]) {
          i = i+1
          k = k+1
      }else{
          days <- c(days,i)
          i=1
          k=k+1
      }
    }
    days <- c(days,i)

    i <- 1
    k <- 1
    daysa5 <- c()
    diarydeaths <- c()
    somadeaths_i = 0;

    # total daily deaths from owd, aligned  since 5th death
    while(k <= N-1){

    if (is.na(dataowd5_days[k,10]) == TRUE) {
        dataowd5_days[k,10] <- 0
    }

      if ( dataowd5_days[k,9] == dataowd5_days[k+1,9]  ) {
         if (dataowd5_days[k,3] != "" && is.na(dataowd5_days[k,10]) != TRUE){
             somadeaths_i = somadeaths_i+ dataowd5_days[k,10]
          }
          i = i+1
          # k = k+1
          
      }else{
         daysa5 <- c(daysa5,i)
         if (dataowd5_days[k,3] != "" && is.na(dataowd5_days[k,10]) != TRUE){
             somadeaths_i = somadeaths_i+ dataowd5_days[k,10]
             diarydeaths <- c(diarydeaths,somadeaths_i)
         }

          i=1
         # k=k+1
      }

          k=k+1

    } # fim while k  
    print("Total deaths")
    print(c(ll, somadeaths_i))

    daysa5 <- c(daysa5,i)
    somadeaths_i = somadeaths_i+ dataowd5_days[k,10]
    diarydeaths <- c(diarydeaths,somadeaths_i)

    ftsize = 1.3;
    png( str_c(homedir, homeCounEvol,"Pandemic_18072021",".png"), width = 500, height = 500)
        mypar(); par(new = FALSE);
        par(mar = c(4, 4, 4, 4))
   
        plot (daysa5 ,main = "Covid-19 Pandemic in 2020-08-19",
            xlab = "Number of Countries",
            ylab = "Days since five deaths",
            cex.main = ftsize , cex.lab = ftsize, cex.axis = ftsize,
            type = "l",
            lty = 1); 
        grid(lty = 3, lwd = 1);
    dev.off();


    Nds = max(daysa5)
    Ndays = c(1:Nds);

#montagem da matriz aux com i dias desde a 5a morte
#namesdatac = rownames(data_c);
    namesdatac = rownames(data_c_percents); #using data from owd
    dimnamesdatac = dim(as.matrix(namesdatac));

    aux <- c();
    aux2 <- c();
    vet_ones <- c(1,1,1,1);
    covid <- c();
    cumulative_covid <- c();

    correlacoes <- c();
    correlacoes_rhp <- c();
    correlacoes_rhn <- c();
    correlacoes_O <- c();
    correlacoes_A <- c();
    correlacoes_B <- c();
    correlacoes_AB <- c();

    correlacoest <- c();     correlacoest_CI <- c();
    correlacoest_rhp <- c(); correlacoest_CI_rhp <- c();
    correlacoest_rhn <- c(); correlacoest_CI_rhn <- c();
    correlacoest_O <- c();   correlacoest_CI_O <- c();
    correlacoest_A <- c();   correlacoest_CI_A <- c();
    correlacoest_B <- c();   correlacoest_CI_B <- c();
    correlacoest_AB <- c();  correlacoest_CI_AB <- c();

    p_values_histogram <- c();
    Wstatistic_histogram <- c();

    maxk = 0;
    cores <- c("aquamarine3","firebrick1","darkviolet","darkorange3","gold4",
         "darkblue","green","darkslategray1","deepskyblue","darkseagreen");
         
    covid_i = c();
    coeff_ll = c(); #coeficientes da análise Covid_19 - 

                 
# --------------- Análises para N_i dias desde a 5a. vítima ---------------
#i - números de dias desde a 5a morte, interno ao loop ll das análises 
for (i in c(1:N_i)){
  print("Análise, Days Since 5:")
  print(c(ll,i));  
  
    covid <- c();
      aux   <- c();
        aux2 <- c();
         vars_pca <- c();
           names_aux2 <- c();
             aux2_percents <- c();
    maxk = maxk + daysa5[i];
    #maxk
   
    l=1
    #k- números de países com i dias  desde a 5a morte
    for (k in c(maxk - daysa5[i]+1: daysa5[i])) {
    
        aux <- rbind(aux,dataowd5_days[k,])
        l=l+1
    
    }
    l=l-1;
    j=1;
    somacovid=0;

    #montagem da matriz de paises s e números de covid a partir da lista da owd
    #k- paises a i dias desde a 5a morte
    for (k in c(1:l)) {
       
       m = 1;
       teste <- FALSE
       while (aux[k,4]!=namesdatac[m] && teste==FALSE) {
          m = m+1;
          if (m == dimnamesdatac[1]) {
             teste <- TRUE;
          }
       }
        #print("m");print(m) 
#      if (i<=5) {
      if (aux[k,4] == namesdatac[m] && is.na(aux[k,8])==FALSE){
      # alteração para todos os países: if ( is.na(aux[k,8])==FALSE){

         #aux2[j,] = datac[l,]
          #using population frow rhresus site
            #aux2 <- rbind( aux2,data_c[m,1:9] );
          
          #using population from owd
              #aux2_percents <- rbind( aux2_percents,c(aux[k,28], data_c_percents[m,2:9]*aux[k,28]) ); # position 28 in previous version
              aux2 <- rbind( aux2,c(as.numeric(aux[k,"population"]), as.numeric(data_c_percents[m,2:9])* as.numeric(aux[k,"population"]) )); #population from owd line 46 in original data

              
              #vars_pca <- rbind( vars_pca,c( as.numeric(aux[k,46:47]), as.numeric(aux[k,51]),
              #                   as.numeric(aux[k,52]),as.numeric(aux[k,53]) ) ); #
              
              #vars_pca1 - WITH stringency_index WITH EXTREME POVERTY
              #vars_pca <- rbind( vars_pca,c( as.numeric(aux[k,46]), as.numeric(aux[k,38]), as.numeric(aux[k,40:47]), as.numeric(aux[k,51]),
              #                    as.numeric(aux[k,52]),as.numeric(aux[k,53]) ) );
              
              #vars_pca1 - WITHout stringency_index WITH EXTREME POVERTY
              #vars_pca <- rbind( vars_pca,c( as.numeric(aux[k,45]), as.numeric(aux[k,51:47]), as.numeric(aux[k,51]),
                                  #as.numeric(aux[k,52]),as.numeric(aux[k,53]) ) );
              #vars_pca <- rbind( vars_pca,c( as.numeric(aux[k,39]), as.numeric(aux[k,40:47]), as.numeric(aux[k,51]),
              #                  as.numeric(aux[k,52]),as.numeric(aux[k,53]) ) );
              
#alterado em 15/102021
              varnames <-  c( "population", "population_density", "median_age","aged_65_older", "aged_70_older",
                              "gdp_per_capita", "extreme_poverty", "cardiovasc_death_rate","diabetes_prevalence",
                              "hospital_beds_per_thousand", "life_expectancy", "human_development_index") #,
                           #   "people_fully_vaccinated_per_hundred" );
              vars_pca <- rbind( vars_pca,c( as.numeric(aux[k,varnames]) ) );
              
              #vars_pca <- rbind( vars_pca,c( as.numeric(aux[k,"population"]), as.numeric(aux[k,40:47]), as.numeric(aux[k,51]),
              #                               as.numeric(aux[k,52]),as.numeric(aux[k,53]) ) );
#fimalteração              
              
              #              print(head(vars_pca));
              #vars_pca2 - WITHOUT EXTREME POVERTY, WITH stringency_index
              #vars_pca <- rbind( vars_pca,c( as.numeric(aux[k,46]), as.numeric(aux[k,38]), as.numeric(aux[k,40:44]), as.numeric(aux[k,46:47]), as.numeric(aux[k,51]),
              #                   as.numeric(aux[k,52]), as.numeric(aux[k,53]) ) ); 
                                 
                        
              #cols: 38-stringency_index, 46-Populatioons,40-Populations_density,41- 42-43-444-45-, 46-cardiovasc_death_rate, 47-diabetes_prevalence, 
              #      51-hospital_beds_per_thousand, 52-life_expectancy, 53-human_development_index
              
              names_aux2<- rbind(names_aux2,aux[k,4])
              #aux2 <-  aux2_percents;  
              #print(aux2)
          
          if (logdata == 1){
             covid <- cbind( covid,log(as.matrix(aux[k,8])) );
          }else{
             covid <- cbind( covid,as.matrix(aux[k,8]) );
          }   
             
          #covid <- cbind( covid,as.matrix(aux[k,8]) );
          somacovid <- somacovid + aux[k,10];
          #print(somacovid);
          
          j = j+1;
       } 
      
  
    } #end for k<-1:l

    rownames(aux2) <- names_aux2;
    colnames(aux2) <- names_col_aux2[2:10];
  
    #  vars_pca1 - WITH stringency_index WITH EXTREME POVERTY
    #colnames(vars_pca1) <- c( "population", "stringency_index", "population_density", "median_age","aged_65_older", "aged_70_older","gdp_per_capita",
    #                         "extreme_poverty", "cardiovasc_death_rate","diabetes_prevalence", "hospital_beds_per_thousand", 
    #                         "life_expectancy", "human_development_index" );
    
    #  vars_pca2 - WITHOUT EXTREME POVERTY, WITH stringency_index
    #colnames(vars_pca2) <- c( "population", "stringency_index", "population_density", "median_age","aged_65_older", "aged_70_older","gdp_per_capita",
    #                          "cardiovasc_death_rate","diabetes_prevalence", "hospital_beds_per_thousand", 
    #                          "life_expectancy", "human_development_index" );
    
    #  vars_pca1 - WITHou stringency_index WITH EXTREME POVERTY
    #colnames(vars_pca1) <- c( "population", "population_density", "median_age","aged_65_older", "aged_70_older","gdp_per_capita",
    #                         "extreme_poverty", "cardiovasc_death_rate","diabetes_prevalence", "hospital_beds_per_thousand", 
    #                         "life_expectancy", "human_development_index" );
       
    if (pca_expanded==1){                         
         colnames(vars_pca) <- varnames

         #colnames(vars_pca) <- c( "population","population_density");#,"median_age","aged_65_older","aged_70_older",
         #                         #"gdp_per_capita","extreme_poverty","cardiovasc_death_rate","diabetes_prevalence",
         #                         #"female_smokers","male_smokers","handwashing_facilities","hospital_beds_per_thousand",
         #                         #"life_expectancy","human_development_index","excess_mortality" );

    
         #using population from owd need log aux2
        dim_vars = dim(vars_pca) 
       
         aux2 = cbind(aux2, vars_pca[,2:dim_vars[2]]);
       
         colnames(aux2) <- c(  names_col_aux2[2:10], colnames( vars_pca[,2:dim_vars[2]] )  );
    }

    print("Dimensão aux2")
      print(dim(aux2))
      
    print("Dimensão covid")
      print(dim(covid))
  
     #elimina paises da matriz aux2
 
     if (outcountry != "") {
        if ( i>=120 ){
            dimaux2_antes <- dim(aux2);
            print("Eliminando paises")
            aux2 <- aux_descontinuidade(aux2, covid, outcountry);
            dimaux2_depois <- dim(aux2);
         
            if (dimaux2_antes[2] != dimaux2_depois[2]) {
               dimaux2 <- dim(aux2);
               covid <- aux2[,dimaux2[2]]
               aux2 <- aux2[,1:dimaux2[2]-1]
               covid <- t(covid)
            }
        }
    } #end if outcountry
     
     #dimaux2 <- dim(aux2);
     #covid <- aux2[,dimaux2[2]]
     #aux2 <- aux2[,1:dimaux2[2]-1]
 
     aux2_r <- aux2;  #aux2 sem transformação log, útil para correlações RH e ABO 
        
         #aux2 = log(aux2);
         #transforamação log dos dados
        if (logdata == 1){
              aux2 = log(aux2);
        }
   
       #rownames(aux2) <- names_aux2;
       #colnames(aux2) <- c(names_col_aux2[2:10],colnames(vars_pca[,2:dim_vars[2]]));

    covid_i[i] <- somacovid;
        
    #print(covid_i);
   
    aux3 <- cbind( aux2,t(covid) );
      aux3_r <- cbind( aux2_r,t(covid) );
    
    aux3 <- na.omit(aux3);
      aux3_r <- na.omit(aux3_r);
       
    aux_covid <- covid;
    
    aux3  <- aux3[!is.infinite( rowSums(aux3) ),];
      aux3_r  <- aux3_r[!is.infinite( rowSums(aux3_r) ),];
      
    ##dimensão de aux2 
    dim_aux2 <- dim(aux2)

    aux2  <- aux3[,1:(dim_aux2[2])];   #aux3 =[ Pop, ABO+, ABO-, 6VARS , covid] 
      aux2_r <- aux3_r[,1:(dim_aux2[2])];
    covid <- aux3[,dim_aux2[2]+1];
    print(head(covid))

    if (datascale == 0) {
       covid <-  as.numeric(covid) ;
       aux2  <-  as.matrix(aux2) ;
    }else{
       covid   <- scale( as.numeric(covid) );
       aux2    <- scale( as.matrix(aux2) );
       aux2_r  <- scale( as.matrix(aux2_r) ); 
    }
    
    # All blood types  analysis: { O+ ,A+ ,B+ , AB+,   O-, A-, B-, AB- } 
    #Calculo da correlação para i dias desde 5 caso de covid - todos os tipos
    
    aux_covid <- c()
    crow_c <- order(covid)
    aux_covid <- covid
    
    covid <- covid[crow_c];
    cumulative_covid <- rbind( cumulative_covid, sum(covid) );
    cumulative_expcovid <- rbind( cumulative_covid, sum(exp(covid)) );
    
    aux2 <- aux2[crow_c,];
          aux2_r <- aux2_r[crow_c,];
     rownames(aux2)
    correl_1 <- cor( aux2, covid );
    
    correltest_1<-c();
     correltest_CI_1<-c();
     
     
    for (kk in c(1:9)){
       auxcorrel_1 <- cor.test( aux2[,kk], covid, method =  method_correl, exact=errors_correl, use = "complete_obs" );
       #testp <- cor.test(scale( aux2[,kk]), covid, method =  method_correl, exact=errors_correl, use = "complete_obs")
       #correltest_1[kk] <- testp$p.value;
       correl_1 [kk] <- auxcorrel_1$estimate;
       correltest_1[kk] <- auxcorrel_1$p.value;
       if (i > 290 & ll> 2){  
         correltest_CI_1 <- rbind( correltest_CI_1, c(0.0, 0.0)); # Intervalo de confiança
       }else{
            correltest_CI_1 <- rbind( correltest_CI_1, t(auxcorrel_1$conf.int[1:2]) ); # Intervalo de confiança
       }
    }
    print(auxcorrel_1)
    #correltest_1 <- cor.test(scale( aux2), covid, method =  method_correl, exact=errors_correl, use = "complete_obs");

      correlacoes <- rbind ( correlacoes, t(correl_1) );
      correlacoest <- rbind ( correlacoest, t(correltest_1) );
      correlacoest_CI <- rbind ( correlacoest_CI, correltest_CI_1 ); # Intervalo de confiança
       
    # ABO analysis
    
    if (logdata == 1){  #dados com transforamações log
    
     auxcor_O <- cor.test( log(aux2_r[,1:9] %*% c(0, 1,0,0,0, 1,0,0,0)) , covid, method =  method_correl, exact=errors_correl, use = "complete_obs" ); # O
        correl_O <- auxcor_O$estimate;
        correltest_O <- auxcor_O$p.value;
  
       if (i > 290 & ll> 2){  
         correltest_CI_O <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_O <- auxcor_O$conf.int[1:2];
       }
       
      correlacoes_O <- rbind( correlacoes_O, t(correl_O) );
      correlacoest_O<-rbind( correlacoest_O, t(correltest_O) );
      correlacoest_CI_O <- rbind ( correlacoest_CI_O, t(correltest_CI_O) )
      
    
     auxcor_A <- cor.test( log(aux2_r[,1:9] %*% c(0, 0,1,0,0, 0,1,0,0)) , covid, method =  method_correl, exact=errors_correl, use = "complete_obs" ); # A
    
        correl_A <- auxcor_A$estimate;
        correltest_A <- auxcor_A$p.value;
        correltest_CI_A <- auxcor_A$conf.int[1:2];
        
       if (i > 290 & ll> 2){  
         correltest_CI_A <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_A <- auxcor_A$conf.int[1:2];
       }
       correlacoes_A <- rbind( correlacoes_A, t(correl_A) );
       correlacoest_A <- rbind( correlacoest_A, t(correltest_A) );
       correlacoest_CI_A <- rbind ( correlacoest_CI_A, t(correltest_CI_A) );
      
     auxcor_B <- cor.test( log(aux2_r[,1:9] %*% c(0, 0,0,1,0, 0,0,1,0)) , covid , method =  method_correl, exact=errors_correl, use = "complete_obs" ); # B
       correl_B <- auxcor_B$estimate;
       correltest_B <- auxcor_B$p.value;
       correltest_CI_B <- auxcor_B$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_B <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_B <- auxcor_B$conf.int[1:2];
       }
       correlacoes_B <- rbind( correlacoes_B, t(correl_B) );
       correlacoest_B <- rbind( correlacoest_B, t(correltest_B) );
       correlacoest_CI_B <- rbind ( correlacoest_CI_B, t(correltest_CI_B) );
      
     auxcor_AB <- cor.test( log(aux2_r[,1:9] %*% c(0, 0,0,0,1, 0,0,0,1)) , covid , method =  method_correl, exact=errors_correl, use = "complete_obs" ); # AB
       correl_AB <- auxcor_AB$estimate;
       correltest_AB <- auxcor_AB$p.value;
       correltest_CI_AB <- auxcor_AB$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_AB <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_AB <- auxcor_AB$conf.int[1:2];
       }
       correlacoes_AB <- rbind( correlacoes_AB, t(correl_AB) );
       correlacoest_AB <- rbind( correlacoest_AB, t(correltest_AB) );
       correlacoest_CI_AB <- rbind ( correlacoest_CI_AB, t(correltest_CI_AB) );
    
     # Rh factor analysis
     auxcor_rhp <- cor.test(log( aux2_r [,2:5]  %*% vet_ones), covid , method =  method_correl, exact=errors_correl, use = "complete_obs" ); # rh+
       correl_rhp <- auxcor_rhp$estimate;
       correltest_rhp <- auxcor_rhp$p.value;
       correltest_CI_rhp <- auxcor_rhp$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_rhp <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_rhp <- auxcor_rhp$conf.int[1:2];
       }
   
       correlacoes_rhp <- rbind( correlacoes_rhp, t(correl_rhp) );
       correlacoest_rhp <- rbind( correlacoest_rhp, t(correltest_rhp) );
       correlacoest_CI_rhp <- rbind ( correlacoest_CI_rhp, t(correltest_CI_rhp) );
      
     auxcor_rhn <- cor.test( log(aux2_r[,6:9]  %*% vet_ones), covid , method =  method_correl, exact=errors_correl, use = "complete_obs" ); # rh-
       correl_rhn <- auxcor_rhn$estimate;
       correltest_rhn <- auxcor_rhn$p.value;
       correltest_CI_rhn <- auxcor_rhn$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_rhn <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_rhn <- auxcor_rhn$conf.int[1:2];
       }
      correlacoes_rhn  <- rbind( correlacoes_rhn, t(correl_rhn) );
      correlacoest_rhn <- rbind( correlacoest_rhn, t(correltest_rhn) );
      correlacoest_CI_rhn <- rbind ( correlacoest_CI_rhn, t(correltest_CI_rhn) );  
      
      
    }else{ #dados originais sem log
  
    auxcor_O <- cor.test( aux2 [,1:9] %*% c(0, 1,0,0,0, 1,0,0,0) , covid, method =  method_correl, exact=errors_correl, use = "complete_obs" ); # O
       correl_O <- auxcor_O$estimate;
       correltest_O <- auxcor_O$p.value;
  
       if (i > 290 & ll> 2){  
         correltest_CI_O <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_O <- auxcor_O$conf.int[1:2];
       }
       
       correlacoes_O <- rbind( correlacoes_O, t(correl_O) );
       correlacoest_O<-rbind( correlacoest_O, t(correltest_O) );
       correlacoest_CI_O <- rbind ( correlacoest_CI_O, t(correltest_CI_O) )
      
    
    auxcor_A <- cor.test( aux2 [,1:9] %*% c(0, 0,1,0,0, 0,1,0,0) , covid, method =  method_correl, exact=errors_correl, use = "complete_obs" ); # A

       correl_A <- auxcor_A$estimate;
       correltest_A <- auxcor_A$p.value;
       correltest_CI_A <- auxcor_A$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_A <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_A <- auxcor_A$conf.int[1:2];
       }
       correlacoes_A <- rbind( correlacoes_A, t(correl_A) );
       correlacoest_A <- rbind( correlacoest_A, t(correltest_A) );
       correlacoest_CI_A <- rbind ( correlacoest_CI_A, t(correltest_CI_A) );
      
    auxcor_B <- cor.test( aux2 [,1:9] %*% c(0, 0,0,1,0, 0,0,1,0) , covid , method =  method_correl, exact=errors_correl, use = "complete_obs" ); # B
       correl_B <- auxcor_B$estimate;
       correltest_B <- auxcor_B$p.value;
       correltest_CI_B <- auxcor_B$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_B <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_B <- auxcor_B$conf.int[1:2];
       }
       correlacoes_B <- rbind( correlacoes_B, t(correl_B) );
       correlacoest_B <- rbind( correlacoest_B, t(correltest_B) );
       correlacoest_CI_B <- rbind ( correlacoest_CI_B, t(correltest_CI_B) );
      
    auxcor_AB <- cor.test( aux2 [,1:9] %*% c(0, 0,0,0,1, 0,0,0,1) , covid , method =  method_correl, exact=errors_correl, use = "complete_obs" ); # AB
       correl_AB <- auxcor_AB$estimate;
       correltest_AB <- auxcor_AB$p.value;
       correltest_CI_AB <- auxcor_AB$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_AB <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_AB <- auxcor_AB$conf.int[1:2];
       }
       correlacoes_AB <- rbind( correlacoes_AB, t(correl_AB) );
       correlacoest_AB <- rbind( correlacoest_AB, t(correltest_AB) );
       correlacoest_CI_AB <- rbind ( correlacoest_CI_AB, t(correltest_CI_AB) );
    
    # Rh factor analysis
    auxcor_rhp <- cor.test( aux2 [,2:5]  %*% vet_ones, covid , method =  method_correl, exact=errors_correl, use = "complete_obs" ); # rh+
       correl_rhp <- auxcor_rhp$estimate;
       correltest_rhp <- auxcor_rhp$p.value;
       correltest_CI_rhp <- auxcor_rhp$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_rhp <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_rhp <- auxcor_rhp$conf.int[1:2];
       }
   
       correlacoes_rhp <- rbind( correlacoes_rhp, t(correl_rhp) );
       correlacoest_rhp <- rbind( correlacoest_rhp, t(correltest_rhp) );
       correlacoest_CI_rhp <- rbind ( correlacoest_CI_rhp, t(correltest_CI_rhp) );
      
    auxcor_rhn <- cor.test( aux2 [,6:9]  %*% vet_ones, covid , method =  method_correl, exact=errors_correl, use = "complete_obs" ); # rh-
       correl_rhn <- auxcor_rhn$estimate;
       correltest_rhn <- auxcor_rhn$p.value;
       correltest_CI_rhn <- auxcor_rhn$conf.int[1:2];
       if (i > 290 & ll> 2){  
         correltest_CI_rhn <- c(0.0, 0.0); # Intervalo de confiança
       }else{
         correltest_CI_rhn <- auxcor_rhn$conf.int[1:2];
       }
       correlacoes_rhn  <- rbind( correlacoes_rhn, t(correl_rhn) );
       correlacoest_rhn <- rbind( correlacoest_rhn, t(correltest_rhn) );
       correlacoest_CI_rhn <- rbind ( correlacoest_CI_rhn, t(correltest_CI_rhn) );  
      
    } #fim if dados originais
    
#          ---------------------- Saídas de dados para cada dia i -------------------
       
#pictures of log(bloodtypes) and covid
if (pca_expanded==1){
    if ( ll==1 | ll== 3 | ll==4){
        print("aux4pca")
        source( str_c(homedir,"aux4pca.R") );
    }
}
    source( str_c(homedir,"coefficients.R") );
    
        if (i==120){
          
            if (logdata==1){
              source( str_c(homedir,"logbloodtypes2.R") );
            }else{
            
            }        
        };
        
#pictures of the pandemic evolution   

    source( str_c(homedir,"movie_covid.R") );  
    
# normality test for each i day

    source( str_c(homedir,"histograms_tests.R") );            

#plot(covid)
    source( str_c(homedir,"CovidEvolution.R") );   
    print(dim(aux2)); 


  } #fim do loop i Ni days since 5a deaths.

    
# -------------------- saidas de dados para cada análise ll -------------------------
#Coeficientes de analises Covid19
    source( str_c(homedir, "coeffCovid19_Log.R") );   
  
# cumulative_covid
    source( str_c(homedir, "plotcumulativecovid.R") );

# Gravando em arquivos cannot open file '/home/klclaudio/Documents/Analises2021_07/bloodcovidOutCH_I22062020.csv': No such file or directory
    print("Gravando arquivos");
    source( str_c(homedir, "saidas_correlacoes.R") );
   
# Normalidade dos dados
    source( str_c(homedir, "evolution_normality.R") );
   
# Gráficos para correlações 
    source( str_c(homedir, "plotcorrelations.R") ); 
  
# Gráficos para p_values 
    source( str_c(homedir, "plotpvalues.R") );  

 # Matriz com número diário de Covid
    covid_ll <- cbind(covid_ll, covid_i);

  
} #fim loop ll (4 conjuntos de dados)
  print("print fim ll");

# Gráfico referente ao número diário de Covid
 if (N_i > 35){
     source( str_c(homedir, "days_covid.R") ); 
 }
 
#   source( str_c(homedir,"plot_normality_all.R");

   # source( str_c(homedir, "maxmin_values.R") ); 
   maxmin_values_f(homedir, homecsv, N_i, nx, type_stat); 
  
   source( str_c(homedir, "pvalues_dif.R") ); 


#Graficos referente às correlaçoes com fit polinomial
   source( str_c(homedir, "fittingLog2.R") );
 
print(" The program fineshed! ")

  




  
  


   

   
