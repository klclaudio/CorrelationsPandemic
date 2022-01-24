
dataowd5_site_f <- function (date_analysis, home_site, homedir, homedata) {
# Cria o arquivo dataowd5_DATE a partir do owid-covid-data_less523122020, organiza por total_deaths e elimina todas  menores que 4 deaths
# Antes de rodar esse arquivo
# 1 - owid-covid-data_less523122020 organizado por total_deaths 
# 2 - elimina-se todas  menores que 4 deaths
# 3 - reorganiza por países
# 4 - executa-se dataowd5.R
# 5 - reorganiza por DaysSince5 e renomeia para dataowd5_data_order
#
# dataowd5 <- read.csv("/home/kleucio/Documents/Analises2020/owid-covid-dataless5_2.csv");
#dataowd5 <- read.csv("/home/kleucio/Documents/Analises2020_12/owid-covid-data_less5_23122020.csv");
#library (stringr);


#preparando arqquivo de dados
  
library ("curl") 

auxdata <-c();
#auxdata <- read.table(str_c(homedir, "owid-covid-data09102021.csv"),h=T);
curl_download(home_site,str_c(homedir,'owid-covid-data',date_analysis,'.csv') );
#auxdata <- read.csv(str_c(homedir, "owid-covid-data09102021.csv"));
auxdata <- read.csv(str_c(homedir, 'owid-covid-data',date_analysis,'.csv') );

#reading real file from site owd 
#auxdata <- read.csv(home_site);


# ## using  curl to downloading
# con <- curl(home_site);
# open(con);
# out <- readLines(con);
# close(con); 




dimauxdata <- dim(auxdata);
auxrow <- dimauxdata[1];
auxcol <- dimauxdata[2];

auxdatam <- c();
auxdatam <- as.matrix(auxdata);


dataowd5_aux <- cbind(auxdata[,1:6], auxdata[,8],c(1:auxrow),auxdata[,9:auxcol]);
#head(dataowd5_aux);
#ok

auxnames <- colnames(dataowd5_aux);
    auxnames[8] <- "DaysSince5";
        auxnames[7] <- "total_deaths";
    colnames(dataowd5_aux) <- auxnames;
datowd5_aux <- dataowd5_aux[ order(dataowd5_aux$total_deaths), ];

#head(dataowd5_aux);
#Ok

dataowd5 <- c();
    dataowd5 <- subset( dataowd5_aux,dataowd5_aux[,7] > 4 );
head(dataowd5)
    
datowd5 <- dataowd5[ order(dataowd5$location), ];
#
#dataowd5 <- read.csv(str_c(homedir, "owid-covid-data_less5_09102021.csv"));
#write.csv(dataowd5,str_c (homedir, "dataowd5_Since5_09102021.csv"), row.names = TRUE)
        

#has_rownames(datac);


dimensao <- dim(dataowd5);
N <- dimensao [1];
M <- dimensao [2];

dataowd5[2,8] <- 1
i<-2
k <- 2
dataowd5[1,8] = 1
#plot (daysa5)

while ( i <= N) {
      if ( dataowd5[i,3]==dataowd5[i-1,3]){
           dataowd5[i,8] = k;
           
      }
      else{
           k=1
           dataowd5[i,8] = k;
      }
      i=i+1;
      k = k+1;
       #print(i)
}

#datowd5 <- dataowd5[ order( dataowd5$DaysSince5), ];
write.csv(dataowd5,str_c ( homedir, "dataowd5_", date_analysis, ".csv"), row.names = TRUE)


dataowd5 <- dataowd5[ order(dataowd5$DaysSince5), ];
write.csv(dataowd5,str_c ( homedir, "dataowd5_", date_analysis,"_order.csv"), row.names = TRUE)
teste<-1
return(teste)  
}
