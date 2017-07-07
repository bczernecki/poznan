## pobranie brakujacych danych dla sniezki, poznania i krakowa

# sniezka do XII 1998
# warszawa by sie przydala od 2005
# z wroclawiem moze byc problem, ale seria brysow do 2008
# krakow i szczecin potrzebne od 2011
library(imgw)
user_pass <- "nwp@amu.edu.pl:363"
data_start <- "2016-12-26" 
data_end <- "2017-02-20" 

#     
stacja <- c("352160330","350150510", "352200375", "351160424","350190566", "353140205")
names(stacja) <- c("poznan","sniezka","warszawa","wroclaw","krakow","szczecin")


for(stacja2 in stacja){
  data_start <- "1996-01-01"
  data_end <- "2017-01-01"
  tmp <- pobierz_lapply(data_start,data_end,user_pass,stacja2,kod, cores=4) # pobieranie danych
  write.table(x = tmp, file=paste0(stacja2, ".csv" ), row.names = F) # zapisywanie danego roku do pliku
} # koniec petli dla stacji


library(dplyr)
library(tidyr)

wyniki <- NULL
plik <- dir(pattern="csv")
for (i in plik ){
  tmp <- read.table(i, header = T, stringsAsFactors = F)
  tmp$data <- as.Date(tmp$data)
  tmp$yy <- as.numeric(as.character(format(tmp$data, "%Y")))
  tmp$mm <- as.numeric(as.character(format(tmp$data, "%m")))
  tmp$wartosc <- as.numeric(as.character(tmp$wartosc))

  a <- tmp %>%  group_by(yy,mm) %>% summarise(t2m=round(mean(wartosc),2), n=n()) %>% filter(n>20)
  head(a)
  colnames(a)[3] <- i
  a <- a %>% dplyr::select(., -n) 
  
  if(i==plik[1]) wyniki <- a
  if(i!=plik[1]) wyniki <- left_join(wyniki, a)
}

#WriteXLS::WriteXLS(wyniki, ExcelFileName  ="data/xls/dane_dociagniete_imgw.xls")
calosc2 <- calosc %>% arrange(yy,mm) 
WriteXLS::WriteXLS(calosc2, ExcelFileName  ="data/xls/calosc_bez_imgw.xls")


calosc2$test <- paste0(calosc2$yy,calosc2$mm)
duplicated(calosc2$test)
table(duplicated(calosc2$test))
a <- calosc2[duplicated(calosc2$test),]




stacja2 <- "250190390"
kod <- "B100B015CM"
names(stacja2) <- c("krakow-obserw")
library(imgw)
  data_start <- "1970-01-01"
  data_end <- "2017-01-01"
  tmp <- pobierz_lapply(data_start,data_end,user_pass,stacja2,kod, cores=4) # pobieranie danych
  write.table(x = tmp, file=paste0(stacja2, ".csv" ), row.names = F) # zapisywanie danego roku do pliku


  
  
  
  
  
  
  
  
  
  
  
czesiek <-   read.table("~/Pulpit/krakow_obserwatorium.txt", stringsAsFactors = F, dec=".",sep=";")
head(czesiek)  
hist(czesiek$V2)

czesiek$V1 <- as.Date(as.POSIXct(czesiek$V1)  )
head(czesiek)  
str(czesiek)  
czesiek$yy <- as.numeric(format(czesiek$V1, "%Y"))
czesiek$mm <- as.numeric(format(czesiek$V1, "%m"))
krak <- czesiek %>% group_by(yy,mm) %>% summarise(krak_obserw=round(mean(V2),2))

dane2 <- left_join(dane,krak)
head(dane2)
  