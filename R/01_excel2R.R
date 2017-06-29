#######################
# bartosz: 28 cze 2017
# wczytanie wszystkich zbiorow danych excelowych i zapisanie do postaci waskiej
#######################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ncdf4)
library(lubridate)

# poczdam
poczdam <- read_xlsx("data/xls/Poczdam1893-2016.xlsx", sheet=2)
poczdam <- gather(data = poczdam,key = mm,value = poczdam, 2:13)
poczdam <- data.frame(yy=poczdam$Rok, mm=as.numeric(as.character(poczdam$mm)), poczdam=poczdam$poczdam)
saveRDS(poczdam,file='data/poczdam.rds')

#dodanie kontrolnego plota:
poczdam %>% group_by(yy) %>% summarise(mean(poczdam)) %>% plot(type='l')



# poznan
poznan <- read_xlsx("data/xls/poznan.xlsx", sheet=1)
poznan <- as.data.frame(apply(poznan, 2, as.numeric))
poznan <- round(poznan,2)
colnames(poznan) <- c("yy",1:12,"rok")
poznan <- poznan[,1:13]
poznan <- gather(data = poznan,key = mm,value = poznan, 2:13)
poznan <- data.frame(yy=poznan$yy, mm=as.numeric(as.character(poznan$mm)), poznan=poznan$poznan)
str(poznan)
saveRDS(poczdam,file='data/poznan.rds')
poznan %>% group_by(yy) %>% summarise(mean(poznan)) %>% plot(type='l')

a <- left_join(poznan,poczdam)

# plot kontrolny do szukania bugow:
plot(a$poznan,a$poczdam, asp=1)
abline(a=0,b=1, col='red')
cor(a$poznan,a$poczdam, use='pairwise.complete.obs')
a[which(abs(a$poczdam-a$poznan)>3),]


roczne <- a %>% group_by(yy) %>% summarise(poz=mean(poznan), poczdam=mean(poczdam))
plot(roczne$yy, roczne$poz-roczne$poczdam, type='l', main="tzw. homogeniczna :)")
#lines(roczne$yy, roczne$poczdam, type='l',col='red')


# seria warszawska:
warszawa <- read_xls("data/xls/Warszawa_Obs.xls", sheet=2)
colnames(warszawa) <- c("yy",1:12)
warszawa <- gather(data = warszawa,key = mm,value = warszawa, 2:13)
warszawa <- data.frame(yy=as.integer(warszawa$yy), mm=as.numeric(as.character(warszawa$mm)), warszawa=warszawa$warszawa)
saveRDS(warszawa, file="data/warszawa.rds")

b <- left_join(a, warszawa) # laczenie serii
head(b)

saveRDS(b, file="data/calosc.rds")
#b <- readRDS(file="data/calosc.rds")

## seria z reanaliz NOAA:
library(ncdf4)
nc <- nc_open("~/Pobrane/air.2m.mon.mean.nc")
t2m <- ncvar_get(nc,varid = "air"); lon=ncvar_get(nc,varid = "lon"); lat=ncvar_get(nc,varid = "lat")
czas <- ncvar_get(nc,varid = "time")

t1 <- as.POSIXct(trimws(gsub("[a-z]","",nc$dim$time$units)),"%Y-%m-%e %T")
czas <- t1+60*60*czas

# do koordynat:
# Poczdam <- 52.3906° N, 13.0645° E # lon=14, lat=20
# Poznan <- 52.4064° N, 16.9252° E # lon=16
# Warszawa <- 52.2297° N, 21.0122° E # lon=20

reanaliza <- data.frame(yy=year(czas), mm=month(czas), noaa_poczdam=round(t2m[8,20,]-273.15,2),
                 noaa_poznan=round(t2m[9,20,]-273.15,2),
                 noaa_warszawa=round(t2m[12,20,]-273.15,2)
           )
nc_close(nc)

saveRDS(reanaliza, file="data/reanaliza.rds")


# laczenie wszystkich serii danych:
calosc <- left_join(b, reanaliza)
saveRDS(calosc, file="data/calosc.rds")


## dodanie pragi:
praga <- read_xlsx("data/xls/praga.xlsx", sheet=3)
praga <- praga %>% group_by(yy,mm) %>% summarise(praga=round(mean(tavg, na.rm=T),2))

# laczenie ponowne wszystkich serii danych:
calosc <- left_join(calosc, praga)
saveRDS(calosc, file="data/calosc.rds")


# ploty kontrolne:
roczne <-calosc %>% group_by(yy) %>% summarise(poznan=mean(poznan), poczdam=mean(poczdam), warszawa=mean(warszawa),
                                               noaa_poznan=mean(noaa_poznan), noaa_poczdam=mean(noaa_poczdam), noaa_warszawa=mean(noaa_warszawa),
                                               praga=mean(praga))
 head(roczne)
# plot(roczne$yy, roczne$poz-roczne$warszawa, type='l')
# plot(roczne$yy, roczne$poczdam-roczne$warszawa, type='l')

# srednia ruchoma:
ma <- function(arr, n=15){
  res = arr
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n):i], na.rm=T)
  }
  res
}

# srednia ruchoma 5-letnia
roczne3 <- apply(roczne,2,function(x) ma(x, 5))
srednia_ruchoma <- as.data.frame(roczne3)
srednia_ruchoma <- srednia_ruchoma[,c(1:4,8)]

head(srednia_ruchoma)
srednia_ruchoma <- gather(as.data.frame(srednia_ruchoma), key = "stacja",value = "t2m",poznan:praga)
srednia_ruchoma[which(srednia_ruchoma$stacja=="poznan" & srednia_ruchoma$yy<1920),3] <-  srednia_ruchoma[which(srednia_ruchoma$stacja=="poznan" & srednia_ruchoma$yy<1920),3]-0.8
srednia_ruchoma <- dplyr::filter(srednia_ruchoma, yy>1852)
head(srednia_ruchoma)
ggplot(srednia_ruchoma, aes(yy,t2m, group=stacja, colour=stacja))+geom_line()+ggtitle("Comparison")+
  geom_smooth(method="loess")#+  scale_colour_manual(values=c("pink","skyblue","red"))



calosc <- as.data.frame(calosc)
head(calosc)
calosc2 <- calosc[,c(1:5,9)]
roczne4 <- gather(calosc2, key = "stacja",value = "t2m",poznan:praga)
ggplot(roczne4, aes(yy,t2m, group=stacja, colour=stacja))+#geom_line()+
  geom_smooth(method="loess")#+  scale_colour_manual(values=c("pink","skyblue","red"))


## szukanie bugów:
calosc[which(abs(calosc$poznan-calosc$poczdam)>2.5),]
calosc[which(abs(calosc$warszawa-calosc$poznan)>2.5),]

calosc %>% group_by(mm) %>% summarise( mean(poznan, na.rm=T)-mean(warszawa, na.rm=T))
