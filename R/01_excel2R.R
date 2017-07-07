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
#saveRDS(calosc, file="data/calosc.rds")


## dodanie pragi:
praga <- read_xlsx("data/xls/praga.xlsx", sheet=3)
praga <- praga %>% group_by(yy,mm) %>% summarise(praga=round(mean(tavg, na.rm=T),2))

# laczenie ponowne wszystkich serii danych:
calosc <- left_join(calosc, praga)
#saveRDS(calosc, file="data/calosc.rds")
#calosc <- readRDS(file="data/calosc.rds")


## dodanie wroclawia brysiów:
wroclaw <- read.table("data/xls/wroclaw_brysiowie.txt", header=T, sep="\t")
wroclaw <- wroclaw[,1:13]
wroclaw <- gather(wroclaw, key = "mm", value = "wroclaw", Jan:Dec) 
wroclaw$mm <- rep(1:12, each=length(1791:2007))
head(wroclaw)
colnames(wroclaw)[1] <- c("yy")
# ponowne laczenie (tym razem wroclawia) z baza:
calosc <- left_join(calosc, wroclaw)
calosc <- select(calosc, yy:warszawa,praga,wroclaw,noaa_poczdam:noaa_warszawa)
#saveRDS(calosc, "data/calosc.rds")
#calosc <- readRDS("data/calosc.rds")


## dodanie serii z crutempa:
## krakow = dawna stolica polakow
krakow <- read.table("data/xls/krakow.txt", header=F, na.strings = -999)
colnames(krakow) <- c("yy", 1:12)
krakow[,-1] <- krakow[,-1]/10
krakow <- gather(krakow, key="mm", value="krakow", 2:13)
krakow$mm <- as.numeric(as.character(krakow$mm))
calosc <- left_join(calosc, krakow)
#saveRDS(calosc, "data/calosc.rds")
#calosc <- readRDS("data/calosc.rds")

# dodanie serii ze szczecina:
szczecin <- read.table("data/xls/szczecin.txt", header=F, na.strings = -999)
colnames(szczecin) <- c("yy", 1:12)
szczecin[,-1] <- szczecin[,-1]/10
szczecin <- gather(szczecin, key="mm", value="szczecin", 2:13)
head(szczecin)
szczecin$mm <- as.numeric(as.character(szczecin$mm))
calosc <- left_join(calosc, szczecin)
calosc <- select(calosc, yy:wroclaw,krakow:szczecin,noaa_poczdam:noaa_warszawa)
#saveRDS(calosc, "data/calosc.rds")

# dolaczenie sniezki z : https://crudata.uea.ac.uk/cru/data/temperature/
# sniezka <- read.table("data/xls/sniezka.txt", na.strings="-99.0")
## zmiana: 07/07/2017: sniezka przepisana przez Hanie z artykulu Glowickiego:
sniezka <- readxl::read_excel("data/xls/sniezka_glowicki.xls")
sniezka <- sniezka[,1:13]
head(sniezka)
colnames(sniezka) <- c("yy", 1:12)
sniezka <- gather(sniezka, key="mm", value="sniezka", 2:13)
sniezka$mm <- as.numeric(as.character(sniezka$mm))

calosc <- left_join(calosc, sniezka)
calosc <- select(calosc, yy:wroclaw,krakow:sniezka,noaa_poczdam:noaa_warszawa)
saveRDS(calosc, "data/calosc.rds")

calosc <- readRDS("data/calosc.rds")
########################
########################
### ploty kontrolne: ###
########################
########################

roczne <-calosc %>% group_by(yy) %>% summarise(poznan=mean(poznan), poczdam=mean(poczdam), warszawa=mean(warszawa), praga=mean(praga), wroclaw=mean(wroclaw),
                                               krakow=mean(krakow), szczecin=mean(szczecin),sniezka=mean(sniezka),
                                               noaa_poznan=mean(noaa_poznan), noaa_poczdam=mean(noaa_poczdam), noaa_warszawa=mean(noaa_warszawa)
                                               )
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
srednia_ruchoma <- srednia_ruchoma[,c(1:9)]

head(srednia_ruchoma)
srednia_ruchoma <- gather(as.data.frame(srednia_ruchoma), key = "stacja",value = "t2m",poznan:sniezka)
# tylko na cele korekty jesli chcemy cos dodac/odjac:
# srednia_ruchoma[which(srednia_ruchoma$stacja=="poznan" & srednia_ruchoma$yy<1920),3] <-  srednia_ruchoma[which(srednia_ruchoma$stacja=="poznan" & srednia_ruchoma$yy<1920),3]

#podciagamy sniezke, zeby nie liczyc na razie anomalii dla kazdej stacji:
srednia_ruchoma[which(srednia_ruchoma$stacja=="sniezka"),3] <-  srednia_ruchoma[which(srednia_ruchoma$stacja=="sniezka"),3]+7.5
#

srednia_ruchoma <- dplyr::filter(srednia_ruchoma, yy>1852, yy<2000)
head(srednia_ruchoma)
srednia_ruchoma <- filter(srednia_ruchoma, stacja!="szczecin")
ggplot(srednia_ruchoma, aes(yy,t2m, group=stacja, colour=stacja))+geom_line()+ggtitle("Comparison")+
  geom_smooth(method="loess")#+  scale_colour_manual(values=c("pink","skyblue","red"))



calosc <- as.data.frame(calosc)
head(calosc)
calosc2 <- calosc[,c(1:6)]
roczne4 <- gather(calosc2, key = "stacja",value = "t2m",poznan:praga)
ggplot(roczne4, aes(yy,t2m, group=stacja, colour=stacja))+#geom_line()+
  geom_smooth(method="loess")#+  scale_colour_manual(values=c("pink","skyblue","red"))


## szukanie bugów:
calosc[which(abs(calosc$poznan-calosc$poczdam)>2.5),]
calosc[which(abs(calosc$warszawa-calosc$poznan)>2.5),]

calosc %>% group_by(mm) %>% summarise( mean(poznan, na.rm=T)-mean(warszawa, na.rm=T))




# plot kontrolny do szukania bugow w krakowie:
plot(roczne$yy, roczne$praga-roczne$krakow, type='l', main="tzw. homogeniczna :)", xlim=c(1950,2010))
cbind(roczne$yy,roczne$praga-roczne$krakow)
#lines(roczne$yy, roczne$poczdam, type='l',col='red')