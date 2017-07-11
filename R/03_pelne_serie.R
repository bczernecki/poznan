library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(climatol)
dane <- read_excel("data/xls/calosc_z_imgw.xls")
head(dane)
dane <- dane %>% dplyr::select(-matches("noaa"))




roczne <-dane %>% group_by(yy) %>% summarise(poznan=mean(poznan), poczdam=mean(poczdam), warszawa=mean(warszawa), praga=mean(praga), wroclaw=mean(wroclaw),
                                             krakow=mean(krakow), szczecin=mean(szczecin),gdansk=mean(gdansk),sniezka=mean(sniezka))
head(roczne)
# plot(roczne$yy, roczne$poz-roczne$warszawa, type='l')
# plot(roczne$yy, roczne$poczdam-roczne$warszawa, type='l')

# srednia ruchoma:
ma <- function(arr, n=15){
  res = arr
  for(i in n:length(arr)){
    #res[i] = mean(arr[(i-n):i], na.rm=T)
    res[i] = mean(arr[(i-n):i], na.rm=T)
  }
  res
}

# srednia ruchoma 7-letnia
roczne3 <- apply(roczne,2,function(x) ma(x, 7))
srednia_ruchoma <- as.data.frame(roczne3)
head(srednia_ruchoma)

srednia_ruchoma <- gather(as.data.frame(srednia_ruchoma), key = "stacja",value = "t2m",poznan:sniezka)
# tylko na cele korekty jesli chcemy cos dodac/odjac:
# srednia_ruchoma[which(srednia_ruchoma$stacja=="poznan" & srednia_ruchoma$yy<1920),3] <-  srednia_ruchoma[which(srednia_ruchoma$stacja=="poznan" & srednia_ruchoma$yy<1920),3]

#podciagamy sniezke, zeby nie liczyc na razie anomalii dla kazdej stacji:
srednia_ruchoma[which(srednia_ruchoma$stacja=="sniezka"),3] <-  srednia_ruchoma[which(srednia_ruchoma$stacja=="sniezka"),3]+6.5
srednia_ruchoma <- dplyr::filter(srednia_ruchoma, yy>1852, yy<2017)
head(srednia_ruchoma)
srednia_ruchoma <- filter(srednia_ruchoma, stacja!="szczecin", stacja!="krakow", stacja!="wroclaw")
ggplot(srednia_ruchoma, aes(yy,t2m, group=stacja, colour=stacja))+geom_line()+ggtitle("Comparison")+
  scale_x_continuous(breaks = seq(1850, 2015, by = 10)) 
#  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = T)
#+  scale_colour_manual(values=c("pink","skyblue","red"))


# pierwsze puszczenie climatola tylko dla wiarygodnych serii -> bez krakowa, szczecina i wroclawia
library(climatol)
dane <- read_excel("data/xls/calosc_z_imgw.xls")
dane2 <- dane %>% dplyr::select(yy:poznan,poczdam,warszawa,praga,sniezka,gdansk)
head(dane2)
# przeksztalcenie analogiczne jak dla dema:
dat2 <- as.matrix(dane2[,-1:-2]) # nasze
write(dat2,'Ttest2_1848-2016.dat') # nasze

# kolejnosc:          poznan poczdam warszawa praga sniezka gdansk
est.c2 <- data.frame(X=c(16.92,13.06,21.01,14.42, 15.74, 18.61),
           Y=c(52.406, 52.39, 52.23, 50.09, 50.736, 54.375),
           Z=c(86, 81, 106, 237, 1602, 10) ,
           Code=as.factor(c("poznan","poczdam", "warszawa", "praga", "sniezka", "gdansk")),
           Names=c("poznan","poczdam", "warszawa", "praga", "sniezka", "gdansk"))   
#est.c <- data.frame(X=c())
#poznan poczdam warszawa praga wroclaw krakow szczecin
# do koordynat:
# Poznan <- 52.4064° N, 16.9252° E # lon=16
# Poczdam <- 52.3906° N, 13.0645° E # lon=14, lat=20
# Warszawa <- 52.2297° N, 21.0122° E # lon=20
# Praga <- 50.0866° N, 14.4160° E
# Wroclaw <- 51.1079° N, 17.0385° E
# Krakow <- 50.0647° N, 19.9450° E
# Szczecin <- 53.4285° N, 14.5528° E
# Sniezka <- 50.7360° N, 15.7399° E
# Gdansk-Wrzeszcz <- 54.3754° N, 18.6091° E

write.table(est.c2,'Ttest2_1848-2016.est',row.names=FALSE,col.names=FALSE)
#Homogenization of the monthly averages:
library(mapdata)
library(maptools)
homogen('Ttest2',1848,2016, hires=F, wd=c(0,100,500), dz.max=6, snht1=30)
## End(Not run)
load("Ttest2_1848-2016.rda")

# poznan zostal poszatkowany na 3 serie, zatem:
a1 <- dah[,,1]
a2 <- dah[,,7]
a3 <- dah[,,10]

b1 <- NULL
b2 <- NULL
b3 <- NULL

for( i in 1:ncol(a1)) {
  b1 <- c(b1, a1[,i])
  b2 <- c(b2, a2[,i])
  b3 <- c(b3, a3[,i])
}
bb <- cbind.data.frame(b1,b2,b3, dane %>% dplyr::select(yy,mm,poznan) %>% dplyr::select(poznan) %>% pull()) 
bb$yy <- dane$yy
bb$mm <- dane$mm

## rysowanki anomalii:
colnames(bb) <- c("v1","v2","v3","oryg","yy","mm")
head(bb)
bb2 <- bb %>% group_by(yy) %>% summarise_all(mean)
# wychodzi, ze V2 to nasza seria najbardziej optymalna
head(bb2)

roczne3 <- apply(bb2,2,function(x) ma(x, 7))
bb2 <- as.data.frame(roczne3)
head(srednia_ruchoma)

 
plot(bb2$yy, bb2$v2-colMeans(bb2,na.rm=T)[6], type='l', lwd=2, ylim=c(-1,3), xlim=c(1850,2015), xaxs='i')
lines(bb2$yy, bb2$v1-colMeans(bb2,na.rm=T)[6], type='l', col="red", lty=2)
lines(bb2$yy, bb2$v3-colMeans(bb2,na.rm=T)[6], type='l', col="green", lty=2)
lines(bb2$yy, bb2$oryg-colMeans(bb2,na.rm=T)[6], type='l', col="blue", lty=1)

# wnioski: do roku 1867 mozna przyjac wersje v2, tj. skorygowana o skok z 1920,
# dla wczesniejszego okresu konieczne bedzie wyprowadzenie kolejnej poprawki bazujac na innych
# seriach z tego okresu, ktore rowniez posiadaja dane (warszawa, praga, gdansk)

##################################
## TODO: 11/07/2017
## UWAGA UWAGA!! LEPIEJ BEDZIE WYKORZYSTAC DO OSTATNICH ZROWNAN ORYGINALNA SERIE!!!
## A NIE REKONSTRUOWANA!!!!

# zalozmy, ze seria 1867-1892 jest homogeniczna (ul. zielona 1 i zielona 2)
# sprobujmy wykorzystac metode stalosci roznic dla tych 3 stacji:
roczne$recon <- bb2$v2
roczne %>% filter(yy>=1867 & yy<=1892) %>% select(yy:poznan,warszawa,praga,gdansk,recon) %>% colMeans() %>% round(.,1)
# yy        poznan warszawa    praga   gdansk    recon 
# 1879.5      8.1      7.4      9.0      7.3      7.7 

# przed przeniesieniem stacji z grobli na zielona 1 roznice te wygladaly tak:
roczne %>% filter(yy>=1862 & yy<=1867) %>% select(yy:poznan,warszawa,praga,gdansk,recon) %>% colMeans() %>% round(.,1)
#     yy   poznan warszawa    praga   gdansk    recon 
# 1864.5      8.1      7.3      9.3      7.3      8.7 

# a jeszcze przed przeniesiem stacji z pocztowej na groble tak:
roczne %>% filter(yy<=1861) %>% select(yy:poznan,warszawa,praga,gdansk,recon) %>% colMeans(na.rm = T) %>% round(.,1)
#     yy   poznan warszawa    praga   gdansk    recon 
# 1855.0      7.7      7.3      8.7      7.5      8.5 

recon <- select(bb,yy,mm,v2,oryg)
recon$data <- as.Date(paste(recon$yy,recon$mm,"01",sep="-"))

# przyjmujac metode stalosci roznic:
# dla serii rekonstruowanej trzeba wprowadzic dodatkowa poprawke 1862-1867 rzedu 1,1*C wzgledem wawy, 1,0* wzgledem gdanska i 0,7 wzgledem pragi
# dla serii rekonstruowanej dodatkowa poprawka <1862 rzedu 0,9*C wzgledem wawy, 0,6*C wzgledem gdanska i 1,1 wzgledem pragi
# interpolujac na oko:

#1862-67 -> odejmujemy 1,0*C
recon[which(recon$data >=as.Date("1862-04-01") & recon$data<=as.Date("1867-09-01") ),"v2"] <- recon[which(recon$data >=as.Date("1862-04-01") & recon$data<=as.Date("1867-09-01") ),"v2"]-1
#1848-62 -> odejmujemy 0,9*C
recon[which(recon$data<=as.Date("1862-03-01") ),"v2"] <- recon[which(recon$data<=as.Date("1862-03-01")) ,"v2"]-0.9

### w tym momencie rekonstrukcja powinna byc gites!!!
recon <- select(recon, data,yy,mm,v2,oryg) 
colnames(recon)[4] <- "rekonstrukcja"

head(recon)
WriteXLS::WriteXLS(recon, ExcelFileName = "data/xls/poznan_zrekonstruowany.xls")

roczne <- left_join(dane, recon)
roczne <- roczne %>% select(yy, poznan:praga,gdansk, sniezka, rekonstrukcja) %>% group_by(yy) %>% 
  summarise(poznan=mean(poznan), poczdam=mean(poczdam), warszawa=mean(warszawa), praga=mean(praga), 
            gdansk=mean(gdansk),sniezka=mean(sniezka), rekonstrukcja=mean(rekonstrukcja))

roczne3 <- apply(roczne,2,function(x) ma(x, 7))
srednia_ruchoma <- as.data.frame(roczne3)
srednia_ruchoma <- gather(as.data.frame(srednia_ruchoma), key = "stacja",value = "t2m",poznan:rekonstrukcja)
srednia_ruchoma[which(srednia_ruchoma$stacja=="sniezka"),3] <-  srednia_ruchoma[which(srednia_ruchoma$stacja=="sniezka"),3]+6.5
ggplot(srednia_ruchoma, aes(yy,t2m, group=stacja, colour=stacja))+geom_line()+ggtitle("Comparison")+
  scale_x_continuous(breaks = seq(1850, 2015, by = 10)) 



















plot(bb2$yy, bb2$v2, xlim=c(1848,1875), type='l', ylim=c(5,12))
lines(roczne$yy, roczne$warszawa, xlim=c(1848,1875), lty=2)
lines(roczne$yy, roczne$praga, xlim=c(1848,1875), lty=3)
lines(roczne$yy, roczne$gdansk, lty=3)


abline(v=seq(1850,2050,10), lty=3)
## cos tu jednak mocno nie pasuje dla odbicia ~1885
plot(bb2$yy, bb2$v2, type='l', lwd=2, xlim=c(1885,2015), xaxs='i')
abline(v=seq(1850,2050,10), lty=3)


### moze warto sprobowac uciac dane dla poznania sprzed okresu 'jumpa'
### i reszte zrekonstruowac
dane[which(dane$yy>1960),"krakow"] <- NA
dane[which(dane$yy<1923),"poznan"] <- NA
dat2 <- as.matrix(dane[,-1:-2]) # nasze
write(dat2,'Ttest2_1848-2016.dat') # nasze
# kolejnosc: poznan poczdam warszawa praga wroclaw krakow szczecin sniezka
est.c2 <- data.frame(X=c(13.06,16.92,21.01,14.42,17.03,19.94,14.55,15.74),
                     Y=c(52.406, 52.39, 52.23, 50.09, 51.11, 50.064, 53.42, 50.736),
                     Z=c(86, 81, 106, 191, 120, 237, 1, 1602) ,
                     Code=as.factor(c("poznan","poczdam", "warszawa", "praga", "wroclaw", "krakow", "szczecin", "sniezka")),
                     Names=c("poznan","poczdam", "warszawa", "praga", "wroclaw", "krakow", "szczecin", "sniezka"))   
write.table(est.c2,'Ttest2_1848-2016.est',row.names=FALSE,col.names=FALSE)
homogen('Ttest2',1848,2016, hires=F, wd=c(0,100,500), dz.max=6, snht1=30)
## End(Not run)
load("Ttest2_1848-2016.rda")









## proba tylko z danymi sprzed 1920
dane2 <- dplyr::filter(dane, yy<=1920)
dat2 <- as.matrix(dane2[,-1:-2]) # nasze
write(dat2,'Ttest2_1848-1920.dat') # nasze

# kolejnosc: poznan poczdam warszawa praga wroclaw krakow szczecin sniezka
est.c2 <- data.frame(X=c(13.06,16.92,21.01,14.42,17.03,19.94,14.55,15.74),
                     Y=c(52.406, 52.39, 52.23, 50.09, 51.11, 50.064, 53.42, 50.736),
                     Z=c(86, 81, 106, 191, 120, 237, 1, 1602) ,
                     Code=as.factor(c("poznan","poczdam", "warszawa", "praga", "wroclaw", "krakow", "szczecin", "sniezka")),
                     Names=c("poznan","poczdam", "warszawa", "praga", "wroclaw", "krakow", "szczecin", "sniezka"))   
write.table(est.c2,'Ttest2_1848-1920.est',row.names=FALSE,col.names=FALSE)
#Homogenization of the monthly averages:
homogen('Ttest2',1848,1920, hires=F,  verb=T, dz.max=5)
load("Ttest2_1848-1920.rda")
# poznan zostal poszatkowany na 4 serie, zatem:
a1 <- dah[,,1]
a2 <- dah[,,10]
a3 <- dah[,,15]

b1 <- NULL
b2 <- NULL
b3 <- NULL

for( i in 1:ncol(a1)) {
  b1 <- c(b1, a1[,i])
  b2 <- c(b2, a2[,i])
  b3 <- c(b3, a3[,i])
}
bb <- cbind.data.frame(b1,b2,b3, dane %>%dplyr::filter(yy<=1920) %>%  dplyr::select(yy,mm,poznan) %>%  pull()) 
bb$yy <- dplyr::filter(dane , yy<=1920) %>% dplyr::select(yy) %>% pull()
bb$mm <- dplyr::filter(dane , yy<=1920) %>% dplyr::select(mm) %>% pull()
head(bb)
colnames(bb) <- c("v1","v2","v3","oryg","yy","mm")
head(bb)

bb2 <- bb %>% group_by(yy) %>% summarise_all(mean)
plot(bb2$yy, bb2$v1-8.173105, type='l')
lines(bb2$yy, bb2$v2-8.173105, type='l', col="red")
plot(bb2$yy, bb2$v2-8.173105, type='l', col="green", lwd=2)
lines(bb2$yy, bb2$oryg-8.173105, type='l', col="blue")




### proba dla danych po 1920:
## proba tylko z danymi sprzed 1920
dane2 <- dplyr::filter(dane, yy>=1920)
dat2 <- as.matrix(dane2[,-1:-2]) # nasze
write(dat2,'Ttest2_1920-2016.dat') # nasze

# kolejnosc: poznan poczdam warszawa praga wroclaw krakow szczecin sniezka
est.c2 <- data.frame(X=c(13.06,16.92,21.01,14.42,17.03,19.94,14.55,15.74),
                     Y=c(52.406, 52.39, 52.23, 50.09, 51.11, 50.064, 53.42, 50.736),
                     Z=c(86, 81, 106, 191, 120, 237, 1, 1602) ,
                     Code=as.factor(c("poznan","poczdam", "warszawa", "praga", "wroclaw", "krakow", "szczecin", "sniezka")),
                     Names=c("poznan","poczdam", "warszawa", "praga", "wroclaw", "krakow", "szczecin", "sniezka"))   
write.table(est.c2,'Ttest2_1920-2016.est',row.names=FALSE,col.names=FALSE)
#Homogenization of the monthly averages:
homogen('Ttest2',1920,2016, hires=F,  verb=T, dz.max=5)
load("Ttest2_1848-1920.rda")
# poznan zostal poszatkowany na 4 serie, zatem:
a1 <- dah[,,1]
a2 <- dah[,,10]
a3 <- dah[,,15]

b1 <- NULL
b2 <- NULL
b3 <- NULL

for( i in 1:ncol(a1)) {
  b1 <- c(b1, a1[,i])
  b2 <- c(b2, a2[,i])
  b3 <- c(b3, a3[,i])
}
bb <- cbind.data.frame(b1,b2,b3, dane %>%dplyr::filter(yy<=1920) %>%  dplyr::select(yy,mm,poznan) %>%  pull()) 
bb$yy <- dplyr::filter(dane , yy<=1920) %>% dplyr::select(yy) %>% pull()
bb$mm <- dplyr::filter(dane , yy<=1920) %>% dplyr::select(mm) %>% pull()
head(bb)
colnames(bb) <- c("v1","v2","v3","oryg","yy","mm")
head(bb)

bb2 <- bb %>% group_by(yy) %>% summarise_all(mean)
plot(bb2$yy, bb2$v1-8.173105, type='l')
lines(bb2$yy, bb2$v2-8.173105, type='l', col="red")
plot(bb2$yy, bb2$v2-8.173105, type='l', col="green", lwd=2)
lines(bb2$yy, bb2$oryg-8.173105, type='l', col="blue")
