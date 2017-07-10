library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(climatol)
dane <- read_excel("data/xls/calosc_z_imgw.xls")
head(dane)
dane <- dane %>% dplyr::select(-matches("noaa"))




roczne <-dane %>% group_by(yy) %>% summarise(poznan=mean(poznan), poczdam=mean(poczdam), warszawa=mean(warszawa), praga=mean(praga), wroclaw=mean(wroclaw),
                                             krakow=mean(krakow), szczecin=mean(szczecin),sniezka=mean(sniezka))
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
head(srednia_ruchoma)

srednia_ruchoma <- gather(as.data.frame(srednia_ruchoma), key = "stacja",value = "t2m",poznan:sniezka)
# tylko na cele korekty jesli chcemy cos dodac/odjac:
# srednia_ruchoma[which(srednia_ruchoma$stacja=="poznan" & srednia_ruchoma$yy<1920),3] <-  srednia_ruchoma[which(srednia_ruchoma$stacja=="poznan" & srednia_ruchoma$yy<1920),3]

#podciagamy sniezke, zeby nie liczyc na razie anomalii dla kazdej stacji:
srednia_ruchoma[which(srednia_ruchoma$stacja=="sniezka"),3] <-  srednia_ruchoma[which(srednia_ruchoma$stacja=="sniezka"),3]+7.5
#

srednia_ruchoma <- dplyr::filter(srednia_ruchoma, yy>1852, yy<2017)
head(srednia_ruchoma)
srednia_ruchoma <- filter(srednia_ruchoma, stacja!="szczecin", stacja!="poznan", stacja!="krakow")
ggplot(srednia_ruchoma, aes(yy,t2m, group=stacja, colour=stacja))+geom_line()+ggtitle("Comparison")+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = T)
#+  scale_colour_manual(values=c("pink","skyblue","red"))

library(climatol)
# przeksztalcenie analogiczne jak dla dema:
#Now with daily mean temperatures. Preparation of the input files:
# odfiltrowac krakow od 1960:
dane[which(dane$yy>1960),"krakow"] <- NA

dat2 <- as.matrix(dane[,-1:-2]) # nasze
write(dat2,'Ttest2_1848-2016.dat') # nasze

# kolejnosc: poznan poczdam warszawa praga wroclaw krakow szczecin sniezka
est.c2 <- data.frame(X=c(13.06,16.92,21.01,14.42,17.03,19.94,14.55,15.74),
           Y=c(52.406, 52.39, 52.23, 50.09, 51.11, 50.064, 53.42, 50.736),
           Z=c(86, 81, 106, 191, 120, 237, 1, 1602) ,
           Code=as.factor(c("poznan","poczdam", "warszawa", "praga", "wroclaw", "krakow", "szczecin", "sniezka")),
           Names=c("poznan","poczdam", "warszawa", "praga", "wroclaw", "krakow", "szczecin", "sniezka"))   
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



write.table(est.c2,'Ttest2_1848-2016.est',row.names=FALSE,col.names=FALSE)
#Homogenization of the monthly averages:
homogen('Ttest2',1848,2016, hires=F, wd=c(0,100,500), dz.max=6, snht1=30)
## End(Not run)
load("Ttest2_1848-2016.rda")

# poznan zostal poszatkowany na 4 serie, zatem:
a1 <- dah[,,1]
a2 <- dah[,,11]
a3 <- dah[,,14]
a4 <- dah[,,19]

b1 <- NULL
b2 <- NULL
b3 <- NULL
b4 <- NULL

for( i in 1:ncol(a1)) {
  b1 <- c(b1, a1[,i])
  b2 <- c(b2, a2[,i])
  b3 <- c(b3, a3[,i])
  b4 <- c(b4, a4[,i])
}
bb <- cbind.data.frame(b1,b2,b3,b4, dane %>% dplyr::select(yy,mm,poznan) %>% dplyr::select(poznan) %>% pull()) 
bb$yy <- dane$yy
bb$mm <- dane$mm

## rysowanki anomalii:
colnames(bb) <- c("v1","v2","v3","v4","oryg","yy","mm")
head(bb)
bb2 <- bb %>% group_by(yy) %>% summarise_all(mean)
# wychodzi, ze V2 to nasza seria najbardziej optymalna
 
plot(bb2$yy, bb2$v2-colMeans(bb2,na.rm=T)[6], type='l', lwd=2, xlim=c(1850,1950))
lines(bb2$yy, bb2$v1-colMeans(bb2,na.rm=T)[6], type='l', col="red", lty=2)
lines(bb2$yy, bb2$v3-colMeans(bb2,na.rm=T)[6], type='l', col="green", lty=2)
lines(bb2$yy, bb2$v4-colMeans(bb2,na.rm=T)[6], type='l', col="purple", lty=2)
lines(bb2$yy, bb2$oryg-colMeans(bb2,na.rm=T)[6], type='l', col="blue", lty=1)




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
