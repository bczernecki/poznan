library(readxl)
library(dplyr)
library(tidyr)

dane <- read_excel("/home/bartosz/github/poznan/data/xls/calosc_z_imgw.xls")
dane <- select(dane, yy, mm, poznan_rekonstr)
# teraz trzeba poklasyfikowac!!! dopiero potem dzielic!
library(reshape)
library(plyr)
library(tidyverse)
library(RColorBrewer)



anomal <- function(x, rok) {
  ind <- which(rok >= 1961 & rok <= 1990) # deklaracja referencji
  round(x-mean(x[ind], na.rm=T),2)
}

klasa_kwantyl <- function(x, rok){
  ind <- which(rok >= 1961 & rok <= 1990) # deklaracja referencji
  przedzialy <- quantile(x[ind], c(0,5,10,20,30,40,60,70,80,90,95,100)/100)
  przedzialy[c(1,11)] <- c(-99,99)
  cut(x, breaks=przedzialy, labels=1:11)
} 

klasa_lorenc <- function(x, rok){
  ind <- which(rok >= 1961 & rok <= 1990) # deklaracja referencji
  przedzialy <- (sd(x[ind], na.rm=T)*c(-99,-2.5,-2.0,-1.5,-1.0,-0.5, 0.5, 1.0, 1.5, 2.0, 2.5,99))+mean(x[ind], na.rm=T)
  cut(x, breaks = przedzialy, labels = 1:11)
}

macierz <- spread(dane, mm, poznan_rekonstr)
macierz$rok <- rowMeans(macierz[,-1])
colnames(macierz)[14] <- "YEAR"


dane55 <- gather(macierz, miesiac, temp, -yy) 
dane55$miesiac <- factor(dane55$miesiac, levels = c(1:12,"YEAR"))
library(plyr)
wynik <- ddply(dane55, "miesiac", transform, 
               anomalia=anomal(x=temp, rok=yy),  
               lorenc=klasa_lorenc(x=temp, rok=yy),
               kwantyl=klasa_kwantyl(x=temp, rok=yy))






range(dane$yy)
(2016-1848)/4
1848+56




dane1 <- filter(wynik, yy<=1903)
dane2 <- filter(wynik, yy>1903, yy<=1960)
dane3 <- filter(wynik, yy>1960)



kolory <-  colorRampPalette(colors = rev(c("red4","red","orange","white","lightblue", "blue","blueviolet")))(11)
etykiety <- rev(c("extremely hot", "anomaly hot", "very hot", "warm", "slightly warm",
                  "normal", "slightly cold" , "cold", "very cold", "anomaly cold", "extremely cold"))



# marginesy: # zakresy okna dla calej rysowanej figury:
head(dane1)
macierz <- dplyr::select(dane1, yy, miesiac, kwantyl) %>% spread(., miesiac, kwantyl)
head(macierz)
macierz$rok <- rowMeans(macierz[,-1])
macierz <- apply(macierz, 2, as.numeric)

rysuj <- function(macierz, legenda=F, poczatek=3){
      par(mar=c(0.5,3,2,1), fig=c(0.05, 0.70, 0.06, 0.95))
      image(x = 1:13, y=1:nrow(macierz), t(macierz[nrow(macierz):1,-1]), xlab = "", xaxt='n', yaxt='n', col = kolory, ylab='')
      axis(2, labels = macierz[seq(from=poczatek, to=nrow(macierz), by=10),1], at=(nrow(macierz):1)[seq(from=poczatek, to=nrow(macierz), by=10)], 
           cex.axis=1.2, padj = 0.5)
      axis(3, labels = c("J","F","M","A","M","J","J","A","S","O","N","D","Year"), at=1:13, tick = F, padj = 1)
      abline(v=1.5:13, lwd=0.8)
      abline(v=12.5:13.5, lwd=2.5, lty=1, col="black") # oddzielamy serie roczna
      abline(h=1.5:nrow(macierz), lwd=0.5)
      box(lwd=2)

        if(legenda==T){
        par(mar=c(0.5,3,2,1), fig=c(0.65, 0.95, 0.06, 0.45), new=T)
        image(t(matrix(1:11)), col=kolory, xaxt='n', yaxt='n', main="Legend:" , cex.main=1)
        text(x = rep(0,11), y=0:10/10, labels=etykiety, cex=0.8)
        box()
        abline(h=0.5:10/10, lwd=0.5)
        }
} # koniec funkcji

# pierwszy okres
svg("~/dane1.svg",width = 7, height = 9)
rysuj(macierz)
dev.off()

# drugi okres:
macierz <- dplyr::select(dane2, yy, miesiac, kwantyl) %>% spread(., miesiac, kwantyl)
macierz$rok <- rowMeans(macierz[,-1])
macierz <- apply(macierz, 2, as.numeric)
svg("~/dane2.svg",width = 7, height = 9)
rysuj(macierz, legenda = F, poczatek = 7)
dev.off()

# trzeci okres:
macierz <- dplyr::select(dane3, yy, miesiac, kwantyl) %>% spread(., miesiac, kwantyl)
macierz$rok <- rowMeans(macierz[,-1])
macierz <- apply(macierz, 2, as.numeric)
svg("~/dane3.svg",width = 7, height = 9)
rysuj(macierz,  poczatek = 10, legenda = T)
dev.off()
