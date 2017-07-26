library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
dane <- read_excel("data/xls/calosc_z_imgw.xls")
dane <- dane %>% dplyr::select(-matches("noaa"))

dane2 <- dane
roczne <-  dane2 %>% group_by(yy) %>% summarise_all(mean)
sr_6190 <- roczne %>% filter(yy>=1961, yy<=1990) %>% colMeans()
#sr_6190 <- as.numeric(sr_6190[-1:-2])

srednie2 <- matrix(ncol=12, byrow = T, rep(sr_6190, times=dim(roczne)[1]))
anomalie <- as.matrix(roczne)-srednie2

# srednia ruchoma:
ma <- function(arr, n=15){
  res = arr
  
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n):i])
  }
  # if(sum(is.na(arr))>=1) res=NA
  res
}

library(zoo)
library(smooth)
rollmean(anomalie[,3], k=5, align = "center")
a <- sma(anomalie[,3], order = 5, intervals = 'none')

ma_anomalie <- apply(anomalie,2,function(x) ma(x, 7))
ma_anomalie <- apply(anomalie,2,function(x) as.numeric(sma(x, order = 5, intervals = 'none')$fitted))
svg("figs/corrected_series_v2.svg", height=6.5, width =11 )
plot(1848:2016,ma_anomalie[,3], type='l', ylim=c(-2,2), xaxs="i", xlim=c(1848,2016), ylab="Temperature anomalies [C]", xlab='', xaxt='n')
axis(1, at = seq(1850,2020, by=20), labels= seq(1850,2020, by=20))
lines(1848:2016,ma_anomalie[,4], type='l', col='blue')
brejki <- c(1847,1862,1867,1885,1892,1911,1919, 1920,2020)
abline(v= brejki, lty=2)
kolory <- sample(gray.colors(8))
for (i in 1:(length(brejki)-1)) polygon(x = rep(brejki[i:(i+1)], each=2), y= c(-3,-2,-2,-3), col = kolory[i]) # polygon1
for (i in 1:(length(brejki)-1)) polygon(x = rep(brejki[i:(i+1)], each=2), y= c(3,2,2,3), col = kolory[i]) # polygon1

adresy <- c("Pocztowa 1","Grobla 1", "Zielona 1", "Zielona 2", "Długa 3", "Wały Wazów", "Uniwersytet", "Poznań-Ławica")
for (i in 1:(length(brejki)-2)) text((brejki[i]+brejki[i+1])/2,1.7,srt=90, adresy[i], adj = 1)
text((brejki[8]+brejki[9])/2,1.6,srt=0, adresy[8])
legend(y=-1.4, x=1965, legend=c("raw data", "homogenized data"), lty=1, lwd=2, col=c("black","blue"), border=NA,  bty = "n" )
box()
dev.off()


dane2$rekon_anomalie <- dane2$poznan_rekonstr-dane2$srednie # anomalie
dane2$rekon_anomalie_sd <- dane2$rekon_anomalie/dane2$odchylenie # odchylenie zestandaryzowane

ggplot(dane2, aes(yy,rekon_anomalie_sd, col=yy))+geom_line()+ggtitle("Comparison")+geom_smooth()+
  scale_x_continuous(breaks = seq(1850, 2015, by = 50))  + facet_wrap(~mm)


roczne <-dane %>% group_by(yy) %>% summarise(poznan=mean(poznan), poznan_rekonstr=mean(poznan_rekonstr), poczdam=mean(poczdam), warszawa=mean(warszawa), praga=mean(praga), wroclaw=mean(wroclaw),
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
