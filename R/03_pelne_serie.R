library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
dane <- read_excel("data/xls/calosc_z_imgw.xls")
head(dane)
dane <- dane %>% select(-matches("noaa"))




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
srednia_ruchoma <- srednia_ruchoma[,c(1:9)]

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

#Now with daily mean temperatures. Preparation of the input files:
data(Ttest)
write(dat,'Ttest_1981-2000.dat')
write.table(est.c,'Ttest_1981-2000.est',row.names=FALSE,col.names=FALSE)
rm(dat,est.c)
#Daily series are quite noisy. Let's aggregate them into monthly values:
dd2m('Ttest',1981,2000)
#Homogenization of the monthly averages:
homogen('Ttest-m',1981,2000)
# If you have a history of the stations, you can edit the file
# Ttest_1981-2000_brk.csv to adjust the dates of the breaks to known relevant
# changes.
# Now we can obtain the homogenized daily series adjusting them to the breaks
# found at the monthly scale:
homogen('Ttest',1981,2000,dz.max=7,metad=TRUE)

## End(Not run)

#est.c <- data.frame(X=c())
#poznan poczdam warszawa praga wroclaw krakow szczecin
# do koordynat:
# Poczdam <- 52.3906° N, 13.0645° E # lon=14, lat=20
# Poznan <- 52.4064° N, 16.9252° E # lon=16
# Warszawa <- 52.2297° N, 21.0122° E # lon=20
