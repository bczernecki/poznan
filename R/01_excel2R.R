#######################
# bartosz: 28 cze 2017
# wczytanie wszystkich zbiorow danych excelowych i zapisanie do postaci waskiej
#######################

library(readxl)
library(dplyr)
library(tidyr)

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

a <- left_join(poznan,poczdam)

plot(a$poznan,a$poczdam, asp=1)
abline(a=0,b=1, col='red')
cor(a$poznan,a$poczdam, use='pairwise.complete.obs')
a[which(abs(a$poznan-a$poczdam)>3),]


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


# ploty kontrolne:
roczne <- b %>% group_by(yy) %>% summarise(poz=mean(poznan), poczdam=mean(poczdam), warszawa=mean(warszawa))
head(roczne)
plot(roczne$yy, roczne$poz-roczne$warszawa, type='l')
plot(roczne$yy, roczne$poczdam-roczne$warszawa, type='l')
