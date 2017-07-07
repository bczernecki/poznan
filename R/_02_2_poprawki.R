# wroclaw - poprawki na miesiac

library(dplyr)
wroc <- read.table("~/Pulpit/wroclaw_diff.txt", dec = ",")
head(wroc)

wroc %>% group_by(V3) %>% summarise_all( mean) %>% select(V2) %>% pull() 



okecie <- read.table("352200375.csv", stringsAsFactors = F, header=T)
okecie$data <- as.Date(okecie$data)

okecie <- cbind(wyniki[,c(1:2,7)])
okecie <- na.omit(okecie)
colnames(okecie)[3] <- "okecie"
head(okecie)


obserwatorium <- read.table("~/Pulpit/wawa_obs.txt", header=F, stringsAsFactors = F, dec=",")
colnames(obserwatorium) <- c("yy",1:12)
head(obserwatorium)
obserwatorium2 <- gather(obserwatorium, key = "mm",value = "obserwatorium",2:13) %>% apply(., 2, as.numeric) %>% as.data.frame() %>% arrange(yy,mm)
head(obserwatorium2)

git <- left_join(okecie, obserwatorium2)
head(git)
git$diff <- git$okecie-git$obserwatorium
git %>% group_by(mm) %>% summarise(mean(diff, na.rm=T)) %>% .[,2] %>% pull %>% hist()




### krakow:

krak1 <- read.table("~/Pulpit/krakow_51_65.txt", stringsAsFactors = F)
str(krak1)
krak1$yy <- as.numeric(substr(krak1$V2,1,4))
krak1$mm <- as.numeric(substr(krak1$V2,5,6))

krak1 <- select(krak1, yy,mm,V3)
head(krak1)

#part2
krak2 <- read.table("~/Pulpit/krakow.txt", stringsAsFactors = F)
krak2$yy <- as.numeric(substr(krak2$V1,1,4))
krak2$mm <- as.numeric(substr(krak2$V1,5,6))
head(krak2)
krak2 <- krak2 %>% group_by(yy,mm) %>% summarise(V3=mean(V3)) 
head(krak1)

krak <- rbind.data.frame(krak1,krak2)
