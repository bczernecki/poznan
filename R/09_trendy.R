# trendy

library(readxl)
library(dplyr)
library(tidyr)
library(reshape)
library(plyr)
library(tidyverse)
library(RColorBrewer)

dane <- read_excel("/home/bartosz/github/poznan/data/xls/calosc_z_imgw.xls")
dane <- select(dane, yy, mm, poznan_rekonstr)

# przetestowanie wartosci statystyki testowej p-value
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic; p <- pf(f[1],f[2],f[3],lower.tail=F); attributes(p) <- NULL
  return(p)
}


policztrend <- function(dane, ileokresow=12){
trendy <- dane %>% group_by(mm) %>% do(model = lm(poznan_rekonstr ~ yy, data = .))
trendy_rok <- lapply(trendy$model, coef)  %>% unlist() %>% matrix(ncol=2, byrow=T) %>% round(., 3)%>% 
  cbind.data.frame(., trendy$mm) %>% .[,-1]

lapply(trendy$model, lmp)
pvalue1 <- lapply(trendy$model, lmp)  %>% unlist() %>%   cbind.data.frame(., trendy$mm) 
names(pvalue1) <- c("pv","nazwa")
pvalue1 <- data.frame(nazwa=pvalue1$nazwa, sez=rep(1:ileokresow), p=format(round(pvalue1$pv,5), scientific = F), trend=trendy_rok[,1])
#format(pvalue1$slope, scientific = F)
return(pvalue1)

}

policztrend(dane)
policztrend(filter(dane,yy<=1902))
policztrend(filter(dane,yy>1902 & yy<1987))
policztrend(filter(dane,yy>=1987))

sezony <- dane
sezony$yy <- ifelse(sezony$mm==12, sezony$yy+1, sezony$yy) # grudzien musimy 'przesunac' o rok
sezony$mm <- ifelse(sezony$mm %in% c(12,1,2), 96, sezony$mm) # zima
sezony$mm <- ifelse(sezony$mm %in% c(3:5), 97, sezony$mm) # wiosna
sezony$mm <- ifelse(sezony$mm %in% c(6:8), 98, sezony$mm)
sezony$mm <- ifelse(sezony$mm %in% c(9:11), 99, sezony$mm)
sezony <- sezony %>% group_by(yy,mm) %>% dplyr::summarise(poznan_rekonstr=mean(poznan_rekonstr, na.rm=T))
#sezony <- sezony %>% filter(yy!=1848 & mm!=96) 
#sezony <- sezony[-which(sezony$yy==1848 & sezony$mm==96),]


policztrend(sezony, ileokresow = 4)
policztrend(filter(sezony,yy<=1902), ileokresow = 4)
policztrend(filter(sezony,yy>1902 & yy<1987), ileokresow = 4)
policztrend(filter(sezony,yy>=1987), ileokresow = 4)


policztrend(sezony, ileokresow = 4)

# roczne
# pelna seria:
roczne <- dane %>% group_by(yy) %>% dplyr::summarise(poznan_rekonstr=mean(poznan_rekonstr, na.rm=T))
model = lm(poznan_rekonstr ~ yy, data = roczne)
summary(model)
coef(model)

# do 1902:
roczne2 <- filter(roczne, yy<=1902)
model = lm(poznan_rekonstr ~ yy, data = roczne2)
summary(model)
coef(model)

# 1903-1986
roczne3 <- filter(roczne, yy>1902, yy<=1986)
model = lm(poznan_rekonstr ~ yy, data = roczne3)
summary(model)
coef(model)

# 1987 - onwards
roczne4 <- filter(roczne, yy>1986)
model = lm(poznan_rekonstr ~ yy, data = roczne4)
summary(model)
coef(model)

