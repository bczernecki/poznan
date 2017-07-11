library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
dane <- read_excel("data/xls/calosc_z_imgw.xls")
dane <- dane %>% dplyr::select(yy,mm,matches("poznan"))

dane2 <- dane
srednie <- dane2 %>% group_by(mm) %>% summarise(srednie=mean(poznan_rekonstr), odchylenie=sd(poznan_rekonstr))
dane2 <- left_join(dane2, srednie)

dane2$rekon_anomalie <- dane2$poznan_rekonstr-dane2$srednie # anomalie
dane2$rekon_anomalie_sd <- dane2$rekon_anomalie/dane2$odchylenie # odchylenie zestandaryzowane

ggplot(dane2, aes(yy,rekon_anomalie_sd, col=yy))+geom_line()+ggtitle("Comparison")+geom_smooth()+
  scale_x_continuous(breaks = seq(1850, 2015, by = 50))  + facet_wrap(~mm)
