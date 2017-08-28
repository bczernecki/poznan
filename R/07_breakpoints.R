

# wyznaczenie trendów z użyciem paczki "bfast"

require(bfast)
require(strucchange)
require(sandwich)
require(forecast)
require(raster)
require(sp)
library(readxl)
require(ggplot2)
require(dplyr)

#example:
head(harvest)
plot(harvest)
fit <- bfast(harvest, season="harmonic", max.iter=2)
plot(fit, type="trend") 


#dane poznan_rekonstr
dane <- read_excel("data/xls/calosc_z_imgw.xls")
dane <- dane %>% dplyr::select(yy:poznan_rekonstr)

dane<-subset(dane, yy>=1848 & yy <= 2016)

dane <- ts(data=dane$poznan_rekonstr, start=1848, end=c(2016,12), frequency=12)
dane
graphics.off()
fit <- bfast(dane, season="harmonic", max.iter=2)

plot(fit, ANOVA = TRUE, type="trend")
graphics.off()

(plot(fit, ANOVA=TRUE)$slope)
wynik <- length(fit$output)
(coef(fit$output[[wynik]]$bp.Vt)[,2])


svg(filename = "figs/trend_component.svg", width = 7, height = 5)
plot(fit, ANOVA = TRUE, type="trend")
dev.off()

svg(filename = "figs/trend_breaks.svg", width = 8, height = 6)
(plot(fit, ANOVA=TRUE)$slope)
dev.off()

######### trendy dla sezonów z paczki openair - liczy się 11 minut [sic!]  
require(openair)
dane <- read_excel("data/xls/calosc_z_imgw.xls")
dane <- dane %>% dplyr::select(1,2,4)
dane
dates <- seq(as.POSIXct("1848-01-01", "GMT"), 
             as.POSIXct("2016-12-01", "GMT"), by = "1 month")
dates
dane$date <- dates
dane <- tbl_df(dane)

TheilSen(dane[1:50,], pollutant = "poznan_rekonstr",xlab="",type="season", text.col = "black", deseason = T,
         lab.frac = 0.97, lab.cex = 1, lty=0, cex=.1,  alpha = .005, dec.place = 3, ylab = expression("Temperature (" * degree * "C)"),
         trend = list(lty = c(1, 5), lwd = c(2, 1),col = c("black", "red")))


# trendy dla sezonów (szybszy sposób)
dane <- read_excel("data/xls/calosc_z_imgw.xls")
dane <- dane %>% dplyr::select(1,2,4)
dane

wiosna <-dane %>% group_by(yy) %>% filter(mm %in% c(3, 4, 5)) %>%
  summarise(wiosna=mean(poznan_rekonstr))
lato <-dane %>% group_by(yy) %>% filter(mm %in% c(6, 7, 8)) %>%
  summarise(lato=mean(poznan_rekonstr))
jesien <-dane %>% group_by(yy) %>% filter(mm %in% c(9, 10, 11)) %>%
  summarise(jesien=mean(poznan_rekonstr))
zima <-dane %>% group_by(yy) %>% filter(mm %in% c(12, 1, 2)) %>%
  summarise(zima=mean(poznan_rekonstr))
   
sezony <- round(cbind(wiosna, lato[,2], jesien[,2], zima[,2]),2)
head(sezony)

#histogramy dla sezonów

library(purrr)
library(tidyr)
library(ggplot2)
head(sezony)

sezony[,2:5] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


shapiro.test(sezony$zima)#zima nie ma rozkładu normalnego


dates <- seq(as.POSIXct("1848-01-01", "GMT"), as.POSIXct("2016-01-01", "GMT"), by = "1 year")
dates
sezony$date <- dates

head(sezony)

sezony <- tbl_df(sezony)


a <- TheilSen(sezony, pollutant = "wiosna",xlab="",date.breaks = 10, text.col = "black",
              lab.frac = 0.97, lab.cex = 1,    alpha = .005, dec.place = 3, ylab = expression("Temperature (" * degree * "C)"),
       trend = list(lty = c(1, 5), lwd = c(2, 1),col = c("black", "red")))
b <- TheilSen(sezony, pollutant = "lato",xlab="",date.breaks = 10, text.col = "black",
              lab.frac = 0.97, lab.cex = 1,    alpha = .005, dec.place = 3, ylab = expression("Temperature (" * degree * "C)"),
              trend = list(lty = c(1, 5), lwd = c(2, 1),col = c("black", "red")))
c <- TheilSen(sezony, pollutant = "jesien",xlab="",date.breaks = 10, text.col = "black",
              lab.frac = 0.97, lab.cex = 1,    alpha = .005, dec.place = 3, ylab = expression("Temperature (" * degree * "C)"),
              trend = list(lty = c(1, 5), lwd = c(2, 1),col = c("black", "red")))
d <- TheilSen(sezony, pollutant = "zima",xlab="",date.breaks = 10, text.col = "black",
              lab.frac = 0.97, lab.cex = 1,    alpha = .005, dec.place = 3, ylab = expression("Temperature (" * degree * "C)"),
              trend = list(lty = c(1, 5), lwd = c(2, 1),col = c("black", "red")))

svg(filename = "figs/trend_seasons.svg", width = 12, height = 8)
print(a, position = c(0, .5, .5, 1), more=T)
print(b, position = c(0.5, 0.5, 1, 1), more=T)
print(c, position = c(0, 0, 0.5, 0.5), more = T)
print(d, position = c(.5, 0, 1, 0.5), more = F)
dev.off()



