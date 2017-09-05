### wszystko co jest ponizej to testy z roznymi paczkami
### TODO
### 26/07/2017

library(strucchange)
library(readxl)
library(dplyr)

dane <- read_excel("data/xls/calosc_z_imgw.xls")
dane <- dane %>% dplyr::select(yy:poznan_rekonstr)

dane <- dane %>% group_by(yy) %>% summarise_all(., mean) %>% dplyr::select(yy,poznan_rekonstr) 


dane <- ts(data=dane$poznan_rekonstr, start=1848, end=2016, frequency=1)


## fit, visualize and test OLS-based CUSUM test
## with a Cramer-von Mises functional

ocus <- efp(dane ~ 1, type="Rec-CUSUM", h=0.1)
plot(ocus, boundary=F)
bound.ocus <- boundary(ocus, alpha=0.05)
bound.ocus1 <- boundary(ocus, alpha=0.01)
lines(bound.ocus, col=4);lines(bound.ocus1, col="red")
#lines(-bound.ocus, col=4)
sctest(ocus)

ocus <- efp(dane ~ 1, type="OLS-MOSUM", h=0.1)
plot(ocus, boundary=F)
bound.ocus <- boundary(ocus, alpha=0.05)
bound.ocus1 <- boundary(ocus, alpha=0.01)
lines(bound.ocus, col=4);lines(bound.ocus1, col="red")


fs <- Fstats(dane ~ 1 , data = dane)
plot(fs, alpha=0.01)
## estimate breakpoints
bp <- breakpoints(dane ~ 1, h=.25)
plot(bp)
summary(bp)

bp.nile <- bp
summary(bp.nile)

## the BIC also chooses one breakpoint
plot(bp.nile)
breakpoints(bp.nile)


fm0 <- lm(dane ~ 1)
fm1 <- lm(dane ~ breakfactor(bp.nile, breaks = 1))
plot(dane)
str(fm1)

lines(ts(fitted(fm0), start = 1848), col = 3)
lines(ts(fitted(fm1), start = 1848), col = 4)
lines(bp.nile)

## confidence interval
ci.nile <- confint(bp.nile)
ci.nile
lines(ci.nile)




library(changepoint)
# change in variance
set.seed(1)
x=c(rnorm(100,0,1),rnorm(100,0,10))
ansvar=cpt.var(x)
plot(ansvar)
print(ansvar) # identifies 1 changepoint at 100
# change in mean
y=c(rnorm(100,0,1),rnorm(100,5,1))
ansmean=cpt.mean(y)
plot(ansmean,cpt.col='blue')
print(ansmean)
# change in mean and variance
z=c(rnorm(100,0,1),rnorm(100,2,10))
ansmeanvar=cpt.meanvar(dane,Q = 10)
plot(ansmeanvar,cpt.width=3)
print(ansmeanvar)
cpts.full(ansmeanvar)









### kod sciagniety ze stackoverflow

library("breakpoint")
library("changepoint")
library("RAD")
library("ggplot2")
## Get Data
tsdata <- read.csv("data/sample/mysql.bytes_received.csv", header = TRUE, sep = ",")
tsdata.value <- tsdata[,2]


## Use Breakpoint
bp.tsdata <- breakpoints(dane ~ 1)
bp.tsdata
b <- breakpoints(bp.tsdata,breaks = 5)
plot(dane)
abline(v=b$breakpoints+1848, col='red')
print(b)

## Use Changepoint
ansmean=cpt.mean(dane,Q = 5, penalty = "AIC",test.stat = "Normal")
ansmean
plot(ansmean,cpt.col='blue')

dane2 <- as.data.frame(dane)
CE.NB(dane2, Nmax = 10,      distyp = 2, penalty = "BIC", parallel = T)


## Poznan data with breakpoints. 
# on the basis of example from package "strucchange"

poznan <- breakpoints(dane ~ 1)
summary(poznan)
plot(poznan)

## compute breakdates corresponding to the
## breakpoints of minimum BIC segmentation
breakdates(poznan)

## confidence intervals
ci.poznan <- confint(poznan)
breakdates(ci.poznan)
ci.poznan

plot(dane, lwd=.3, ylab = expression("Temperature (" * degree * "C)"))
lines(ci.poznan)

dane1 <- read_excel("data/xls/calosc_z_imgw.xls")
dane1 <- dane1 %>% group_by(yy)  %>% summarise_all(., mean)%>%
                    dplyr::select(yy,poznan_rekonstr) 

trend <- lm(poznan_rekonstr ~yy, dane1)
summary(trend)
str(trend)
lines(ts(fitted(trend), start = 1848), col = 4, lwd=1.7, lty=2)

dane2 <- dane1 %>% group_by(yy)  %>% filter(yy<=1902) %>% summarise_all(., mean)%>%
  dplyr::select(yy,poznan_rekonstr)

dane3 <- dane1 %>% group_by(yy)  %>% filter(yy>=1902, yy<=1987) %>% summarise_all(., mean)%>%
  dplyr::select(yy,poznan_rekonstr)

dane4 <- dane1 %>% group_by(yy)  %>% filter(yy>=1987, yy<=2016) %>% summarise_all(., mean)%>%
  dplyr::select(yy,poznan_rekonstr)

trend2 <- lm(poznan_rekonstr ~yy, dane2)
trend3 <- lm(poznan_rekonstr ~yy, dane3)
trend4 <- lm(poznan_rekonstr ~yy, dane4)

summary(trend2)
summary(trend3)
summary(trend4)

lines(ts(fitted(trend2), start = 1848), col = 2, lwd=1.5)
lines(ts(fitted(trend3), start = 1902), col = 2, lwd=1.5)
lines(ts(fitted(trend4), start = 1987), col = 2, lwd=1.5)

text(1964, 10.4,  expression("" * beta * " = 0.010"), col="blue")
text(1964, 10.1,  "p = 0.000", col="blue")
text(1854, 10.4,  expression("" * beta * " = 0.007"), col="red")
text(1854, 10.1,  "p = 0.247", col="red")
text(1914, 10.4,  expression("" * beta * " = 0.002"), col="red")
text(1914, 10.1,  "p = 0.445", col="red")
text(1995, 10.4,  expression("" * beta * " = 0.045"), col="red")
text(1995, 10.1,  "p = 0.009", col="red")

#### koniec