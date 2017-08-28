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
ocus <- efp(dane ~ 1, type = "OLS-CUSUM")
plot(ocus, functional = "meanL2")
sctest(ocus, functional = "meanL2")



## estimate breakpoints
bp <- breakpoints(dane ~ 1)
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

