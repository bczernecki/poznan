## 26/07/2017
## przygotowanie pod wykresy dla trojkata weimarskiego:
dane <- read_excel("data/xls/calosc_z_imgw.xls")
dane <- dane %>% dplyr::select(-matches("noaa"))

library(esd)
konwersja<-function (dane, loc = NA, param = NA, unit = NA, lon = NA, lat = NA, 
                     alt = NA, cntr = NA, longname = NA, stid = NA, quality = NA, 
                     src = NA, url = NA, reference = NA, info = NA, method = NA, 
                     type = NA, aspect = NA) {
  dates=as.Date(paste(dane$yy,dane$mm,01,sep="-"))
  data=dane$poznan_rekonstr
  y <- zoo(data, order.by = dates)
  attr(y, "location") <- NA
  attr(y, "variable") <- "t2m"
  attr(y, "unit") <- "deg C"
  attr(y, "longitude") <- NA
  attr(y, "latitude") <- NA
  attr(y, "altitude") <- NA
  attr(y, "country") <- "PL"
  attr(y, "longname") <- NA
  attr(y, "station_id") <- NA
  attr(y, "quality") <- NA
  attr(y, "calendar") <- "gregorian"
  attr(y, "source") <- NA
  attr(y, "URL") <- NA
  attr(y, "type") <- NA
  attr(y, "aspect") <- NA
  attr(y, "reference") <- NA
  attr(y, "info") <- NA
  attr(y, "method") <- NA
  attr(y, "history") <- history.stamp(NULL)
  class(y) <- c("station", "month", "zoo")
  return(y)
}

  tmp<-konwersja(dane=dane)
  tmp2 <- annual(tmp)
  plot(tmp2)
  lines(trend(tmp2),col="red",lwd=2)
  
  source("R/vis_trends_modified.R")
  
  svg(filename = "figs/trojkat_weimarski.svg")
  vis.trends2(tmp2, minlen=30, verbose=T, pmax=0.05, show.significance = T, 
                   varlabel = "Temperature", unitlabel="deg C", vmax=0.4)
  dev.off()
  
  head(dane)
  
  ## ponizej obliczenia dla trendu rocznego
a <- as.numeric(coredata(tmp2))
ab <- data.frame(rok=1848:2016, tmp2=a)
coef(lm(ab$tmp2~ab$rok))[2]*100 # wynik po przeliczeniu dla trendu na 100 lat
