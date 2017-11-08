vis.trends2 <- function (x, unitlabel = "unit", varlabel = "", is = 1, pmax = 0.01, 
    minlen = 15, lwd = NA, vmax = NA, new = TRUE, show.significance = TRUE, 
    verbose = FALSE) 
{
    if (verbose) 
        print("vis.trends")
    T <- calculate.trends2(x, minlen = minlen, is = is, verbose = verbose)
    trends <- T$trends * 10
    p <- T$p
    cols <- as.numeric(colnames(trends))
    rows <- as.numeric(rownames(trends))
    significant <- ifelse(p < pmax, trends, NA)
    ticks <- seq(1, length(cols), signif(length(cols)/10, 1))
    if (is.na(lwd)) 
        lwd <- max(3 - 0.05 * length(cols), 0.2)
    if (is.na(vmax) | vmax == "q995") 
        vmax <- q995(abs(trends))
    if (vmax == "max") 
        vmax <- max(abs(trends), na.rm = T)
    if (vmax < 1) 
        vmax <- signif(vmax, 1)
    if (vmax > 1) 
        vmax <- signif(vmax, 2)
    dv <- signif(vmax/8, 1)
    v0 <- 0
    vstep <- seq(v0, vmax, dv)
    vstep <- unique(c(-1 * vstep, vstep))
    vstep <- vstep[order(vstep)]
    cticks <- vstep[2:length(vstep)] - dv/2
    # cmin <- rgb(239, 138, 98, max = 255)
    # cmid <- rgb(255, 0, 0, max = 255)
    # cmax <- rgb(103, 169, 207, max = 255)
    cmin <- "brown"
    cmid0 <- c("orange", "red")
    cmid1 <- c("white","white")
    cmid2 <- c("blue", "skyblue")
    cmax <- "darkblue"
    rgb.palette <- colorRampPalette(c(cmax, cmid2, cmid1, cmid0, cmin), space = "rgb")
    #image(matrix(1:12), col=rgb.palette(12))
    cstep <- rgb.palette(n = length(vstep) - 1)
    # if (new) 
    #     dev.new()
    image(cols, rows, t(trends), breaks = vstep, col = cstep, 
        xlab = "start year", ylab = "length of period (years)", 
        main = paste(c(varlabel, " trend (", unitlabel, "/decade)"), 
            collapse = ""))
    trends.plus <- t(trends)
    trends.plus[trends.plus < max(vstep)] <- NA
    image(cols, rows, trends.plus, col = cstep[length(cstep)], 
        add = TRUE)
    trends.minus <- t(trends)
    trends.minus[trends.minus > min(vstep)] <- NA
    image(cols, rows, trends.minus, col = cstep[1], add = TRUE)
    if (show.significance) {
        if (verbose) 
            print(paste("mark significant trends (p<", pmax, 
                ")", sep = ""))
        i <- which((is.finite(t(p)) & t(p) < pmax))
        x <- array(sapply(cols, function(x) rep(x, nrow(p))), 
            length(p))[i]
        y <- rep(rows, nrow(p))[i]
        matlines(rbind(x - 1/2, x + 1/2), rbind(y - 1/2, y - 
            1/2), col = "gray50", lwd = lwd, lty = 1)
        matlines(rbind(x - 1/2, x + 1/2), rbind(y + 1/2, y + 
            1/2), col = "gray50", lwd = lwd, lty = 1)
        matlines(rbind(x - 1/2, x - 1/2), rbind(y - 1/2, y + 
            1/2), col = "gray50", lwd = lwd, lty = 1)
        matlines(rbind(x + 1/2, x + 1/2), rbind(y - 1/2, y + 
            1/2), col = "gray50", lwd = lwd, lty = 1)
    }
    colbar(cticks, cstep, fig = c(0.85, 0.9, 0.65, 0.85))
}





calculate.trends2 <- function(x,minlen=15,is=1,verbose=FALSE){
  # Calculate trends of time series x
  if(verbose) print("calculate.trends - calculate trends for all subperiods")
  stopifnot(inherits(x,'zoo'))
  if(!is.null(dim(x))) {
    x <- subset(x,is=is)
    if(!is.null(dim(x))) {
      x <- apply(x,2,mean,na.rm=TRUE)
    }
  }
  if(!inherits(x,c("annual","season"))) {
    xm <- aggregate(x,by=as.yearmon(index(x)),FUN="mean")
    xy <- aggregate(xm,by=strftime(index(xm),"%Y"),FUN="mean")
    ny <- aggregate(xm,by=strftime(index(xm),"%Y"),FUN="nv")
    xy <- xy[ny==max(ny)] # exclude years with missing months
  } else xy <- x
  year <- as.numeric(index(xy))
  firstyear <- min(year):(max(year)-minlen+1)
  trendlen <- minlen:(diff(range(year))+1)
  #lastyear <- firstyear+minlen-1
  n <- length(firstyear)
  trends <- matrix(NA,n,n)
  rownames(trends) <- trendlen#firstyear
  colnames(trends) <- firstyear#lastyear
  p <- trends
  # speed up with apply?
  for (i in firstyear) {
    jvec <- i+trendlen[trendlen<=(max(year)-i+1)]-1#(i+minlen-1):(max(year)+1)
    for (j in jvec) {
      if(!is.na(xy[year==i]) & !is.na(xy[year==j])) {
        print(paste(i,j))
        ij <- which(year %in% i:j & !is.na(xy))
        ij.model <- lm(xy[ij]~year[ij])
        #ij.kendall <- Kendall(x[ij],year[ij])
        iout <- firstyear==i#as.numeric(colnames(trends))==j
        jout <- trendlen==(j-i+1)#as.numeric(rownames(trends))==i
        trends[jout,iout] <- ij.model$coefficients[2]
        p[jout,iout] <- anova(ij.model)$Pr[1]#ij.kendall$sl[1]
      }
    }
  }  
  return(list("trends"=trends,"p"=p))
}
