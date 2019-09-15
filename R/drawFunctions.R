# replacement for approx in handling rescaling of time/dist axis for plot pts
approxSegments <- function(xvar,yvar,segment,npoints,toofar=0) {
  if (!is.vector(xvar) | !is.vector(yvar) | !is.vector(segment))
    stop("approxSegments needs 3 vectors")
  if (length(xvar) != length(yvar))
    stop("approxSegments needs equal length xvar and yvar")
  if (length(xvar) != length(segment))
    stop("approxSegments needs a segment for every x,y pair")

  xout <- seq(from=xvar[1], to=xvar[length(xvar)], length.out=npoints)

  #  xvar is the independent variable, and is increasing (as is segment)
  #  average the non missing values y at duplicate (x,s) points
  dfwork <- tibble::as_tibble(list(x=xvar,y=yvar,segment=segment)) %>%
    dplyr::group_by(x,segment) %>%
    dplyr::summarize(ymean=mean(y,na.rm=TRUE))
  xvar <- dfwork$x
  segment <- dfwork$segment
  yvar <- dfwork$ymean
  yvar[is.nan(yvar)] <- NA

  #  how far apart are the x points
  xincr <- (xout[2] - xout[1])/2

  if (toofar>0) {
    xtoofar <- c(diff(xvar)>toofar,FALSE)
  }
  else {
    xtoofar <- rep(FALSE,length(xvar))
  }
  xidx <- findInterval(xout,xvar,rightmost.closed=TRUE)

  #  if last entry, exact match, or yupper missing in same seg
  #       and ylower not too far in past, use ylower
  case1 <- (xidx == length(xvar)) |
           (abs(xout-xvar[xidx]) < 0.01*xincr) |
           (!xtoofar[xidx] &
            (is.na(yvar[xidx+1]) & segment[xidx]==segment[xidx+1]))
  #  otherwise, if ylower and yupper both present, use weighted average
  y1 <- yvar[xidx]
  case2 <- !case1 & !xtoofar[xidx] & !is.na(yvar[xidx]) & !is.na(yvar[xidx+1])
  wt <- (xout - xvar[xidx])/(xvar[xidx+1] - xvar[xidx])
  y2 <- y1 + wt*(yvar[xidx+1]-yvar[xidx])
  #  all others, return NA
  yout <- rep(NA,npoints)
  yout[case2] <- y2[case2]
  yout[case1] <- y1[case1]

  return(list("xout"=xout,"yout"=yout))
}
#  return hr/cad legend width
dLegendWidth <- function(npoints,distPerPoint,minNumPoints) {
  return( distPerPoint*min(npoints,2000)/13 )
}
# return the number of points on the x-axis for data
numPointsXAxis <- function(dist,ppm,imperial) {
  miles <- ifelse(imperial,dist,milesFromMeters(1000*dist))
  if (!is.na(ppm)&ppm>=10) {
    return(ceiling(ppm*miles))
  } else {
    distbends <- c(0,5,10,35,85,200,Inf)       # begin at 0, end at max distance
    pointsbends <- c(0,2200,3300,4500,6500,10000,10000) # begin at 0, end at max
    pointsbends <- c(0,800,1600,5600,13600,15000,15000) # begin at 0, end at max
    pointsbends <- c(0,2000,3000,6000,15000,18000,18000) # begin at 0, end at max
    return(ceiling(pointsbends[which(distbends>miles)[1]-1] +
                     ( (pointsbends[which(distbends>miles)[1]]-
                          pointsbends[which(distbends>miles)[1]-1])/
                         (distbends[which(distbends>miles)[1]]-
                            distbends[which(distbends>miles)[1]-1]) )*
                     (miles-distbends[which(distbends>miles)[1]-1])))
  }
}
#  return vertical scaling factor for profile
verticalMult <- function(dist,imperial) {
  miles <- ifelse(imperial,dist,milesFromMeters(1000*dist))
  distbends <- c(0,10,35,85,200,Inf) # begin at 0, end max distance
  vertbends <- c(15,17,21,25,30,40)  # begin at 15, end 40
  vm <-
    ceiling(vertbends[which(distbends>miles)[1]-1] +
                        ( (vertbends[which(distbends>miles)[1]]-
                           vertbends[which(distbends>miles)[1]-1]) /
                          (distbends[which(distbends>miles)[1]]-
                           distbends[which(distbends>miles)[1]-1]) ) *
                        (miles-distbends[which(distbends>miles)[1]-1]) )
  return(vm)
}
heightWith <- function(ordervec,showTime,plotscale) {
  headerH <- heightXAxis(plotscale) + heightTAxis(plotscale) +
    + height("connector",plotscale)
  nlegends <- sum(!is.na(ordervec))
  nbands <- 2*nlegends
  return( ifelse(showTime,headerH,heightXAxis(plotscale)) +
          nlegends*height("label",plotscale) +
          nbands*height("band",plotscale)
  )
}
heightTAxis <- function(plotscale) {
  return(height("axisToLegend",plotscale)+
           height("axisLabel",plotscale)+
           5*height("gap",plotscale))
}
heightXAxis <- function(plotscale) {
  return(height("axisToLegend",plotscale)+
           height("axisLabel",plotscale)+
           3*height("gap",plotscale))
}
height <- function(what,plotscale) {
  if (what=="label") return(20/plotscale)
  else if (what=="band") return(35/plotscale)
  else if (what=="gap") return(3/plotscale)
  else if (what=="connector") return(100/plotscale)
  else if (what=="summary") return(200/plotscale)
  else if (what=="axisToLegend") return(20/plotscale)
  else if (what=="axisLabel") return(35/plotscale)
  else stop(paste0("don't know what ",what," is"))
}
milesFromMeters <- function(meters) {
  return(meters/1609.34)
}
kmFromMeters <- function(meters) {
  return(meters/1000)
}
feetFromMeters <- function(meters) {
  return(meters*3.28084)
}
pointsFromMatrix <- function(dataMat) {
  dmCol <- ncol(dataMat)
  dmRow <- nrow(dataMat)
  row <- matrix(rep(seq(1:dmRow),dmCol),ncol=dmCol)
  col <- t(matrix(rep(seq(1:dmCol),dmRow),ncol=dmRow))
  dmPoint <- !is.na(as.vector(dataMat))
  return(as_tibble(list(x=as.vector(col)[dmPoint],
                        y=as.vector(row)[dmPoint],
                        z=as.vector(dataMat)[dmPoint])))
}

yRatio <- function(rrr) {
  xmin <- rrr@extent@xmin
  xmax <- rrr@extent@xmax
  ymin <- rrr@extent@ymin
  ymax <- rrr@extent@ymax
  return(yRatioPts(xmin,xmax,ymin,ymax))
}
yRatioPts <- function(xmin,xmax,ymin,ymax) {
  width <-
    (raster::pointDistance(cbind(xmin,ymin),cbind(xmax,ymin),lonlat=TRUE) +
     raster::pointDistance(cbind(xmin,ymax),cbind(xmax,ymax),lonlat=TRUE)) / 2
  height <-
    (raster::pointDistance(cbind(xmin,ymin),cbind(xmin,ymax),lonlat=TRUE) +
     raster::pointDistance(cbind(xmax,ymin),cbind(xmax,ymax),lonlat=TRUE)) / 2
  return(height/width)
}
##  note that lag_one/lead_one pad the new entry with the first/last value,
##      which is different than lag_n/lead_n(,1)
##    this gives flexibility with differences, but be careful!

lag_one <- function(vec) {
  return(c(vec[1],vec[-length(vec)]))
}
lead_one <- function(vec) {
  return(c(vec[-1],vec[length(vec)]))
}
lag_n <- function(vec,n) {
  if (n < length(vec)) {
    return(c(rep(NA,n),vec[1:(length(vec)-n)]))
  }
  else {
    return(vec<-NA)
  }
}
lead_n <- function(vec,n) {
  if (n < length(vec)) {
    return(c(vec[-n:-1],rep(NA,n)))
  }
  else {
    return(vec<-NA)
  }
}
smoothDataSegments <- function(yvec,xvar,segment,
                               bw,nneighbors=10,kernel="epanechnikov",
                               replaceNAs=TRUE) {
  if (!is.vector(xvar)) stop("smoothDataSegments needs xvar as vector")
  if (missing(segment)) segment <- rep(1,length(xvar))
  if (length(xvar)!=length(segment))
    stop("smoothDataSegments needs segment and xvar same length")

  xret <- vector("numeric",length(xvar))
  xret <- NA  #  if anything goes wrong return garbage
  for (seg in unique(segment)) {
    inseg <- segment == seg
    if (sum(inseg)>1) {
      xret[inseg] <- smoothData(yvec=yvec[inseg],xvar=xvar[inseg],
                                bw=bw,nneighbors=nneighbors,
                                kernel=kernel,replaceNAs=replaceNAs)
    } else {
      xret[inseg] <- xvar[inseg]
    }
  }
  return(xret)

}
smoothData <- function(yvec,xvar,bw,nneighbors=10,
                       kernel="epanechnikov",replaceNAs=TRUE) {
  if (!is.vector(xvar)) stop("smoothData needs xvar as vector")
  if (!is.vector(yvec)) stop("smoothData needs yvec as vector")
  if (length(xvar)==0) stop("smoothData needs some points")
  if (length(yvec)!=length(xvar))
    stop("smoothData needs xvar and yvec same length")
  #  ignore the scale factor, since we are using the kernel for weighting
  #            numerator and denominator
  #  note this function fills in missing values with approximation unless
  #   replaceNAs is FALSE
  if (!(is.vector(yvec) & is.vector(xvar) & length(yvec)==length(xvar)))
    stop("need 2 vectors of same length in smoothData")
  ypresent <- as.numeric(!is.na(yvec))
  num <- ypresent*yvec
  den <- ypresent
  xvar <- as.numeric(xvar)
  for (i in 1:nneighbors) {
    if (kernel == "triangular") {
      twt <- 1.0-((lead_n(xvar,i)-xvar)/bw)
    } else if (kernel == "epanechnikov") {
      twt <- 1.0-((lead_n(xvar,i)-xvar)/bw)*((lead_n(xvar,i)-xvar)/bw)
    } else {
      stop("invalid kernel")
    }
    twt[is.na(twt)] <- 0
    y <- lead_n(yvec,i)
    num[!is.na(y)&(twt>0)] <-
      num[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]*y[!is.na(y)&(twt>0)]
    den[!is.na(y)&(twt>0)] <-
      den[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]

    if (kernel == "triangular") {
      twt <- 1.0-((lag_n(xvar,i)-xvar)/bw)
    } else if (kernel == "epanechnikov") {
      twt <- 1.0-((lag_n(xvar,i)-xvar)/bw)*((lag_n(xvar,i)-xvar)/bw)
    } else {
      stop("invalid kernel")
    }
    twt[is.na(twt)] <- 0
    y <- lag_n(yvec,i)
    num[!is.na(y)&(twt>0)] <-
      num[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]*y[!is.na(y)&(twt>0)]
    den[!is.na(y)&(twt>0)] <-
      den[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]
  }
  retvec <- num/den # if only NAs in neighbors, return NA
  if (!replaceNAs) retvec[is.na(yvec)] <- NA
  return(retvec)
}

