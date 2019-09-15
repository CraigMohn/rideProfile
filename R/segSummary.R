# build tibble with segment data
#  segnum, begtime, endtime, stoptime, begdist, enddist
segSummary <- function(time,dist,segment,stopped,subsegment,
                       stopDistTolerance,
                       stopRunLength,...) {

  if ( !is.numeric(segment) | !all(diff(segment)>=0) )
    stop("segment must be nondecreasing integers")
  if ( !is.numeric(dist) | !all(diff(dist)>=0) )
    stop("dist must be nondecreasing numeric")
  if ( !is.numeric(time) | !all(diff(time)>=0) )
    stop("time must be nondecreasing numeric")

  newseg <- c(TRUE,(diff(segment)>0))
  endseg <- c((diff(segment)>0),TRUE)

  segstop <- tibble::as_tibble(list(time=time,
                                     dist=dist,
                                     segment=segment,
                                     stopped=stopped)) %>%
    dplyr::group_by(segment) %>%
    dplyr::mutate(maxdist=max(dist),
                  segbegtime=min(time),
                  segendtime=max(time)) %>%
    dplyr::mutate(timelaststop=lag_one(cummax(stopped*time))) %>%
    dplyr::mutate(movingrun=((time-timelaststop) > stopRunLength) |
                    ((time-segbegtime)  <= stopRunLength )) %>%
    dplyr::mutate(startofstop=!movingrun &
                    lag_one(movingrun) &
                    (maxdist-dist<stopDistTolerance) ) %>%
    dplyr::mutate(sosNA=ifelse(startofstop,1,NA)) %>%
    dplyr::summarize(timeStop=ifelse(sum(startofstop)==0,
                                     segendtime,
                                     min(sosNA*time,na.rm=TRUE)),
                     distStop=ifelse(sum(startofstop)==0,
                                     max(dist),
                                     min(sosNA*dist,na.rm=TRUE)))

  segdata <- tibble::as_tibble(list(time=time,
                                    dist=dist,
                                    segment=segment,
                                    subsegment=subsegment)) %>%
    dplyr::group_by(segment) %>%
    dplyr::summarize(timeMove=min(time[subsegment>0]),
                     distMove=min(dist[subsegment>0]),
                     timeStop=max(time[subsegment==1]),
                     distStop=max(dist[subsegment==1]))

  segdata$timeStop[segdata$timeStop < segstop$timeStop] <-
    segstop$timeStop[segdata$timeStop < segstop$timeStop]
  segdata$distStop[segdata$distStop < segstop$distStop] <-
    segstop$distStop[segdata$distStop < segstop$distStop]

  segSumFrame <- tibble::as_tibble(list(segment=segment[newseg],
                                        locBeg=segdata$distMove,
                                        locEnd=segdata$distStop,
                                        timeBeg=segdata$timeMove,
                                        timeEnd=segdata$timeStop))
  stopSumFrame <- tibble::as_tibble(list(stopNum=c(0,segment[newseg]),
                                         locBeg=c(0.0,segdata$distStop),
                                         locEnd=c(segdata$distMove,dist[length(dist)]),
                                         timeBeg=c(0.0,segdata$timeStop),
                                         timeEnd=c(segdata$timeMove,time[length(time)])))
  stopSumFrame <- stopSumFrame %>%
    dplyr::mutate(lenStop=timeEnd-timeBeg)


  return(list(segSumFrame=segSumFrame,stopSumFrame=stopSumFrame))
}

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