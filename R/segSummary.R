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
