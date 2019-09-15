drawTAxis <- function(ggp,walltime,startsAndStops,distPerPoint,hoursPerPoint) {
  ggpreturn <- ggp
  g <- ggp[["g"]]
  xmax <- ggp[["xmax"]]
  ngraphpoints <- ggp[["ngraphpoints"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  yTAxis <- ggp[["ymin"]]
  ymin <- yTAxis - heightTAxis(heightFactor)

  stops <- startsAndStops[["stopSumFrame"]]
  segs <- startsAndStops[["segSumFrame"]]

  #  axis line - color coded for stops
  xscale <- distPerPoint/hoursPerPoint
  tAxisSegData <- data.frame(x=segs$timeBeg,xend=segs$timeEnd,xcol=40,y=yTAxis)
  tAxisStopData <- data.frame(x=stops$timeBeg,xend=stops$timeEnd,xcol=15,y=yTAxis)
  tAxisData <- rbind(tAxisSegData,tAxisStopData)
  tAxisData$x <- (xscale/3600)*tAxisData$x
  tAxisData$xend <- (xscale/3600)*tAxisData$xend
  g <- g +
    geom_segment(data=tAxisData,
                 aes(x=x,y=y,xend=xend,yend=y,color=xcol))
  tmax <- (xmax/xscale)
  tincr <- round(exp(log(10)*floor(log10(tmax))))

  # axis numbers for hours and fractions
  axischarsize <- 2.75
  if (tincr > 0) {
    t <- seq(0,tmax,tincr)
    ttext <- as.character(t)
    t <- 3600*t
    tAxisLabels <- data.frame(x=t,y=yTAxis,ttext=ttext)
  } else {
    tAxisLabels <- data.frame(x=0,y=yTAxis,ttext="0")
  }
  tAxisLabels$x <- (xscale/3600)*tAxisLabels$x
  tAxisLabels$y <- yTAxis - height("axisToLegend",heightFactor)
  if (nrow(tAxisLabels) >= 2) {
    tAxisLabels$hjust <- c(0,rep(0.5,nrow(tAxisLabels)-2),1)
  } else {
    tAxisLabels$hjust <- c(0,rep(0,nrow(tAxisLabels)-1))
  }
  g <- g +
    geom_text(data=tAxisLabels,aes(x=x,y=y,label=ttext,hjust=hjust),vjust=0,
              size=axischarsize,show.legend = FALSE)
  if (tmax <3 & tmax >= 0.25) {
    df15 <- data.frame(x=seq(900,tmax*3600,3600),y=yTAxis,ttext="1/4")
    if (tmax >= 0.5)
      df15 <- rbind(df15,
                    data.frame(x=seq(1800,tmax*3600,3600),y=yTAxis,ttext="1/2"))
    if (tmax >= 0.75)
      df15 <- rbind(df15,
                    data.frame(x=seq(2700,tmax*3600,3600),y=yTAxis,ttext="3/4"))
    df15$x <- (xscale/3600)*df15$x
    df15$y <- yTAxis - .8*height("axisToLegend",heightFactor)
    g <- g +
      geom_text(data=df15,aes(x=x,y=y,label=ttext),hjust=0.5,vjust=0,
                size=0.75*axischarsize,show.legend = FALSE)
  }
  #  axis ticks
  incr <- ifelse(tmax > 6,xscale,xscale/4)
  t <- seq(0,xscale*tmax,incr)
  axisdata2 <- data.frame(x=t,y=yTAxis)
  g <- g +
    ggplot2::geom_point(data=axisdata2,aes(x=x,y=y),size=1.2,color="black",
                        shape=124,show.legend=FALSE)
  # axis label
  ylabel <- yTAxis-height("axisToLegend",heightFactor) -
    height("axisLabel",heightFactor)
  tAxisTextFrame <- data.frame(x=xmax/2,
                               y=ylabel,
                               label="Time (hrs)")
  g <- g +
    ggplot2::geom_text(data=tAxisTextFrame,aes(x=x,y=y,label=label),
                       size=1.10*axischarsize,hjust=0.5,vjust=0,
                       color="black",show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}

