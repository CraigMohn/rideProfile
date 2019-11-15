drawXAxis <- function(ggp,distance,startsAndStops,
                      showStops,distPerPoint,imperial=TRUE,
                      underLine=FALSE,lineAtZero=FALSE) {
  ggpreturn <- ggp
  g <- ggp[["g"]]
  xmax <- ggp[["xmax"]]
  ngraphpoints <- ggp[["ngraphpoints"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  yXAxis <- ggp[["ymin"]]

  if (underLine) {
    if (lineAtZero) {
      yDistAxis <- yXAxis
      yDistScale <- yDistAxis - height("axisToLegend",heightFactor)
      yDistLabel <- yDistScale - height("axisLabel",heightFactor)
      ymin <- yDistLabel - 3*height("gap",heightFactor)
    } else {
      yDistAxis <- yXAxis  - height("gap",heightFactor)
      yDistScale <- yDistAxis - height("axisToLegend",heightFactor)
      yDistLabel <- yDistScale - height("axisLabel",heightFactor)
      ymin <- yDistLabel - height("gap",heightFactor)
    }
  } else {
    yDistLabel <- yXAxis - 2*height("gap",heightFactor) -
               height("axisLabel",heightFactor)
    yDistAxis <- yDistLabel - height("axisToLegend",heightFactor)
    yDistScale <- yDistAxis
    ymin <- yDistAxis
  }
  if (xmax < 2) {
    xincrement <- 0.5
    mincrement <- 0.5
  } else if (xmax < 5) {
    xincrement <- 1
    mincrement <- 0.25
  } else if (xmax < 10) {
    xincrement <- 2
    mincrement <- 0.5
  } else if (xmax < 50) {
    xincrement <- 5
    mincrement <- 1
  } else if (xmax < 100) {
    xincrement <- 10
    mincrement <- 5
  } else {
    xincrement <- 20
    mincrement <- 5
  }
  #  axis line
  distancegraphends <- c(0,npoints*distPerPoint)
  xAxisData <- data.frame(x=distancegraphends,y=yDistAxis)
  g <- g + ggplot2::geom_line(data=xAxisData,aes(x=x,y=y),alpha=1)

  stops <- startsAndStops[["stopSumFrame"]]
  if (showStops) {
    #  create data frames for stops/breaks
    #  classify seg breaks as short stops or long breaks, work in grid coords
    stopdata <-
      data.frame(distance=stops$locEnd,yDistAxis,lenStop=stops$lenStop)
    stopdata$pauseSize <- ifelse(stopdata$lenStop<300,
                                 1,
                                 1+log(stopdata$lenStop/300))
    stopdataShort <- stopdata[stopdata$lenStop<300,]
    stopdataLong <- stopdata[stopdata$lenStop>=300,]
    if (nrow(stopdataShort)>0) {
      g <- g +
        ggplot2::geom_point(data=stopdataShort,
                            aes(y=yDistAxis,alpha=0.8,size=pauseSize),
                            color="red1",shape=124,show.legend=FALSE)
    }
    if (nrow(stopdataLong)>0) {
      g <- g +
        ggplot2::geom_point(data=stopdataLong,
                            aes(y=yDistAxis,alpha=0.8,size=pauseSize),
                            color="purple3",shape=124,show.legend=FALSE)
    }
  }
  # axis labels
  axischarsize <- 2.25
  x <- seq(0,xmax,xincrement)
  xtext <- as.character(x)
  xhjust <- c(0,rep(0.5,length(x)-1))
  axisdata <- data.frame(x=x,y=yDistScale,label=xtext,hjust=xhjust)
  g <- g +
    ggplot2::geom_text(data=axisdata,aes(x=x,y=y,label=label,hjust=hjust,vjust=-0.25),
                       size=axischarsize)
  # axis ticks
  x <- seq(0,xmax,mincrement)
  axisdata2 <- data.frame(x=x,y=yDistAxis)
  g <- g +
    ggplot2::geom_point(data=axisdata2,aes(x=x,y=y),size=1.1,color="black",
                        shape=124,show.legend=FALSE)
  # axis title
  xAxisTextFrame <- data.frame(x=xmax/2,
                               y=yDistLabel,
                               label=ifelse(imperial,
                                            "Distance (mi)",
                                            "Distance (km)"))
  g <- g +
    ggplot2::geom_text(data=xAxisTextFrame,aes(x=x,y=y,label=label),
                       size=1.10*axischarsize,hjust=0.5,vjust=0,
                       color="black",show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
