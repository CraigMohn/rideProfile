drawXTConnect <- function(ggp,distance,walltime,startsAndStops,
                          distPerPoint,hoursPerPoint) {
  ggpreturn <- ggp
  g <- ggp[["g"]]
  xmax <- ggp[["xmax"]]
  ngraphpoints <- ggp[["ngraphpoints"]]
  npoints <- ggp[["npoints"]]
  timelast <- walltime[length(walltime)]
  yXTConnect <- ggp[["ymin"]]
  heightFactor=ggp[["heightFactor"]]
  ymin <- yXTConnect - height("connector",heightFactor)
  yCenter <- (yXTConnect + ymin)/2
  xscale <- distPerPoint/(hoursPerPoint*3600)

  yTimeAxis <- ymin
  yDistAxis <- yXTConnect

  stops <- startsAndStops[["stopSumFrame"]]

   #stop begin time,stop end times, stop location
  pointsStopBeg <- data.frame(group=stops$stopNum,
                              x=stops$locBeg,
                              y=yDistAxis)
  pointsStopEnd <- data.frame(group=stops$stopNum,
                              x=stops$locEnd,
                              y=yDistAxis)
  pointsTimeBeg <- data.frame(group=stops$stopNum,
                              x=xscale*stops$timeBeg,
                              y=yTimeAxis)
  pointsTimeEnd <- data.frame(group=stops$stopNum,
                              x=xscale*stops$timeEnd,
                              y=yTimeAxis)
  stopData <- rbind(pointsStopBeg,pointsStopEnd,pointsTimeBeg,pointsTimeEnd)
  g <- g +
     ggplot2::geom_polygon(data=stopData,
                           aes(x=x,y=y,group=group),fill="red3",alpha=0.5,
                           show.legend=FALSE)
   XTConnTextFrame <- data.frame(x=xmax/2,y=yCenter,label="Stops")
   g <- g +
       ggplot2::geom_text(data=XTConnTextFrame,aes(x=x,y=y,label=label),
                          size=3.3,hjust=0.5,vjust=0.5,
                          color="red",show.legend = FALSE)

  hourtimes <- seq(0,timelast,3600)
  hourcolors <- rep(40,length(hourtimes))
  houralphas <- rep(0.5,length(hourtimes))
  if (timelast <= 7200 & timelast >= 900) {
    quartertimes <- seq(900,timelast,3600)
    if (timelast >= 1800)
      quartertimes <- c(quartertimes,seq(1800,timelast,3600))
    if (timelast >= 2700)
      quartertimes <- c(quartertimes,seq(2700,timelast,3600))
    quartercolors <- rep(35,length(quartertimes))
    quarteralphas <- rep(0.25,length(quartertimes))
    hourtimes <- c(hourtimes,quartertimes)
    hourcolors <- c(hourcolors,quartercolors)
    houralphas <- c(houralphas,quarteralphas)
  }
  hourplaces <- approx(x=walltime,y=distance,hourtimes)[[2]]

  XTConnectData <- data.frame(x=c(hourplaces),
                              xend=c(hourtimes),
                              color=c(hourcolors),
                              alpha=c(houralphas),
                              y=yDistAxis,
                              yend=yTimeAxis,
                              stringsAsFactors=FALSE)
  XTConnectData$xend <- xscale*XTConnectData$xend
  g <- g +
    ggplot2::geom_segment(data=XTConnectData,
                          aes(x=x,y=y,xend=xend,yend=yend,color=color,alpha=alpha),
                          show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
