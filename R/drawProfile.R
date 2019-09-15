#  create the basic plot, with reserved room below
#  elevation and speed are smoothed already if that's what is wanted
drawProfile <- function(distancevec,elevationvec,speedvec,
                        distPerPoint,palette,naPlotColor,
                        vertMult,npoints,minNumPoints,
                        elevationShape,imperial,
                        orderBands,showTime) {

  ngraphpoints <- max(minNumPoints,npoints)
  dist <- distancevec[length(distancevec)]
  if (is.na(vertMult)|vertMult<=1)
    vertMult <- verticalMult(dist,imperial)
  heightFactor=vertMult/50
  heightBelow <- heightWith(ordervec=orderBands,showTime=showTime,
                            plotscale=heightFactor)

  #  use equally spaced grid for plotting
  eProfilePts <- stats::approx(distancevec,elevationvec,n=npoints)
  sProfilePts <- stats::approx(distancevec,speedvec,n=npoints)
  distance <- eProfilePts[[1]]
  elevation <- eProfilePts[[2]]
  speed <- sProfilePts[[2]]
  #  need to do color scaling
  if (imperial) {
    speed[speed<3] <- 3
    speed[speed>40] <- 40
    elevalpha <-
      0.4 + 0.6*(ifelse(speed > 15,ifelse(speed<40,(speed-15)/25,1),0))
    elevround <- 200
  }
  else {
    speed[speed<5] <- 5
    speed[speed>67] <- 67
    elevalpha <-
      0.4 + 0.6*(ifelse(speed > 25,ifelse(speed<67,(speed-12)/25,1),0))
    elevround <- 100
  }
  elevprtchar <- rep("|",length(distance))

# set limits for plots
  elevMin <- min(elevation)
  if (elevMin < 0) {
    elevMinInt <- 100*floor(elevround*floor(elevMin/elevround)/100)
  } else {
    elevMinInt <- 500*floor(elevround*floor(elevMin/elevround)/500)
  }
  elevMinShade <- max(0,elevMinInt)
  elevMinShade <- elevMinInt
  ymin <- elevMinInt - heightBelow - 50
  ybottom <- ymin

  elevMax <- max(elevation)
  elevMaxInt <- elevround*ceiling(elevMax/elevround)
  ymax <- 500*ceiling((elevMaxInt + height("summary",heightFactor))/500)

  xmin <- 0
  xmax <- distPerPoint*ngraphpoints

  plotdata <- data.frame(distance,elevation,speed,elevprtchar,elevalpha)
  major.breaks <- ifelse(dist>100,10,ifelse(dist>10,5,1))
 #  aspect.ratio <- vertMult*(ymax-ymin)/(5280*(ngraphpoints)*distPerPoint)

  g <- ggplot2::ggplot(plotdata,aes(x=distance,y=elevation)) +
    ggplot2::scale_y_continuous(limits=c(ymin,ymax),expand=c(0,0),
                                breaks=seq(from=0,to=ymax,by=500),
                                minor_breaks=seq(0,ymax,100)) +
    ggplot2::scale_x_continuous(limits=c(xmin,xmax),expand=c(0,0),
                                breaks=seq(from=0,to=xmax,by=major.breaks),
                                minor_breaks=seq(0,xmax,1)) +
 #  ggplot2::theme(aspect.ratio=aspect.ratio) +
    ggplot2::theme(legend.title=
                     ggplot2::element_text(colour="steelblue",size=7)) +
    ggplot2::theme(legend.text=
                     ggplot2::element_text(colour="lightsteelblue",size=6)) +
    ggplot2::theme(legend.key.size=unit(10,"points")) +
    ggplot2::theme(legend.justification="top") +
 #  suppress x axis, add it manually
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank(),
                   axis.line.x=ggplot2::element_blank(),
                   axis.line.y=ggplot2::element_blank(),
                   panel.background=ggplot2::element_rect(fill="lightblue1",
                                                  colour="lightblue1",
                                                  size=0.5,linetype="solid"),
                   panel.grid.major=ggplot2::element_line(size=0.3,
                                                  linetype='solid',
                                                  colour="steelblue1"),
                   panel.grid.minor=ggplot2::element_line(size=0.15,
                                                  linetype='solid',
                                                  colour="steelblue1")) +
    ggplot2::labs(y=paste0("Elevation (",
                           ifelse(imperial,"ft)","m)"))) +
    ggplot2::theme(axis.title.y=element_text(hjust=0.8)) +
    viridis::scale_color_viridis(option=palette,limits=c(0,40),
                                 na.value=naPlotColor,direction=-1) +
    viridis::scale_fill_viridis(option=palette,limits=c(0,40),
                                na.value=naPlotColor,direction=-1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymax=elevation,ymin=elevMinShade),
                         color="lightgreen",fill="lightgreen",
                         alpha=0.7) +
#   lay down a light background that lets some of the grid show through
    ggplot2::geom_ribbon(ggplot2::aes(ymax=elevMinShade,ymin=ymin),
                         color="white",fill="white",alpha=0.7)
    if (!is.na(orderBands[["speed"]])) {
      #  suppress ggplot2 legend if adding speed legend
      g <- g +
           ggplot2::geom_line(ggplot2::aes(color=speed),show.legend=F) +
           ggplot2::theme(legend.position="none")
    } else {
      g <- g + ggplot2::geom_line(ggplot2::aes(color=speed))
    }
#    ggplot2::geom_point(ggplot2::aes(color=speed),shape=elevationShape,
#                      size=1.25*heightFactor,alpha=0.6,
#                      position=ggplot2::position_nudge(y=1.2))
    if (npoints < ngraphpoints) {
      g <- g +
        ggplot2::geom_rect(xmin=npoints*distPerPoint,xmax=xmax,
                           ymin=ymin,ymax=ymax,color="white",fill="white")
    }

    return(list(g=g,xmin=xmin,xmax=xmax,xlast=distPerPoint*npoints,
              ymin=elevMinInt,ymax=ymax,ybottom=ybottom,vertmult=vertMult,
              distPerPoint=distPerPoint,heightFactor=heightFactor,
              npoints=npoints,ngraphpoints=ngraphpoints))
}
