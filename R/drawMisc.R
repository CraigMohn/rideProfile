drawLegend <- function(ggp,dvar,xvar,y.band,
                       segment,toofar=0,
                       legendtext,
                       dLowD=NA,dTarget=NA,
                       dCont=TRUE,dLow=NA,dHigh=NA,
                       dColorLow=NA,dColorMid=NA,dColorHigh=NA,
                       minNumPoints) {
  ggpreturn <- ggp
  xend <- ggp[["xlast"]]
  g <- ggp[["g"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  distPerPoint=ggp[["distPerPoint"]]
  # column width vectors sum to 13 in bar functions
  dLegendWidth <- dLegendWidth(npoints,distPerPoint,minNumPoints)

  drawpts <- approxSegments(xvar=xvar,yvar=dvar,
                            segment=segment,npoints=npoints,
                            toofar=toofar)
  xvardraw <- drawpts[[1]]*(xend/max(drawpts[[1]]))
  xvardraw[xvardraw>distPerPoint*npoints] <- distPerPoint*npoints
  xvardraw[xvardraw<0] <- 0
  if (dCont) {
    g <- continuous_legend(g,legendtext=legendtext,
                           xvar=xvardraw,
                           lowval=dLow,hival=dHigh,
                           lowcolor=dColorLow,hicolor=dColorHigh,
                           legendwidth=dLegendWidth,y.bottom=y.band,
                           legend.height=height("label",heightFactor))
  } else {
    g <- discrete_legend(g,legendtext=legendtext,
                         xvar=xvardraw,
                         lowval=dLowD,hival=dTarget,
                         lowcolor=dColorLow,midcolor=dColorMid,
                         hicolor=dColorHigh,
                         legendwidth=dLegendWidth,y.bottom=y.band,
                         legend.height=height("label",heightFactor))
  }
  ggpreturn[["g"]] <- g
  return(ggpreturn)
}
drawBar <- function(ggp,dvar,xvar,y.band,
                    segment,toofar=0,
                    dTarget=NA,
                    dCont=TRUE,dLow=NA,dHigh=NA,
                    dColorLow=NA,dColorMid=NA,dColorHigh=NA,
                    minNumPoints) {
  ggpreturn <- ggp
  xend <- ggp[["xlast"]]
  g <- ggp[["g"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  distPerPoint=ggp[["distPerPoint"]]

  drawpts <- approxSegments(xvar=xvar,yvar=dvar,
                            segment=segment,npoints=npoints,
                            toofar=toofar)
  xvardraw <- drawpts[[1]]*(xend/max(drawpts[[1]]))
  xvardraw[xvardraw>distPerPoint*npoints] <- distPerPoint*npoints
  xvardraw[xvardraw<0] <- 0
  ddraw <- drawpts[[2]]
  if (dCont) {
    g <- continuous_band(g,xvar=xvardraw,vals=ddraw,
                         lowval=dLow,hival=dHigh,
                         lowcolor=dColorLow,hicolor=dColorHigh,
                         y.bottom=y.band,
                         band.height=height("band",heightFactor))
  } else {
    g <- discrete_band(g,xvar=xvardraw,vals=ddraw,
                      lowval=dLow,hival=dTarget,
                      lowcolor=dColorLow,midcolor=dColorMid,
                      hicolor=dColorHigh,
                      y.bottom=y.band,
                      band.height=height("band",heightFactor))
  }
  ggpreturn[["g"]] <- g
  return(ggpreturn)
}
addGap <- function(ggp,nrep=1) {

  ggpreturn <- ggp
  heightFactor=ggp[["heightFactor"]]
  distPerPoint=ggp[["distPerPoint"]]
  ymin <- ggp[["ymin"]] - nrep*height("gap",plotscale=heightFactor)
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
