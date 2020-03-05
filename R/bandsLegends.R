## to do - remove heightFactor argument

data_band <- function(ggp,xvar,cvals,segment) {

  #  map interval to color interval

  heightFactor <- ggp[["heightFactor"]]
  xend <- ggp[["xlast"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  distPerPoint=ggp[["distPerPoint"]]
  band.height <- height("band",heightFactor)
  y.bottom <- ggp[["ymin"]] - band.height

  #  now evenly space the xvars out
  drawpts <- approxSegments(xvar=xvar,yvar=cvals,
                            segment=segment,npoints=npoints,
                            toofar=0)
  xvardraw <- drawpts[[1]]*(xend/max(drawpts[[1]]))
  xvardraw[xvardraw>distPerPoint*npoints] <- distPerPoint*npoints
  xvardraw[xvardraw<0] <- 0
  ddraw <- drawpts[[2]]
  y.band <- rep(y.bottom + (band.height/2),length(xvardraw))

  valsDataFrame <- data.frame(xvar=xvardraw,cvals=ddraw,y.band)

  ggp[["g"]] <- ggp[["g"]] +
    ggplot2::geom_tile(data=valsDataFrame,
                       aes(y=y.band,x=xvar,fill=cvals,color=cvals),
                       alpha=0.6,
                       height=band.height,
                       show.legend = FALSE)
  ggp[["ymin"]] <- y.bottom
  return(ggp)
}
cont_legend <- function(legendtext,
                        lowval,hival,
                        lowcolor,hicolor,
                        legendwidth) {
  function(ggp) {

    #columns for legend - description,lowvalue,colorbar,hivalue
    heightFactor <- ggp[["heightFactor"]]
    y.bottom <- ggp[["ymin"]] - height("label",heightFactor)
    legend.height <- height("label",heightFactor)
    width.legend <- c(5,2,4,2)
    xvar <- seq(from=0,to=sum(width.legend)*legendwidth,length.out=1300)
    column.legend <- c(0,cumsum(width.legend)[-4])*legendwidth
    xtext.legend <- c(0,column.legend[3],column.legend[3],column.legend[4])
    ytext.legend <- rep(y.bottom+(legend.height/2),4)
    alpha.legend <- c(1,1,0,1)
    hjust.legend <- c(0,1,0,0)
    legendlabels <- c(legendtext,paste0(lowval," "),"",paste0("  ",hival))
    legendpos <- xvar>=column.legend[3] & xvar<=column.legend[4]
    prtlegend <- seq(lowcolor,hicolor,length.out=sum(legendpos))
    x.legend <- xvar[legendpos]
    y.legend <- rep(ytext.legend[1],length(x.legend))

    y.band <- rep(y.bottom+(legend.height/2),length(xvar))
    valsLegendFrame <- data.frame(x.legend,y.legend,prtlegend,legend.height)
    valsTextFrame <- data.frame(xtext.legend,ytext.legend,
                                legendlabels,alpha.legend,hjust.legend)
    ggp[["g"]] <- ggp[["g"]] +
      ggplot2::geom_tile(data=valsLegendFrame,
                         aes(y=y.legend,x=x.legend,fill=prtlegend,
                             color=prtlegend),
                         height=legend.height,alpha=1,show.legend = FALSE) +
      ggplot2::geom_text(data=valsTextFrame,
                         aes(x=xtext.legend,y=ytext.legend,label=legendlabels,
                             hjust=hjust.legend,alpha=alpha.legend),
                         size=2,color="black",fontface="italic",show.legend = FALSE)
    ggp[["ymin"]] <- y.bottom
    return(ggp)
  }
}
disc_legend <- function(legendtext,
                            lowval,hival,
                            lowcolor,midcolor,hicolor,
                            legendwidth) {
  function(ggp) {

    legendlabels <- c(legendtext,
                      paste0(" < ",lowval,"  "),
                      paste0(" ",lowval,"-",hival," "),
                      paste0(" >= ",hival," "))
    legendcolors <- c(NA,lowcolor,midcolor,hicolor)

    heightFactor <- ggp[["heightFactor"]]
    y.bottom <- ggp[["ymin"]] - height("label",heightFactor)
    legend.height <- height("label",heightFactor)
    width.legend <- c(4,3,3,3)
    x1.legend <- c(0,cumsum(width.legend[1:3]))*legendwidth
    x2.legend <- cumsum(width.legend)*legendwidth
    y1.legend <- y.bottom
    y2.legend <- y1.legend+legend.height

    xtext.legend <- c(0,(x1.legend[2:4]+x2.legend[2:4])/2)
    ytext.legend <- rep(y.bottom+(legend.height/2),4)
    alpha.legend <- c(0,1,1,1)
    hjust.legend <- c(0,0.5,0.5,0.5)
    valsTextFrame <- data.frame(x1.legend,x2.legend,y1.legend,y2.legend,
                                xtext.legend,ytext.legend,legendlabels,
                                legendcolors,alpha.legend,hjust.legend,
                                row.names=NULL)
    ggp[["g"]] <- ggp[["g"]] +
      ggplot2::geom_rect(data=valsTextFrame,
                         aes(xmin=x1.legend,xmax=x2.legend,fill=legendcolors,
                             alpha=alpha.legend),
                         ymin=y1.legend,ymax=y2.legend,inherit.aes=FALSE,
                         show.legend=FALSE) +
      ggplot2::geom_text(data=valsTextFrame,
                         aes(x=xtext.legend,y=ytext.legend,label=legendlabels,
                             hjust=hjust.legend),size=2,fontface="italic")
    ggp[["ymin"]] <- y.bottom
    return(ggp)
  }
}

addGap <- function(ggp,nrep=1) {

  ggpreturn <- ggp
  heightFactor=ggp[["heightFactor"]]
  distPerPoint=ggp[["distPerPoint"]]
  ymin <- ggp[["ymin"]] - nrep*height("gap",plotscale=heightFactor)
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}

