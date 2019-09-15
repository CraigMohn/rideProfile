continuous_band <- function(g,xvar,vals,
                            lowval,hival,
                            lowcolor,hicolor,
                            y.bottom,band.height) {

  #  map interval to color interval
  prtvalue <- lowcolor + (hicolor-lowcolor)*(vals-lowval)/(hival-lowval)
  prtvalue[!is.na(vals) & vals<lowval] <- lowcolor
  prtvalue[!is.na(vals) & vals>hival] <- hicolor
  prtvalue[vals==0] <- NA
  prtalpha <- (prtvalue-lowcolor)/(hicolor-lowcolor) #higher vals more intense
  prtalpha[prtalpha<0.3] <- 0.3

  y.band <- rep(y.bottom + (band.height/2),length(xvar))
  valsDataFrame <- data.frame(xvar,prtvalue,prtalpha,vals,y.band,band.height)
  g <- g +
    ggplot2::geom_tile(data=valsDataFrame,
                       aes(y=y.band,x=xvar,fill=prtvalue,color=prtvalue),
                       alpha=0.6,
                       height=band.height,
                       show.legend = FALSE)
  return(g)
}
discrete_band <- function(g,xvar,vals,
                          lowval,hival,
                          lowcolor,midcolor,hicolor,
                          y.bottom,band.height) {

  prtvalue  <-  rep(NA,length(vals))
  prtvalue[!is.na(vals) & vals>0 & vals<lowval] <- lowcolor
  prtvalue[!is.na(vals) & vals>=lowval & vals<hival] <- midcolor
  prtvalue[!is.na(vals) & vals>=hival] <- hicolor

  y.band <- y.bottom + (band.height/2)
  valsDataFrame <- data.frame(xvar,y.band,vals,prtvalue,band.height,
                              row.names=NULL)
  g <- g +
    ggplot2::geom_tile(data=valsDataFrame,
                       aes(y=y.band,x=xvar,fill=prtvalue,color=prtvalue),
                       height=band.height,show.legend=FALSE)
  return(g)
}
continuous_legend <- function(g,legendtext,xvar,
                              lowval,hival,
                              lowcolor,hicolor,
                              legendwidth,y.bottom,
                              legend.height) {

  #columns for legend - description,lowvalue,colorbar,hivalue
  width.legend <- c(5,2,4,2)
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
  g <- g +
    ggplot2::geom_tile(data=valsLegendFrame,
                       aes(y=y.legend,x=x.legend,fill=prtlegend,
                           color=prtlegend),
                       height=legend.height,alpha=1,show.legend = FALSE) +
    ggplot2::geom_text(data=valsTextFrame,
                       aes(x=xtext.legend,y=ytext.legend,label=legendlabels,
                           hjust=hjust.legend,alpha=alpha.legend),
                       size=2,color="black",fontface="italic",show.legend = FALSE)
  return(g)
}
discrete_legend <- function(g,legendtext,xvar,
                            lowval,hival,
                            lowcolor,midcolor,hicolor,
                            legendwidth,y.bottom,
                            legend.height) {
   legendlabels <- c(legendtext,
                      paste0(" < ",lowval,"  "),
                      paste0(" ",lowval,"-",hival," "),
                      paste0(" >= ",hival," "))
   legendcolors <- c(NA,lowcolor,midcolor,hicolor)

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
   g <- g +
      ggplot2::geom_rect(data=valsTextFrame,
                         aes(xmin=x1.legend,xmax=x2.legend,fill=legendcolors,
                             alpha=alpha.legend),
                         ymin=y1.legend,ymax=y2.legend,inherit.aes=FALSE,
                         show.legend=FALSE) +
      ggplot2::geom_text(data=valsTextFrame,
                         aes(x=xtext.legend,y=ytext.legend,label=legendlabels,
                             hjust=hjust.legend),size=2,fontface="italic")
  return(g)
}

