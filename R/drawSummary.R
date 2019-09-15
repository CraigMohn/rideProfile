drawSummary <- function(ggp,summary,title){

  ggpreturn <- ggp
  ymax <- ggp[["ymax"]]
  xmax <- ggp[["xmax"]]
  xlast <- ggp[["xlast"]]
  g <- ggp[["g"]]
  heightFactor=ggp[["heightFactor"]]
  headerWidth <- min(xmax,25)
  ycenterSum <- seq(1,4) #rows
  ycenterSum <- ymax - (50/heightFactor)*ycenterSum
  xcenterSum <- c(headerWidth*0.05,headerWidth*0.55)
  xposSum <- c(rep(xcenterSum[1],4),rep(xcenterSum[2],4))
  yposSum <- c(ycenterSum,ycenterSum)
  summarylabels <-
    c(paste0(round(xlast,digits=2)," miles"),
      paste0(round(3.28084*summary$ascent[1],digits=0)," ft climbing"),
      paste0(round(1/60*summary$rolling.time[1],digits=0)," minutes rolling"),
      paste0(round(1/60*summary$total.time[1],digits=0)," minutes total"),
      paste0(round(2.23694*summary$speed.rolling.m.s[1],digits=2)," mph avg"),
      paste0(round(summary$avgcadence.withzeros[1],digits=1),
             " cad avg(incl/zeros)"),
      paste0(round(summary$session.total.calories[1],digits=0)," kC burned"),
      paste0(round(summary$avgpower.nozeros[1],digits=0)," Watts(excl/zeros)"))
  summaryTextFrame <- data.frame(xposSum,yposSum,summarylabels)
  if ((summary$pct.trkpts.cad < .95)|(is.na(summary$avgpower.nozeros)))
    summaryTextFrame <- summaryTextFrame[-8,]
  if ((summary$pct.trkpts.hr < .95)|(is.na(summary$session.total.calories)))
    summaryTextFrame <- summaryTextFrame[-7,]
  if (summary$pct.trkpts.cad < .7)
    summaryTextFrame <- summaryTextFrame[-6,]
  g <- g +
    ggplot2::ggtitle(paste0(title,"  ",summary$start.time[1])) +
    ggplot2::geom_text(data=summaryTextFrame,
                       aes(x=xposSum,y=yposSum,label=summarylabels),
                       size=3,hjust=0,alpha=1,color="midnightblue") +
    ggplot2::theme(plot.title=element_text(color="Black",face="bold",
                                           size=13,hjust=0.5))
  ggpreturn[["g"]] <- g
  return(ggpreturn)

}

