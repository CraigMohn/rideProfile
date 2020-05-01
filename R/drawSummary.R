drawSummary <- function(ggp,summary,title){

  ggpreturn <- ggp
  ymax <- ggp[["ymax"]]
  xmax <- ggp[["xmax"]]
  xlast <- ggp[["xlast"]]
  g <- ggp[["g"]]
  heightFactor=ggp[["heightFactor"]]
  headerWidth <- min(xmax,25)
  tablerows <- 5
  ycenterSum <- seq(1,tablerows)
  ycenterSum <- ymax - (36/heightFactor)*ycenterSum
  xcenterSum <- c(headerWidth*0.05,headerWidth*0.55)
  xposSum <- c(rep(xcenterSum[1],tablerows),rep(xcenterSum[2],tablerows))
  yposSum <- c(ycenterSum,ycenterSum)
  if (is.na(summary$avgcadence.withzeros.session))  {
    cadprint <-summary$avgcadence.withzeros[1]
  } else {
    cadprint <- summary$avgcadence.withzeros.session[1]
  }
  summarylabels <-
    c(paste0(round(xlast,digits=2)," miles"),
      paste0(round(3.28084*summary$ascent[1],digits=0)," ft climbing"),
      paste0(round(2.23694*summary$speed.rolling.m.s[1],digits=2)," mph avg"),
      paste0(round(summary$session.total.calories[1],digits=0)," kC burned"),
      paste0(" "),
      paste0(h_m_s(summary$rolling.time[1])," rolling time"),
      paste0(h_m_s(summary$total.time[1])," total time"),
      paste0(round(cadprint,digits=1),
             " cad avg (incl/zeros)"),
      paste0(round(summary$avgpower.nozeros[1],digits=0)," Watts (excl/zeros)"),
      paste0(round(100*(1-summary$session.left.right.balance[1]),digits=1),"% / ",
             round(100*summary$session.left.right.balance[1],digits=1),"%  L/R power split")
    )
  summaryTextFrame <- data.frame(xposSum,yposSum,summarylabels)
  #  blank out fields that are empty
  if ((summary$pct.trkpts.cad[1] < .5)|(is.na(summary$session.left.right.balance[1])))
    summaryTextFrame$summarylabels[10] <- " "
  if ((summary$pct.trkpts.cad[1] < .5)|(is.na(summary$avgpower.nozeros[1])))
    summaryTextFrame$summarylabels[9] <- " "
  if (summary$pct.trkpts.cad[1] < .5)
    summaryTextFrame$summarylabels[8] <- " "
  if (((summary$pct.trkpts.hr[1] < .95) & is.na(summary$avgpower.nozeros[1])) |
      (is.na(summary$session.total.calories[1])))
    summaryTextFrame$summarylabels[4] <- " "

  g <- g +
    ggplot2::ggtitle(paste0(title,"  ",summary$start.time[1])) +
    ggplot2::geom_text(data=summaryTextFrame,
                       aes(x=xposSum,y=yposSum,label=summarylabels),
                       size=2.3,hjust=0,alpha=1,color="midnightblue") +
    ggplot2::theme(plot.title=element_text(color="Blue2",face="bold.italic",
                                           size=13,hjust=0.5))
  ggpreturn[["g"]] <- g
  return(ggpreturn)

}
h_m_s <- function(seconds) {
  h <- floor(seconds/3600)
  m <- floor((seconds-3600*h)/60)
  s <- round((seconds-3600*h-60*m))
  if (h > 0) {
    return(paste0(h,":",sprintf("%02d",m),":",sprintf("%02d",s)," hrs"))
  } else {
    return(paste0(sprintf("%02d",m),":",sprintf("%02d",s)," mins"))
  }
}

