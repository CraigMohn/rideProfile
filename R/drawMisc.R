#' create functions to map variables into colors based on parameters
#'
#' \code{setupColorBounds} creates lists of functions for color bars and legends
#'
#' @param palette viridis palette name, see main routine
#' @param legendwidth width of a legend chunk in distance units
#'
#' @param speedLow speeds below this are same color
#' @param speedHigh speeds above this are same color
#' @param speedColorLow set color for speedLow and slower
#' @param speedColorHigh set color for speedHigh and faster
#'
#' @param gradeLow grades below this are same color
#' @param gradeHigh grades above this are same color
#' @param gradeColorLow set color for gradeLow and steeper downhill
#' @param gradeColorHigh set color for gradeHigh and steeper uphill
#'
#' @param hrLow heartrates below this are same color
#' @param hrHigh heartrates above this are same color
#' @param hrColorLow set color for hrLow and lower
#' @param hrColorHigh set color for hrHigh and higher
#'    colors are from same palette as speeds, number is the speed corresponding
#'    to the desired limit on the range of heartrates
#'
#' @param cadCont display cadence as a continuos color map
#' @param cadTarget target cadence range minimum
#' @param cadLow lower cadence limit for continuous color, all
#'    lower cadences are displayed as same color
#' @param cadHigh upper cadence limit for continuous color, all
#'    higher cadences are displayed as same color
#' @param cadColorLow set color for cadence at cadLow or below
#' @param cadColorMid set color for cadence above low but below target
#' @param cadColorHigh set color for cadence above target
#'
#' @param powerLow power outputs below this are same color
#' @param powerHigh power outputs above this are same color
#' @param powerColorLow set color for powerLow and lower
#' @param powerColorHigh set color for powerHigh and higher
#'
#' @param imperial use mi and ft instead of km and m
#'
#' @param ... stuff for other functions
#'
#' @return a named list of named lists of functions
#'
#' @export
setupColorBounds <- function(palette,
                             legendwidth,
                             speedLow=3,speedHigh=40,
                             speedColorLow=0,speedColorHigh=40,
                             gradeLow=-0.15,gradeHigh=0.15,
                             gradeColorLow=40,gradeColorHigh=0,
                             hrLow=100,hrHigh=170,
                             hrColorLow=11,hrColorHigh=26,
                             cadCont=TRUE,cadTarget=88,
                             cadLow=50,cadHigh=120,
                             cadColorLow=4,cadColorMid=10,cadColorHigh=15,
                             powerLow=75,powerHigh=400,
                             powerColorLow=9,powerColorHigh=21,
                             imperial=TRUE,
                             ...) {

  if (imperial) {
    speedLegendText <- "Speed (mph)"
  }
  else {
    speedLegendText <- "Speed (kph)"
  }
  ##  create functions for color data bands and legends
  colorize <-
    list("speed"=colorIndexC(speedLow,speedHigh,speedColorLow,speedColorHigh),
         "grade"=colorIndexC(gradeLow,gradeHigh,gradeColorLow,gradeColorHigh),
         "hr"=colorIndexC(hrLow,hrHigh,hrColorLow,hrColorHigh),
         "cad"=colorIndexC(cadLow,cadHigh,cadColorLow,cadColorHigh),
         "power"=colorIndexC(powerLow,powerHigh,powerColorLow,powerColorHigh),
         "na"=colorIndexNA() )
  if (!cadCont) colorize[["cad"]] <-
      colorIndexD(cadLow,cadTarget,cadColorLow,cadColorMid,cadColorHigh)

  colorLegends <-
    list("speed"=cont_legend(speedLegendText,
                             speedLow,speedHigh,speedColorLow,speedColorHigh,
                             legendwidth),
         "grade"=cont_legend("Grade",
                             gradeLow,gradeHigh,gradeColorLow,gradeColorHigh,
                             legendwidth),
         "hr"=cont_legend("Heart Rate (bpm)",
                          hrLow,hrHigh,hrColorLow,hrColorHigh,
                          legendwidth),
         "cad"=cont_legend("Cadence (rpm)",
                           cadLow,cadHigh,cadColorLow,cadColorHigh,
                           legendwidth),
         "power"=cont_legend("Power (watts)",
                             powerLow,powerHigh,powerColorLow,powerColorHigh,
                             legendwidth)  )
  if (!cadCont) colorLegends[["cad"]] <-
    disc_legend("Cadence (rpm)",
                cadLow,cadTarget,cadColorLow,cadColorMid,cadColorHigh,
                legendwidth)

  return(list(colorize=colorize,colorLegends=colorLegends))
}

colorIndexC <- function(valueLow,valueHigh,indexLow,indexHigh) {
  function(valueVec) {

    #  return a vector of values which are a linear mapping from the interior
    #  of the low and high values to the corresponding point in the interval between
    #  the low and high index values, with exterior points mapped to the endpoints

    valueIndex <- indexLow +
      (indexHigh - indexLow) * (valueVec - valueLow) / (valueHigh - valueLow)
    valueIndex[ !is.na(valueVec) & (valueVec < valueLow) ] <- indexLow
    valueIndex[ !is.na(valueVec) & (valueVec>valueHigh) ] <- indexHigh
    valueIndex[valueIndex==0] <- NA

    valueIndex
  }
}
colorIndexD <- function(valueLow,valueHigh,indexLow,indexMid,indexHigh) {
  function(valueVec) {

    #  return a vector of values which are a linear mapping from the interior
    #  of the low and high values to the corresponding point in the interval between
    #  the low and high index values, with exterior points mapped to the endpoints

    valueIndex <- rep(indexMid,length(valueVec))
    valueIndex[ !is.na(valueVec) & (valueVec < valueLow) ] <- indexLow
    valueIndex[ !is.na(valueVec) & (valueVec > valueHigh) ] <- indexHigh
    valueIndex[is.na(valueVec)] <- NA
    valueIndex[valueIndex==0] <- NA

    valueIndex
  }
}
colorIndexNA <- function() {
  function(valueVec) {
    rep(NA,length(valueVec))
  }
}

#' create smoothed data series
#'
#' \code{smoothSeries} creates lists of smoothed data series
#'
#' @param track data frame or tibble containing track details including
#'    location and elevation data, as well as heartrate and cadence
#' @param hrSmoothBW bandwidth (in seconds) for smoothing kernel
#'    for heartrate data
#' @param hrSmoothNN number of points (on each side) to use in smoothing
#'    kernel for heartrate data
#' @param gradeSmoothBW bandwidth (in seconds) for smoothing kernel
#'    for grade data
#' @param gradeSmoothNN number of points (on each side) to use in
#'    smoothing kernel for grade data
#' @param cadSmoothBW bandwidth (in seconds) for smoothing kernel
#'    for cadence data
#' @param cadSmoothNN number of points (on each side) to use in
#'    smoothing kernel for cadence data
#' @param powerSmoothBW bandwidth (in seconds) for smoothing kernel
#'    for power data
#' @param powerSmoothNN number of points (on each side) to use in smoothing
#'    for power data
#' @param elevSmoothBWMeters bandwidth (in meters) for elevation smoothing
#' @param imperial use mi and ft instead of km and m
#' @param ... arguments for other functions including color bounds

smoothSeries <- function(track,
                         hrSmoothBW=6,hrSmoothNN=6,
                         cadSmoothBW=10,cadSmoothNN=10,
                         gradeSmoothBW=13,gradeSmoothNN=13,
                         powerSmoothBW=10,powerSmoothNN=10,
                         elevSmoothBWMeters=15,
                         imperial=TRUE,
                         ...) {

  if (imperial) {
    distance  <- milesFromMeters(track$distance.m)
    elevsmbw <- milesFromMeters(elevSmoothBWMeters)
  } else {
    distance <- kmFromMeters(track$distance.m)
    elevsmbw <- kmFromMeters(elevSmoothBWMeters)
  }
  walltime <- as.numeric(difftime(track$timestamp.s,track$timestamp.s[1],
                                  units="secs"))
  #  note there may be multiple records at same distance.  smoothing
  #    algorithm will weight equally.
  elevsm <- smoothData(yvec=track$altitude.m,xvar=distance,
                       bw=elevsmbw,nneighbors=18,kernel="epanechnikov",
                       replaceNAs=TRUE)

  speedsm <- track$speed.m.s
  speedsm[speedsm==0] <- NA  #  don't display or average stops
  speedsm <- smoothDataSegments(yvec=track$speed.m.s,xvar=walltime,
                                segment=track$segment,
                                bw=4,nneighbors=4,
                                kernel="epanechnikov",
                                replaceNAs=TRUE)
  speedsm[is.na(speedsm)] <- 0

  trackdeltadistance <- track$distance.m - lag_one(track$distance.m)
  trackgrade <- (elevsm - lag_one(elevsm)) / trackdeltadistance
  trackgrade[trackdeltadistance<0.5] <- NA
  gradesm <- smoothData(yvec=trackgrade,xvar=walltime,
                        bw=gradeSmoothBW,nneighbors=gradeSmoothNN,
                        kernel="epanechnikov",
                        replaceNAs=TRUE)

  cadzero <- track$cadence.rpm == 0
  cadencetemp <- track$cadence.rpm
  cadencetemp[cadencetemp==0] <- NA
  cadencesm <- smoothDataSegments(yvec=cadencetemp,xvar=walltime,
                                  segment=track$segment,
                                  bw=cadSmoothBW,nneighbors=cadSmoothNN,
                                  kernel="triangular",
                                  replaceNAs=FALSE)
  cadencesm[cadzero] <- 0.0

  hrsm <- smoothDataSegments(yvec=track$heart_rate.bpm,xvar=walltime,
                               segment=track$segment,
                               bw=hrSmoothBW,nneighbors=hrSmoothNN,
                               kernel="epanechnikov",
                               replaceNAs=FALSE)

  powertemp <- track$power.watts
  powertemp[powertemp==0] <- NA
  powersm <- smoothDataSegments(yvec=powertemp,xvar=walltime,
                                segment=track$segment,
                                bw=powerSmoothBW,nneighbors=powerSmoothNN,
                                kernel="epanechnikov",
                                replaceNAs=FALSE)
  if (imperial) {
    elevsm <- feetFromMeters(elevsm)
    speedsm <- milesFromMeters(speedsm)*3600
  }
  else {
    speedsm <- kmFromMeters(speedsm)*3600
  }
  smoothvar <- list("elev"=elevsm,"speed"=speedsm,"grade"=gradesm,
                    "hr"=hrsm,"cad"=cadencesm,"power"=powersm)

  return(smoothvar)
}
