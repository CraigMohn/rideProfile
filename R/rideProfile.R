#' rideProfile - plot heartrate, cadence, speed and more using color-driven
#'   data summaries to convey performance patterns
#'
#' @section main call :
#'   \link{rideProfile}
#'
#' @import magrittr tibble dplyr ggplot2
#' @import viridis
#' @importFrom grDevices colorRampPalette dev.off heat.colors
#'                       jpeg rainbow tiff
#' @importFrom graphics lines par plot points title
#' @importFrom stats approx median quantile setNames weighted.mean
#' @importFrom raster pointDistance 
#'
#' @name rideProfile
NULL

###  make the R checker happy
tedious <- utils::globalVariables(c("alphachar","alphahour","distlegend",
                                    "prtchar","prthour","start.hour","start.time",
                                    "startbutton.date","stoplabels","timestamp.s",
                                    "distance.m","segment","timestamp",
                                    "x","xtext.stop","y","xend","xcol",
                                    "verticalMultiplier","color","hjust","label",
                                    "yend","group","timeBeg","timeEnd","pauseSize",
                                    "timelaststop","segbegtime","segendtime",
                                    "movingrun","maxdist","startofstop","sosNA",
                                    "joinseg","subsegment"))


