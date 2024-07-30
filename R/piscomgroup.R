#' values extraction of PISCO monthly climatic databases from a stations group
#'
#' function for extract values of PISCO monthly climatic databases from a stations group, PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
#' @param x A dataframe containing the PISCO file name (in netCDF format), longitude and latitude from a stations group.
#' @param type OPTIONAL, default is "stable". Change to "unstable" to modify the study range.
#' @importFrom raster brick
#' @importFrom raster projection
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @import sp
#' @import raster
#' @import openxlsx
#'
#' @export
#'
#' @examples
#'
#' library(RPisco)
#'
#' PISCO.file <- file.path("D:", "PISCOm_pp_2.0.nc")
#' station <- c("Ayacucho", "Coracora")
#' latitude <- c(-76.11, -73.78)
#' longitude <- c(-13.11, -15.01)
#' x <-  data.frame(PISCO.file, station, latitude, longitude)
#' piscomgroup(x)
#'
#' @author Geomar Perales Apaico
#'
#' @name piscomgroup

piscomgroup <- function(x, type = "stable"){
  x <- x[,1:4]
  colnames(x) <- c("nc", "name","v1", "v2")
  if(x$v1[1] < x$v2[1]){
    colnames(x) <- c("nc", "name","lon", "lat")
  } else if(x$v1[1] > x$v2[1]){
    colnames(x) <- c("nc", "name","lat", "lon")
  }

  file.nc <- unique(as.character(x$nc))
  name <- as.character(x$name)
  longitude <- as.numeric(x$lon)
  latitude <- as.numeric(x$lat)

  if(is.numeric(longitude) & is.numeric(latitude)){
    coord <- data.frame(x = as.numeric(longitude), y = as.numeric(latitude))
  }  else {
    stop("coordinates not defined")
  }
  coord <- coord
  variable.raster <- raster::brick(file.nc)
  sp::coordinates(coord) <- ~ x + y
  raster::projection(coord) <- raster::projection(variable.raster)
  points <- raster::extract(variable.raster[[1]], coord, cellnumbers = T)[,1]
  Pisco.data <- t(variable.raster[points])



  study.range <- data.frame( Date = seq( from = as.Date( "1981-01-01"), to = as.Date( "2016-12-01"), by = "months"))

  Pisco.data <- cbind( study.range, round(Pisco.data, digits = 2))

  row.names(Pisco.data) <- seq(1, nrow(Pisco.data), 1)
  colnames(Pisco.data) <- c("date", as.vector(name))
  return(Pisco.data)

}
#' @rdname piscomgroup
