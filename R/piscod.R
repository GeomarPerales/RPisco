#' values extraction of PISCO daily climatic databases from a station
#'
#' function for extract values of PISCO daily climatic databases, PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
#' @param x A dataframe containing the PISCO file name (in netCDF format), longitude and latitude of station.
#' @importFrom raster brick
#' @importFrom raster projection
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @import sp
#' @import raster
#'
#' @export
#'
#' @examples
#'
#'library(RPisco)
#'
#' PISCO.file <- file.path("D:", "PISCOd_pp_2.0.nc")
#' latitude <- -76.11
#' longitude <- -13.11
#' x <-  data.frame(PISCO.file, latitude, longitude)
#' piscod(x)
#'
#' @author Geomar Perales Apaico
#'
#' @name piscod

piscod <- function(x){
  x <- x[,1:3]
  colnames(x) <- c("nc","v1", "v2")
  if(x$v1[1] < x$v2[1]){
    colnames(x) <- c("nc","lon", "lat")
  } else if(x$v1[1] > x$v2[1]){
    colnames(x) <- c("nc","lat", "lon")
  }

  file.nc <- as.character(x$nc)
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

  date <- gsub("X", "", rownames(Pisco.data))
  date <- as.Date(date, format = "%Y.%m.%d")
  Pisco.data <- data.frame(date = date, values = round(Pisco.data, digits = 2))
  rownames(Pisco.data) <- NULL
  return(Pisco.data)

}

#' @rdname piscod
