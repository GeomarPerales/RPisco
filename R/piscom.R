#' values extraction of PISCO monthly climatic databases from a station
#'
#' function for extract values of PISCO monthly climatic databases, PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
#'
#' To use RPisco, Download PISCO climatic databases from IRI/LDEO Climate Data Library.
#' visit: \href{https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/}{IRI/LDEO-SENAMHI-PISCO}
#'
#' @param x A dataframe containing the PISCO file name (in netCDF format), longitude, and latitude of the station.
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
#' library(RPisco)
#'
#' PISCO.file <- file.path("D:", "PISCOm_pp_2.0.nc")
#' latitude <- -74.23
#' longitude <- -13.18
#' x <-  data.frame(PISCO.file, latitude, longitude)
#' piscom(x)
#'
#' @author Geomar Perales Apaico
#'
#' @name piscom

piscom <- function(x){
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

  date <- seq(from = as.Date("1981-01-01"), by = "month", length.out = nrow(Pisco.data))
  Pisco.data <- data.frame(date = date, values = round(Pisco.data, digits = 2))
  rownames(Pisco.data) <- NULL
  return(Pisco.data)

}
#' @rdname piscom
