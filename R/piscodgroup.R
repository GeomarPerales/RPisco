#' values extraction of PISCO daily climatic databases from a stations group.
#'
#' function for extract values of PISCO daily climatic databases from a stations group, PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
#' @param x A dataframe containing the PISCO file name (in netCDF format), longitude and latitude from stations group.
#' @importFrom raster brick
#' @importFrom raster projection
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @import sp
#' @import raster
#'
#' @export
#'
#'@examples
#'
#'library(RPisco)
#'
#' PISCO.file <- file.path("D:", "PISCOd_pp_2.0.nc")
#' station <- c("Ayacucho", "Coracora")
#' latitude <- c(-74.23 -73.78)
#' longitude <- c(-13.18, -15.01)
#' x <-  data.frame(PISCO.file, station, latitude, longitude)
#' piscodgroup(x)
#'
#' @author Geomar Perales Apaico
#'
#' @name piscodgroup

piscodgroup <- function(x){
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

  date <- seq(from = as.Date("1981-01-01"), by = "days", length.out = nrow(Pisco.data))
  Pisco.data <- data.frame(date = date, values = round(Pisco.data, digits = 2))
  colnames(Pisco.data) <- c("date", name)
  rownames(Pisco.data) <- NULL
  return(Pisco.data)

}

#' @rdname piscodgroup
