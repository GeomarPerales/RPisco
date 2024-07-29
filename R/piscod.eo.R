#' values extraction of PISCO evapotranspiration daily climatic databases
#'
#' function for extract values of PISCO evapotranspiration daily climatic databases,
#' PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
#' @param x a dataframe with PISCO files folder (netCDF format) of evapotranspiration, longitude and latitude of station.
#' @importFrom raster brick
#' @importFrom raster projection
#' @importFrom raster extract
#' @importFrom raster readAll
#' @importFrom sp coordinates
#' @import sp
#' @import raster
#'
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @name piscod.eo

piscod.eo <- function(x){
  x <- x[,1:3]
  colnames(x) <- c("nc","v1", "v2")
  if(x$v1[1] < x$v2[1]){
    colnames(x) <- c("nc","lon", "lat")
  } else if(x$v1[1] > x$v2[1]){
    colnames(x) <- c("nc","lat", "lon")
  }

  longitude <- as.numeric(x$lon)
  latitude <- as.numeric(x$lat)
  if(is.numeric(longitude) & is.numeric(latitude)){
    coord <- data.frame(x = as.numeric(longitude), y = as.numeric(latitude))
  }  else {
    stop("coordinates not defined")
  }

  dir.nc <- as.character(x$nc)
  files.ncdf <- list.files(path = dir.nc, pattern = ".nc")

  sp::coordinates(coord) <- ~ x + y
  Pisco.data <- list()
  for (i in 1:length(files.ncdf)) {
    file.nc <- as.character(paste0(dir.nc, "\\", files.ncdf[i]))
    variable.raster <- raster::brick(file.nc)
    raster::projection(coord) <- raster::projection(variable.raster)
    points <- raster::extract(readAll(variable.raster[[1]]), coord, cellnumbers = T)[,1]
    Pisco.data[[i]] <- t(variable.raster[points])
  }
  Pisco.df <- do.call( rbind, Pisco.data)
  date <- gsub("X", "", rownames(Pisco.df))
  date <- as.Date(date, format = "%d.%m.%Y")
  Pisco.df <- data.frame(date = date, values = Pisco.df)
  return(Pisco.df)
}

#' @rdname piscod.eo
