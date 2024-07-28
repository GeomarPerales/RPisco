#' values extraction of PISCO monthly climatic databases from a station
#'
#' function for extract values of PISCO monthly climatic databases, PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
#' @param x a dataframe with PISCO file name (in netCDF format), longitude and latitude of station.
#' @param obj OPTIONAL, default parameter is NULL, use this parameter for obtain data in
#'column or matrix. Use col for obtain data column or matrix for obtain data matrix.
#' @importFrom raster brick
#' @importFrom raster projection
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @import sp
#' @import raster
#' @export
#'
#' @examples
#'
#' PISCO.file <- file.path("D:", "PISCOm_pp_2.0.nc")
#' latitude <- -76.11
#' longitude <- -13.11
#' x <-  data.frame(PISCO.file, latitude, longitude)
#' piscom(x)
#'
#' @author Geomar Perales Apaico
#'
#' @name piscom

piscom <- function(x, obj = NULL){
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

  study.range <- data.frame( Date = seq( from = as.Date( "1981-01-01"), to = as.Date( "2016-12-01"), by = "months"))
  Pisco.data <- cbind( study.range, format(as.vector(Pisco.data), scientific = F, digits = 2))
  row.names(Pisco.data) <- seq(1, nrow(Pisco.data), 1)


  if(is.null(obj)){
    colnames(Pisco.data) <- c("date", "values")
    return(Pisco.data)

  } else if(obj == "col"){
    colnames(Pisco.data) <- c("date", "values")
    return(Pisco.data)

  } else if(obj == "matrix"){
    Pisco.data <- t(matrix(Pisco.data[,2], 12, 36))
    Pisco.data <- data.frame(Pisco.data)
    colnames(Pisco.data) <- month.abb
    return(Pisco.data)

  } else if(is.na(match(obj, c("col", "matrix")))){
    stop("obj parameter not recognized")

  } else {
    stop("obj parameter not recognized")

  }
}
#' @rdname piscom
