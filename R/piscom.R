#' values extraction of PISCO monthly data
#'
#' function for extract values of PISCO monthly data, PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
#' @param x a dataframe with PISCO file name (in netCDF format), longitude and latitude of station.
#' @param start OPTIONAL, start date of dataset, default value is 1981-01-01.
#' @param end OPTIONAL, end date of dataset, default value is 2016-12-01.
#' @importFrom raster brick
#' @importFrom raster projection
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @import sp
#' @import raster
#' @export
#' @name piscom

piscom <-function(x, ...) UseMethod("piscom")

piscom <- function(x, start = NULL, end = NULL){
  colnames(x) <- c("nc", "lon", "lat")
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
  colnames(Pisco.data) <- c("date", "values")

  if(is.null(start) & is.null(end)){
    return(Pisco.data)
  } else if(!is.null(start)  & !is.null(end)){
    if( sapply( start, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))) == TRUE &
        sapply( start, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))) == TRUE){
      start.out <- start
      end.out <- end
      Pisco.data <- Pisco.data[(Pisco.data$Months >= start.out & Pisco.data$Months <= end.out),]
      return(Pisco.data)
    } else {
      stop("date format not recognized, date format is %Y-%m-%d ")
    }

  } else if(!is.null(start) & sapply( start,
                                      function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))) == TRUE){
    start.out <- start
    end.out <- "2016-12-01"
    Pisco.data <- Pisco.data[(Pisco.data$Months >= start.out & Pisco.data$Months <= end.out),]
    return(Pisco.data)
  } else {
    stop("date format not recognized, date format is %Y-%m-%d ")
  }
}
#' @rdname piscom
