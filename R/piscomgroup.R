#' values extraction of PISCO monthly climatic databases from a stations group
#'
#' function for extract values of PISCO monthly climatic databases from a stations group, PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
#' @param x a dataframe with PISCO file name (netCDF format), longitude and latitude from a stations group.
#' @param start OPTIONAL, start date of dataset, default value is 1981-01-01.
#' @param end OPTIONAL, end date of dataset, default value is 2016-12-01.
#' @importFrom raster brick
#' @importFrom raster projection
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @import sp
#' @import raster
#'
#' @export
#'
#' PISCO.file <- file.path("D:", "PISCOm_pp_2.0.nc")
#' station <- "Ayacucho"
#' latitude <- -76.11
#' longitude <- -13.11
#' x <-  data.frame(station, latitude, longitude)
#' piscomgroup(PISCO.file, x)
#'
#' @author Geomar Perales Apaico
#'
#' @name piscomgroup

piscomgroup <- function(x, start = NULL, end = NULL){
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

  Pisco.data <- cbind( study.range, format(Pisco.data, scientific = F, digits = 2))

  row.names(Pisco.data) <- seq(1, nrow(Pisco.data), 1)
  colnames(Pisco.data) <- c("date", as.vector(name))

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
#' @rdname piscomgroup
