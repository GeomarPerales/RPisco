#' function for search SENAMHI stations information by coordinates and maximum distance
#'
#' function for search SENAMHI stations information by coordinates (latitude and longitude) and maximum distance inside SENAMHI data web.
#'
#' @param x numeric; numeric vector of coordinates (latitude and longitude)
#' @param senamhi stations information of SENAMHI web scraping
#' @param dist_max numeric, value of maximum distance from stations to point (kms)
#'
#' @export
#'
#' @examples
#'
#' x <- c(-13.18, -74.23)
#' dist_max <- 50
#' stations <- senamhi()
#'
#' senamhi.find(x, senamhi(), dist_max)
#'
#' @author Geomar Perales Apaico
#'
#' @name senamhi.find

senamhi.find <- function(x, senamhi, dist_max) {

  lat1 <- as.numeric(as.character(senamhi$lat))
  lon1 <- as.numeric(as.character(senamhi$lon))

  if(is.null(x)){
    return(print("localizacion no definida"))

  } else if(length(x) == 2 & is.numeric(x)){
    x <- x

  } else if(length(x) == 1 & is.numeric(x)){
    return(print("localizacion no definida"))

  } else if(length(x) == 1 & !is.numeric(x)){
    return(print("localizacion no definida"))

  } else if(length(x) == 2 & !is.numeric(x)){
    return(print("localizacion no definida"))

  }

  lat2 <- x[1]
  lon2 <- x[2]
  R <- 6371.0  # Radio de la Tierra en kilómetros

  lat1_rad <- lat1*pi/180
  lon1_rad <- lon1*pi/180
  lat2_rad <- lat2*pi/180
  lon2_rad <- lon2*pi/180

  dlat <- lat2_rad - lat1_rad
  dlon <- lon2_rad - lon1_rad

  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  senamhi$dist <- round(distance, 2)

  estaciones_cercanas <- senamhi[senamhi$dist <= dist_max, ]
  return(estaciones_cercanas)
}
