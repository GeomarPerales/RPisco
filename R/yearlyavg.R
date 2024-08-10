#' Function for processing PISCO daily/monthly values into a yearly values
#'
#' Function for processing PISCO daily/monthly values into a yearly values, apply to precipitation,
#' temperature and evapotranspiration.
#' @param x a dataframe with PISCO daily/monthly values.
#' @param param OPTIONAL, default parameter is sum for precipitation and evapotranspiration,
#' use mean for temperature.
#'
#' @export
#'
#' @examples
#'
#' library(RPisco)
#'
#' pisco.ppd <- "D:/PISCOd_pp_2.0.nc"
#' latitude <- -74.23
#' longitude <- -13.18
#' x <-  data.frame(pisco.ppd, latitude, longitude)
#' data.diaria.pp <- piscod(x)
#' monthlyavg(data.diaria.pp)
#'
#' @author Geomar Perales Apaico
#'
#' @name yearlyavg

yearlyavg <- function(x, param = NULL){

  colnames(x) <- c("date", "values")

  if(is.null(x)){
    stop("values not recognized")
  }

  if(is.null(param)){
    opt = sum

  } else if(param == "sum"){
    opt = sum

  } else if(param == "mean"){
    opt = mean
  } else {
    stop("parameter not recognized")
  }

  annual <- strftime(x$date, "%Y")
  annual.sum <- aggregate( as.numeric(as.vector(x$values)), by = list(annual), FUN = opt)
  colnames(annual.sum) <- c("date","values")
  annual.mean <- mean(annual.sum$values)
  return(annual.mean)
}

#' @rdname yearlyavg
