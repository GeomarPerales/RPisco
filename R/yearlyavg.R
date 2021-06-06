#' function for processing of pisco daily/monthly values to a yearly average.
#'
#' function for processing of pisco daily/monthly values to a yearly average, apply to precipitation,
#' temperature and evapotranspiration.
#' @param x a dataframe with PISCO daily/monthly values.
#' @param param OPTIONAL, default parameter is sum for precipitation and evapotranspiration,
#' use mean for temperature.
#'
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @name yearlyavg

yearlyavg <-function(x, ...) UseMethod("yearlyavg")

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

  annual = strftime(x$date, "%Y")
  annual.sum = aggregate( as.numeric(as.vector(x$values)), by = list(annual), FUN = opt)
  colnames(annual.sum) <- c("date","values")
  annual.mean = mean(annual.sum$values)
  return(annual.mean)
}

#' @rdname yearlyavg
