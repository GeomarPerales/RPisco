#' function for processing of pisco daily/monthly values to a yearly average.
#'
#' function for processing of pisco daily/monthly values to a yearly average, apply to precipitation and
#' temperature.
#' @param x a dataframe with PISCO daily/monthly values.
#' @param variable OPTIONAL, default value is sum for precipitation, use mean for temperature.
#' @export
#' @name yearlyavg

yearlyavg <-function(x, ...) UseMethod("yearlyavg")

yearlyavg <- function(x, variable = NULL){

  colnames(x) <- c("date", "values")

  if(is.null(x)){
    stop("values not recognized")
  }

  if(is.null(variable)){
    opt = sum

  } else if(variable == "precipitation"){
    opt = sum

  } else if(variable == "temperature"){
    opt = mean
  } else {
    stop("variable not recognized")
  }

  annual = strftime(x$date, "%Y")
  annual.sum = aggregate( as.numeric(as.vector(x$values)), by = list(annual), FUN = opt)
  colnames(annual.sum) <- c("date","values")
  annual.mean = mean(annual.sum$values)
  return(annual.mean)
}

#' @rdname yearlyavg
