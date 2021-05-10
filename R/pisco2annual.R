#' function for processing of pisco daily/monthly values to a annual serie.
#'
#' function for processing of pisco daily/monthly values to a annual serie, apply to precipitation,
#' temperature and evapotranspiration.
#' @param x a dataframe with PISCO daily/monthly values.
#' @param param OPTIONAL, default parameter is sum for precipitation and evapotranspiration,
#' use mean for temperature.
#' @export
#' @name pisco2annual

pisco2annual <-function(x, ...) UseMethod("pisco2annual")

pisco2annual <- function(x, param = NULL){

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
  return(annual.sum)
}

#' @rdname pisco2annual
