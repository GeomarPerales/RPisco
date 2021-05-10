#' function for processing of pisco daily values to a monthly serie.
#'
#' function for processing of pisco daily values to a monthly serie, apply to precipitation,
#' temperature and evapotranspiration.
#' @param x a dataframe with PISCO daily values.
#' @param param OPTIONAL, default parameter is sum for precipitation and evapotranspiration,
#' use mean for temperature.
#' @export
#' @name pisco2monthly

pisco2monthly <-function(x, ...) UseMethod("pisco2monthly")

pisco2monthly <- function(x, param = NULL){

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

  date = strftime(x$date, "%Y-%m")
  values.sum = aggregate( as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
  colnames(values.sum) <- c("date","values")
  return(values.sum)
}

#' @rdname pisco2monthly
