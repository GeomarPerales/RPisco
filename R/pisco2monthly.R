#' function for processing of pisco daily values to a monthly serie.
#'
#' function for processing of pisco daily values to a monthly serie, apply to precipitation and
#' temperature.
#' @param x a dataframe with PISCO daily values.
#' @param variable OPTIONAL, default value is sum for precipitation, use mean for temperature.
#' @export
#' @name pisco2monthly

pisco2monthly <-function(x, ...) UseMethod("pisco2monthly")

pisco2monthly <- function(x, variable = NULL){

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

  date = strftime(x$date, "%Y-%m")
  values.sum = aggregate( as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
  colnames(values.sum) <- c("date","values")
  return(values.sum)
}

#' @rdname pisco2monthly
