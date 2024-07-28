#' Function for processing PISCO daily values into a monthly series
#'
#' Function for processing PISCO daily values into a monthly series, apply to precipitation,
#' temperature and evapotranspiration.
#' @param x a dataframe with PISCO daily values.
#' @param param OPTIONAL, default parameter is sum for precipitation and evapotranspiration,
#' use mean for temperature.
#' @param obj OPTIONAL, default parameter is NULL, use this parameter for obtain data in
#'column or matrix. Use col for obtain data column or matrix for obtain data matrix.
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @name pisco2monthly

pisco2monthly <-function(x, ...) UseMethod("pisco2monthly")

pisco2monthly <- function(x, param = NULL, obj = NULL){

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
    stop("param parameter not recognized")
  }

  date = strftime(x$date, "%Y-%m")
  values.sum = aggregate( as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
  if(is.null(obj)){
    colnames(values.sum) <- c("date","values")
    return(values.sum)

  } else if(obj == "col"){
    colnames(values.sum) <- c("date","values")
    return(values.sum)

  } else if(obj == "matrix"){
    values.sum <- t(matrix(values.sum[,2], 12, 36))
    values.sum <- data.frame(values.sum)
    colnames(values.sum) <- month.abb
    return(values.sum)

  } else if(is.na(match(obj, c("col", "matrix")))){
    stop("obj parameter not recognized")
  } else {
    stop("obj parameter not recognized")
  }

}

#' @rdname pisco2monthly
