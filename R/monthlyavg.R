#' function for processing of pisco daily values to a monthly average.
#'
#' function for processing of pisco daily values to a monthly average, apply to precipitation,
#' temperature and evapotranspiration.
#' @param x a dataframe with PISCO daily values.
#' @param param OPTIONAL, default parameter is sum for precipitation and evapotranspiration,
#' use mean for temperature.
#'
#' @author Geomar Perales Apaico
#'
#' @export
#' @name monthlyavg

monthlyavg <-function(x,...) UseMethod("monthlyavg")

monthlyavg <- function(x,param = NULL){

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
  values.sum = aggregate(as.numeric(as.vector(x$values)),by = list(date),FUN = opt)
  colnames(values.sum) <- c("date","values")

  min.yr <- as.numeric(substr(min(x$date),1,4))
  max.yr <- as.numeric(substr(max(x$date),1,4))
  yrs.numbs <- max.yr - min.yr + 1

  data.matrix <- t(matrix(values.sum$values, 12, yrs.numbs))
  data.matrix <- data.frame(data.matrix)
  data.vector <- rep(NA,12)
  for (i in 1:12) {
    data.vector[i] <- mean(data.matrix[,i])
  }
  data.vector <- data.frame(matrix(data.vector, 1, 12))
  colnames(data.vector) <- month.abb
  return(data.vector)
}

#' @rdname monthlyavg
