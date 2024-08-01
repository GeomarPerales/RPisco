#' Function for processing PISCO daily/monthly values into an annual series
#'
#' Function for processing PISCO daily/monthly values into an annual series, apply to precipitation,
#' temperature and evapotranspiration.
#' @param x a dataframe with PISCO daily/monthly values.
#' @param param OPTIONAL, default parameter is sum for precipitation and evapotranspiration,
#' use mean for temperature.
#'
#' @export
#'
#'library(RPisco)
#'
#' pisco.evp <- "D:/0-BD-Pisco/0-Evp"
#' latitude <- -76.11
#' longitude <- -13.11
#' x <-  data.frame(pisco.evp, latitude, longitude)
#' data.diaria.eo <- piscod.eo(x)
#' pisco2annual(data.diaria.eo)
#'
#' @author Geomar Perales Apaico
#'
#' @name pisco2annual

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
