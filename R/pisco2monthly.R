#' Function for processing PISCO daily values into a monthly series
#'
#' Function for processing PISCO daily values into a monthly series, apply to precipitation,
#' temperature and evapotranspiration.
#' @param x a dataframe with PISCO daily values.
#' @param param OPTIONAL, default parameter is sum for precipitation and evapotranspiration,
#' use mean for temperature.
#'
#' @export
#'
#' library(RPisco)
#'
#' pisco.ppd <- "D:/PISCOd_pp_2.0.nc"
#' latitude <- -76.11
#' longitude <- -13.11
#' x <-  data.frame(pisco.ppd, latitude, longitude)
#' data.diaria.pp <- piscod(x)
#' pisco2monthly(data.diaria.pp)
#'
#' @author Geomar Perales Apaico
#'
#' @name pisco2monthly

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
    stop("param parameter not recognized")
  }

  date <- strftime(x$date, "%Y-%m")
  values.sum <- aggregate( as.numeric(as.vector(x$values)), by = list(date), FUN = opt)
  colnames(values.sum) <- c("date", "values")
  values.sum$date <- as.Date(paste0(values.sum$date, "-01"))
  return(values.sum)

}

#' @rdname pisco2monthly
