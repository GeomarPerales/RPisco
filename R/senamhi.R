#' function for get all stations information of SENAMHI web scraping
#'
#' function for get all stations information (coordinates and altitude) of SENAMHI web scraping.
#'
#' @import rvest
#'
#' @export
#'
#' @examples
#'
#' stations <- senamhi()
#
#' @author Geomar Perales Apaico
#'
#' @name senamhi


senamhi <- function() {
  link <- "https://www.senamhi.gob.pe/mapas/mapa-estaciones-2/"
  stn_senamhi <- read_html(link)

  stn_senamhi2 <- strsplit(html_text(stn_senamhi), "nom")
  stn_senamhi2[[1]] <- stn_senamhi2[[1]][-c(1)]

  stn <- list()
  cat <- list()
  lat <- list()
  lon <- list()
  ico <- list()
  cod <- list()
  cod_old <- list()
  estado <- list()
  data_stn <- list()

  for (i in 1:length(stn_senamhi2[[1]])) {
    x <- gsub('\"',"", stn_senamhi2[[1]][i])
    x <- gsub(': ', ":", x)
    x <- gsub(',\n', "", x)
    x <- gsub('\\}\\{', "", x)
    data_estaciones <- strsplit(x, ",")
    stn[[i]] <- gsub(":", "", data_estaciones[[1]][1])
    cat[[i]] <- gsub("cate:", "", data_estaciones[[1]][2])
    lat[[i]] <- gsub("lat:", "", data_estaciones[[1]][3])
    lat[[i]] <- gsub(" ", "", lat[[i]])

    lon[[i]] <- gsub("lon:", "", data_estaciones[[1]][4])
    lon[[i]] <- gsub(" ", "", lon[[i]])

    ico[[i]] <- gsub(" ico:", "", data_estaciones[[1]][5])

    cod[[i]] <- ifelse(substr(data_estaciones[[1]][6], 1, 5) == " cod:",
                       gsub(" cod:", "",data_estaciones[[1]][6]), NA)

    cod_old[[i]] <- ifelse(substr(data_estaciones[[1]][7], 1, 8) == "cod_old:",
                           gsub("cod_old:", "",data_estaciones[[1]][7]), NA)

    estado[[i]] <- ifelse(is.na(data_estaciones[[1]][8]),
                          ifelse(substr(data_estaciones[[1]][7], 1, 8) == " estado:", gsub(" estado:", "",data_estaciones[[1]][7]), NA),
                          ifelse(substr(data_estaciones[[1]][8], 1, 8) == " estado:", gsub(" estado:", "",data_estaciones[[1]][8]), NA))

    data_stn[[i]] <- data.frame(estacion = stn[[i]], categoria = cat[[i]],
                                lat = lat[[i]], lon = lon[[i]], ico = ico[[i]],
                                cod = cod[[i]], cod_old = cod_old[[i]], estado = estado[[i]])
  }

  df_stns <- do.call("rbind", data_stn)
  return(df_stns)
}
