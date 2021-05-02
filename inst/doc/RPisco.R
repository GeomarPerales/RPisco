## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)

## -----------------------------------------------------------------------------
library(RPisco)

## -----------------------------------------------------------------------------
PISCO.file <- "D:\\PISCOd_pp_2.1.nc"
x <- data.frame(PISCO.file, -76.11, -13.11)
data <- piscod(x)
head(data) 

## -----------------------------------------------------------------------------
PISCO.file <- "D:\\PISCOm_pp_2.1.nc"
x <- data.frame(PISCO.file, -76.11, -13.11)
data <- piscom(x)
head(data)

## -----------------------------------------------------------------------------
PISCO.file <- "D:\\PISCOd_tmn_v1.1.nc"
x <- data.frame(PISCO.file, -76.11, -13.11)
data <- piscod(x)
head(data)

## -----------------------------------------------------------------------------
PISCO.file <- "D:\\PISCOm_tmn_v1.1.nc"
x <- data.frame(PISCO.file, -76.11, -13.11)
data <- piscom(x)
head(data)

## -----------------------------------------------------------------------------
PISCO.file <- "D:\\PISCOd_tmx_v1.1.nc"
x <- data.frame(PISCO.file, -76.11, -13.11)
data <- piscod(x)
head(data)

## -----------------------------------------------------------------------------
PISCO.file <- "D:\\PISCOm_tmx_v1.1.nc"
x <- data.frame(PISCO.file, -76.11, -13.11)
data <- piscom(x)
head(data)

## -----------------------------------------------------------------------------
head(Huarpa.stations)

## -----------------------------------------------------------------------------
PISCO.file <- "D:\\PISCOm_pp_2.1.nc"

monthly.data <- list()
monthly.matrix <- data.frame(matrix(NA, 432, nrow(Huarpa.stations)))
for (i in 1:nrow(Huarpa.stations)) {
  monthly.data[[i]] <- piscom(data.frame(PISCO.file, Huarpa.stations$Lon[i], Huarpa.stations$Lat[i]))
  monthly.matrix[,i] <- monthly.data[[i]]$values
}
colnames(monthly.matrix) <- Huarpa.stations$Estacion
head(monthly.matrix)

## -----------------------------------------------------------------------------
ini.yr <- as.Date("1981-01-01")
end.yr <- as.Date("2016-12-01")
date <- seq(ini.yr, end.yr, by = "months")
monthly.matrix <- data.frame(date, monthly.matrix)
monthlyavg(data.frame(monthly.matrix$date, monthly.matrix[,2]))

## -----------------------------------------------------------------------------
yearlyavg(data.frame(monthly.matrix$date, monthly.matrix[,2]))

## -----------------------------------------------------------------------------
head(pisco2annual(data.frame(monthly.matrix$date, monthly.matrix[,2])))

