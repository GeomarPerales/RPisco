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
monthly.data <- piscomgroup(data.frame(PISCO.file, Huarpa.stations))
head(monthly.data)

## -----------------------------------------------------------------------------
monthlyavg(data.frame(monthly.data$date, monthly.data[,2]))

## -----------------------------------------------------------------------------
yearlyavg(data.frame(monthly.data$date, monthly.data[,2]))

## -----------------------------------------------------------------------------
head(pisco2annual(data.frame(monthly.matrix$date, monthly.data[,2])))

