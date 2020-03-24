# IPv4 Runout Phase 3 Modeling: carlos@lacnic.net
# v1: 2018-02-15 @Brisbane
# v3: 2020-02-20 @Montevideo

# dependencies
# install.packages("jsonlite")
# install.packages("curl")
library("jsonlite")

# clean environment
rm(list=ls())

# helper function: converts a "mongo date" string into an Unix-like timestamp
tsFromMongoDate <- function(x) {
  a = gsub("Date(", "", x, fixed=TRUE)
  b = gsub(")", "", a, fixed=TRUE)
  e = unlist(strsplit(b, ","), use.names = FALSE)
  
  f = paste(e[1],as.integer(e[2])+1,e[3], sep="-") 
  r = gsub(" ", "", f)
  #return(r)
  p = as.numeric(as.POSIXct(r, tz="UTC"))
}
# -- end function --

# Getting and cleaning data
# setwd("~/scratch")

getCleanIPv4Data <- function(EspacioReservado) {
  ## TODO: get lastday offset automatically, not in a fixed way
  d <- fromJSON("http://opendata.labs.lacnic.net/ipv4stats/ipv4avail/lacnic?lastdays=365")
  # d <- fromJSON("freeipv4_lacnic.json")
  e <- unlist(d[1], use.names=FALSE, recursive=FALSE)
  f <- t(data.frame(e))
  
  dates_free4 = lapply(f[,1], tsFromMongoDate)
  free4 = lapply(f[,2], as.integer)
  
  free4 = t ( as.matrix( as.data.frame(free4)))
  free4 = as.numeric(free4[,1])
  free4 = free4 + EspacioReservado 
  
  dates_free4 = t ( as.matrix( as.data.frame(dates_free4)))
  dates_free4 = as.numeric(dates_free4[,1])
  
  ipv4 = NULL
  ipv4 = cbind(ipv4, dates_free4)
  ipv4 = cbind(ipv4, free4)
  ipv4 = as.data.frame(ipv4)
  
  return(ipv4)
}

# ----------------------------------------
smoothIPv4Series <- function(wipv4) {
  
  # wipv4.lowess = smoothLowess(wipv4, f=0.1)
  
  wipv4.smooth = lowess(wipv4)
  
  return (wipv4.smooth)
  
}
# ----------------------------------------

# plotFase3 argumentos:
# Grado : grado del polinomio
# EspacioReservado: cantidad de direcciones IPv4 reservadas
# wFutureHorizon : cantidad de dias en el futuro para buscar el cero de la regresión
# wipv4 : data frame con dates_free4 y free4

plotFase3 <- function(wipv4, Grado, EspacioReservado, wFutureHorizon) {

  # ipv4.raw <- getCleanIPv4Data(EspacioReservado)
  
  # ipv4 <- smoothIPv4Series(ipv4.raw)
  ipv4 <- wipv4
  
  ## Modeling and prediction
  # m5 = lm(free4 ~ dates_free4, data = ipv4)
  ## Grado = 3
  m5 = lm(free4 ~ poly(dates_free4, Grado), data = ipv4)
  pr2 = predict(m5, interval="confidence")
  
  # plot(dates_free4, free4)
  # lines(dates_free4, pr2[,'fit'], col="green", lwd=3)
  
  # Forecasting ----------------------------------------------------
  secsday = 86400
  # future_horizon_days = 365*2.3
  future_horizon_days = wFutureHorizon*1.1
  future_dates_free4 = seq(ipv4$dates_free4[1], ipv4$dates_free4[1]+secsday*future_horizon_days, secsday)
  future_free4 = predict(m5, newdata = data.frame(dates_free4=future_dates_free4))
  fcast = as.data.frame( cbind(future_dates_free4, future_free4))
  cutoff = which(fcast$future_free4 < 0)
  cutoff_offset = cutoff[1]
  cutoff_ts = future_dates_free4[cutoff_offset]
  cutoff_date = as.Date(as.POSIXct(cutoff_ts, origin="1970-01-01"))
  # -----------------------------------------------------------------
  
  # Plotting
  plot(c(future_dates_free4, ipv4$dates_free4), c(future_free4, pr2[,'fit']), 
       col='green', lwd=1, xlab='', ylab='', xaxt="n", yaxt="n")
  axis(side=2, seq(from=0, to=5)*1e6, c("0", "1M", "2M", "3M", "4M", "5M"))
  ttlabels = c("2017-1-1","2018-1-1", "2019-1-1", "2020-1-1", "2021-1-1")
  tticks = as.numeric(as.POSIXct(ttlabels, tz="UTC"))
  axis(side=1, tticks, ttlabels)
  title("LACNIC Fase 3 de agotamiento de IPv4", sub=paste("Modelo grado ", Grado), xlab="Tiempo", ylab="Direcciones IPv4 libres (milliones)")
  lines(ipv4$dates_free4, ipv4$free4, col='red', lwd=3, xlab='')
  lines(c(future_dates_free4, ipv4$dates_free4), numeric(length(c(future_dates_free4, ipv4$dates_free4))), col='blue', xlab='')
  points(future_dates_free4[cutoff_offset], 0, col='red', lwd=5, xlab='')
  text(cutoff_ts, 300000, labels=c(cutoff_date))
  
  #print(summary(m5))
  
  return(cutoff_date)
} # end function plotFase3
# ------------------------------------------------------------------------

# definicion del espacio reservado (un /15)
EspacioReservado = 1334016

ipv4.raw <-  getCleanIPv4Data(EspacioReservado)
ipv4.smooth <- as.data.frame(  smoothIPv4Series( ipv4.raw  ) )

ipv4.smooth2 <- data.frame(dates_free4=as.array(ipv4.smooth$x), free4=as.array(ipv4.smooth$y))
# ipv4.smooth2$dates_free4 = as.array(ipv4.smooth$x)

# Fecha de generacion
FechaDeGeneracion = format(Sys.time(), "%a %b %d %X %Y")

cutoff_date_rate = plotFase3(ipv4.raw, 1, EspacioReservado, 365*1.12)
cutoff_date_smooth = plotFase3(ipv4.smooth2, 1, EspacioReservado, 365*2)
# plotFase3(2)
# plotFase3(3)

## end
