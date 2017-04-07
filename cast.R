library(RNetCDF)
library(data.table)
library(tidyverse)
library(forecast)
library(tframe)
library(fpp)

setwd("C:/Users/andrewkc/Documents/forecast")

########################################################

co2 <- readLines("co2_mm_mlo.txt")

#Remove comment lines 
co2 = co2[-(1:72)]

#Covert to df
co2 <- data.frame(co2)

#Parse to columns
co2 <- separate(co2, col="co2", sep="[ ]+", into=c("year", 
                                                      "month", 
                                                      "decimal date", 
                                                      "average", 
                                                      "interpolated", 
                                                      "trend (season corr)", 
                                                      "days"))

#Convert to numeric
co2 <- as.data.frame(sapply(co2, as.numeric))

#Add NA values
co2$average[(co2$average)==-99.99] <- NA

#Create ts

co2.cont <- ts(co2$average[(max(which(is.na(co2$average)))+1):length(co2$average)], 
               frequency=12,  
               start=c(1984,5))

co2.orig <- ts(co2$interpolated, frequency=12, start=c(1958,3))

##########################################
Wind <- read.csv("wind.csv", header=TRUE, stringsAsFactors = FALSE)

dates <- data.frame(Time=unique(Wind$Time), Num=1:710)

Wind <- merge(Wind, dates, by="Time")

Wind <- Wind %>%
  group_by(Num) %>%
  summarise(merid=mean(meridional.wind), zon=mean(zonal.wind), speed=mean(win.speed))

merid <- ts(head(Wind$merid,709), start=c(1958,3), frequency=12)
zon <- ts(head(Wind$zon,709), start=c(1958,3), frequency=12)
speed <- ts(head(Wind$speed,709), start=c(1958,3), frequency=12)

########################################################
HadSST <- read.table("HadSST.3.1.1.0.time_series.txt")

revtrunc <- function(x) { x - floor(x) } 

decimals <- sort(unique(revtrunc(time(co2))))

decimaldates <- data.frame("decimal" = decimals, "V2" = seq(1:12))

HadSST <- HadSST[,c(1,2,12)]

HadSST <- HadSST %>%
  group_by(V1,V2) %>%
  summarise(mean=mean(V8)) 

HadSST <- ts(HadSST$mean, frequency=12, end=c(2017,3))

########################################################

#Make lags of wind data
speed.lag <- tail(lag(speed),708)
zon.lag <- tail(lag(zon),708)
merid.lag <- tail(lag(merid),708)

#Make difference of co2 data
co2 <- tfwindow(diff(co2.orig), start=tfstart(speed.lag), end=tfend(speed.lag))

#Make lag of sea surface temp data
sst.lag <- tfwindow(lag(HadSST), start=tfstart(co2), end=tfend(co2))

#Test a simple regression
linfit <- lm(co2 ~ sst.lag + speed.lag + zon.lag + merid.lag)
sum(coef(linfit) * c(1, tail(HadSST,1), tail(speed, 1), tail(zon, 1), tail(merid, 1)))

#Form matrix of wind and sea temps
covariates <- matrix(cbind(sst.lag,speed.lag,zon.lag,merid.lag), ncol=4, nrow=708)

#Add auto arima
fit <- auto.arima(co2, xreg=covariates)

#Display residuals
tsdisplay(arima.errors(fit), main="ARIMA errors")
residuals.Arima(fit, type="regression")

#Test residuals
Box.test(residuals(fit),fitdf=5,lag=10,type="Ljung")

#Forecast April 2017!!
final <- forecast(fit, 
                  xreg=cbind(tail(HadSST,1), tail(speed, 1), tail(zon, 1), tail(merid, 1)), 
                  h=1,
                  level=c(95,99))

tail(co2.orig,1) + final$mean[1]
