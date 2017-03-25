library(tidyverse)
library(curl)
library(ggplot2)
library(seasonal)
library(tframe)
library(forecast)

#Get Mauna loa data
home <- getwd()
temp <- tempfile()
url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"
curl_download(url,temp)
data <- readLines(temp)
unlink(temp)

#Remove comment lines 
data = data[-(1:72)]

#Covert to df
data <- data.frame(data)

#Parse to columns
data <- separate(data, col="data", sep="[ ]+", into=c("year", 
                                                      "month", 
                                                      "decimal date", 
                                                      "average", 
                                                      "interpolated", 
                                                      "trend (season corr)", 
                                                      "days"))
#Convert to numeric
data <- as.data.frame(sapply(data, as.numeric))

#Add NA values
data$average[(data$average)==-99.99] <- NA

#Create ts
co2 <- ts(data$interpolated, frequency=12, start=c(1958,3))

#Create ts of April levels
April <- subset(co2), cycle(co2)==4)

#Do initial forecast
forecast(co2)

#Check default forecast method
result <- data.frame()
for (y in 1990:2016){
  zz <- tfwindow(co2, end=c(y,2))
  temp1 <- forecast(zz, h=2)
  temp2 <- as.data.frame(temp1)[2,]
  result <- rbind(result, temp2)
}

#Add previous values
result$Actual <- tail(April,dim(result)[1])

#Create difference to check hindcast
result$diff <- abs(result$Actual - result$`Point Forecast`)

#Summarize result
summary(result$diff)


