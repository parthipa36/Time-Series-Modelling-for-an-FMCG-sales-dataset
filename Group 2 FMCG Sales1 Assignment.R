

#=============================================================
# Importing the data set and libraries
#=============================================================

# 1. Inputing the data
#####------- NOTE: PLEASE SET YOUR OWN DIRECTORY TO READ FILE-----
  setwd('C:\\Users\\PARTHI vs BHARATHI\\Downloads\\PRAXIS\\A Term 2\\ECM\\ECM presentation FMCG Sales1')
  fmcg_sales =read.csv("FMCG Sales1.csv")

# 2. Summary and view of the data
  summary(fmcg_sales)
  View(fmcg_sales)

# 3. Importing Libraries
  
  #INSTALLING THE PACKAGES FIRST:
  install.packages("ggplot2")
  install.packages("ggthemes")
  install.packages("forecast")
  install.packages("tseries")
  install.packages("zoo")
  install.packages("scales")
  install.packages("xts")
  install.packages("tm")
  install.packages("astsa")
  install.packages("TTR")
  
  
  library(ggplot2)
  library(ggthemes)
  library(forecast)
  library(tseries)
  library(zoo)
  library(scales)
  library(xts)
  library(tm)
  library(astsa)
  library(TTR)
  
  
# ==========================================================================================
  # Plotting Time series model
# ==========================================================================================
  
  # Replacing data with date form:
  fmcg_sales[,1] <-  as.Date(paste("01",fmcg_sales$Month,sep="-"),"%d-%b-%y")

  ggplot(aes(Month, Shampoo.Sales), data = fmcg_sales) + 
    geom_point() + stat_smooth(method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 2, raw=TRUE)) + 
    ggtitle("Time Series Vs Shampoo Sales") + theme(plot.title = element_text(hjust = 0.5, size = 22)) +
    xlab("Time") + ylab("Shampoo Sales")   + scale_color_manual(values = "red") + geom_line(aes(Month, Shampoo.Sales), color = "red") 
  
  
  # Applying time series model to the data frame
  
  time_sales <-  ts(fmcg_sales$Shampoo.Sales, start = c(1995,1),end = c(1997,12),frequency = 12)
  class(time_sales)
  plot(time_sales)
  View(time_sales)
  
  #View(as.zoo(time_sales))

  
# Decompsition of time series Model
# For identifying wether our model is additive or multiplicative we have to 
  #decompose our model into its component
 
  time_sales.decompose <- decompose(time_sales , type="additive")
  autoplot(time_sales.decompose)
  plot(time_sales.decompose)
  autoplot(time_sales.decompose$seasonal,main="Seasonal Component",xlab="Time",ylab="Seasonality",col="blue")
  autoplot(time_sales.decompose$trend,main="Trend Component",xlab="Time",ylab="Trend",col="purple")
  autoplot(time_sales.decompose$random,main="Random Component",xlab="Time",ylab="Random",col="green")
  
#==========================================================================================
  # Simple Moving Average
#==========================================================================================
  
  time_salesSMA3 <- SMA(time_sales,n=3)
  time_salesSMA5 <- SMA(time_sales,n=5)
  time_salesSMA8 <- SMA(time_sales,n=8)
  
  ggplot()  +
    geom_line( aes(x = index(as.zoo(time_salesSMA3)),y = time_salesSMA3, color = "SMA 3")) +
    geom_line( aes(x = index(as.zoo(time_salesSMA5)),y = time_salesSMA5, color = "SMA 5")) +
    geom_line( aes(x = index(as.zoo(time_salesSMA8)),y = time_salesSMA8, color = "SMA 8")) +
    geom_line( aes(x = index(as.zoo(time_sales)),y = time_sales, color = "Series")) +
    theme(plot.title = element_text(hjust = 0.5, size = 22)) + xlab("Time") + ylab("Shampoo Sales") + ggtitle("Simple Moving Average")
  
#=========================================================================================
  # Auto ARIMA Prediction
#=========================================================================================
  
  
  # Degree of differencing for stationarity
  ndiffs(time_sales)
  
  # Converting to stationary data
  stationary_df <- diff(time_sales)
  View(stationary_df)
  
  # Plot the sample P/ACF pair of the differenced data 
  acf2(stationary_df)
  
  #plotting stationary data
  autoplot(stationary_df)
  
  #test for stationary data
  adf.test(stationary_df)
  
  #auto arima
  ts_arima<-auto.arima(time_sales)
  
  #arima sunmmary
  summary(ts_arima)
  
  #forecast for auto arima model
  ts_forecast <-forecast(ts_arima,12)
  
  #plotting the forecasted values
  autoplot(ts_forecast)+
    ylab(label = "shampoo sales")
  
  
#==========================================================================================
  # Naives Prediction
#==========================================================================================
  
  cb.naive <- naive(time_sales, h = 36)
  
  # Plot and summarize the forecasts
  autoplot(cb.naive) + labs(x="Year" , y="Shampoo Sales")

  time_sales %>% naive() %>% checkresiduals()
  summary(cb.naive)
  View(time_sales)
#==========================================================================================
  # Holt winter's Prediction
#==========================================================================================
  
  sales <- window(time_sales,start=1995)
  #fit1 <- hw(sales, seasonal = "additive)
  fit1 <- hw(sales)
  autoplot(sales) +
    autolayer(fit1, series="HW forecasts", PI=FALSE) +xlab("Year") +
    ylab("Sales") +
    ggtitle("Shampoo Sales") +
    guides(colour=guide_legend(title="Forecast")) 
  
  summary(fit1)
  ets(sales)
  
  
  
  
  
  
  
  
  
