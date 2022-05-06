#loading the required libraries for the analysis
library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(readxl)
library(smooth)
library(rmarkdown)
install.packages("rmarkdown")

#loading the data sets into r-studio
data<-read_excel("C:\\Users\\abegu\\OneDrive\\Documents\\gs.xlsx")


#exploring the data
str(data)  #since the YEAR COLUMN IS IN double, we don`t have to be bothered since it will be converted to time series later
glimpse(data)
dim(data)


#splitting the data sets into train and test data 
dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]



#a look at the train and test dataset
View(train)
View(test)




#preparing the time series Object
data_ts <- ts(train[, 3], start = c(1970), end=c(2016), frequency = 1)


#create a utility function for calculating MEAN ABSOLUTE PERCENTAGE ERROR (MAPE) which will be used to evaluate the performance of the forecasting models. NB the lower the MAPE, the better the forecasting model 
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}



#naive forecasting method
naive_mod <- naive(data_ts, h = 15)
summary(naive_mod)
naivemethod<-as.data.frame(summary(naive_mod))
View(naivemethod)
accuracy(naive_mod)
autoplot(naive_mod)



snaive_mod <- snaive(data_ts, h = 15)
summary(snaive_mod)
snaivemethod<-as.data.frame(summary(snaive_mod))
View(snaivemethod)
accuracy(snaive_mod)
autoplot(snaive_mod)


#calculating MAPE ERROR
test$naive=7484.347
mape(test$VOLUME,test$naive)




#simple exponential smoothing
se_model <- ses(data_ts, h = 15)
summary(se_model)
autoplot(se_model)
accuracy(se_model)


#calculating MAPE ERROR FOR SE_MODEL
df_fc = as.data.frame(se_model)
test$simplexp = df_fc$'Point Forecast'
mape(test$VOLUME, test$simplexp)


#holt`s trend method
holt_model <- holt(data_ts, h = 15)
summary(holt_model)
autoplot(holt_model)
accuracy(holt_model)


#calculating MAPE ERROR FOR HOLT`S TREND MODEL
df_holt = as.data.frame(holt_model)
test$holt = df_holt$`Point Forecast`
mape(test$VOLUME, test$holt) 



#arima model
arima_model <- auto.arima(data_ts)
summary(arima_model)
autoplot(arima_model)
accuracy(arima_model)



#claculating the MAPE ERROR ON THE arima model
fore_arima = forecast::forecast(arima_model, h=15)
fore_arima
df_arima = as.data.frame(fore_arima)
test$arima = df_arima$`Point Forecast`
mape(test$VOLUME, test$arima) 


#TBATS-T- TRIGONOMETRIC TERMS FOR SEASONALITY,B-BOX-COX TRANSFORMATIONS FOR HETEROGENITY,A-ARMA ERRORS FOR SHORT TERM DYNAMICS,T-TREND,S-SEASONAL 
tbats_f <- tbats(data_ts)
summary(tbats_f)
autoplot(tbats_f)

#evaluating the performance of the test data
for_tbats <- forecast::forecast(tbats_f, h = 15)
for_tbats
df_tbats = as.data.frame(for_tbats)
test$tbats = df_tbats$`Point Forecast`
mape(test$VOLUME, test$tbats) 


#average forecasting
mean_f=meanf(data_ts,h=15)
mean_f
autoplot(mean_f)


#damped forecasting
damp_f<-holt(data_ts,damped=TRUE,phi=0.9, h=15)
autoplot(damp_f)
damp_f


#plotting all together
autoplot(data_ts)+autolayer(damp_f,series="Holt method",PI=F)+
  autolayer(mean_f,series="mean method",PI=F)+
  autolayer(fore_arima,series="arima method",PI=F)+
  autolayer(naive_mod,series="naive model",PI=F)+
autolayer(se_model,series="simple xponential modelling",PI=F)+
autolayer(for_tbats,series="TBATS",PI=F)

