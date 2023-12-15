install.packages("tseries")
install.packages("adf")
install.packages("forecast")
install.packages("lmtest")
library(zoo) 
library(tseries) 
library(forecast) 
library(lmtest)
library(TSA)
library(car)
#buat bikin time series
#buat uji stationeritas
#buat auto.arima
#buat coeftest

temp0 <- read.csv("D://rakamin_qty_time_series.csv") #download datanya dari kaggle
head(temp0)
y<-seq(as.Date("2022-01-01"),as.Date("2022-12-31"),by = 1)
#preprocessing

#bikin dataframe baru yang isinya date sama tavg aja sekalian ubah tanggal ke format date
temp <- data.frame(y, temp0)
colnames(temp) <- c("Tanggal","Qty")
temp$Tanggal <- as.Date(temp$Tanggal) 
head(temp)

#mengambil data temperatur di bulan juni-desember 2022
temp <- temp[temp[["Tanggal"]] >= "2022-06-01",]
rownames(temp) <- 1:nrow(temp)
head(temp)
#cek jumlah datanya sekarang
dim(temp)

#mengecek missing values
sum(is.na(temp$Tanggal)) ; sum(is.na(temp$Temperatur))

#mengecek outlier
boxplot(temp$Temperatur)

#mengubah data ke bentuk time series
temp <- read.zoo(temp)
head(temp)

#plot time seriesnya
plot(temp, xlab="Date", ylab="Temperatur")

#uji stasioneritas
adf.test(temp)

#differencing satu kali 
difftemp <- diff(temp,differences = 1)
plot(difftemp)
adf.test (difftemp)

#spesifikasi model
pacf(difftemp)
acf(difftemp)
eacf(difftemp)
auto.arima(difftemp)

#model yang diajukan
model1 <- Arima(temp,order=c(1,1,0))
model2 <- Arima(temp,order=c(0,1,2))
model3 <- Arima(temp,order=c(1,1,2))
model4 <- Arima(temp,order=c(2,1,2))


#uji signifikansi parameter
coeftest(model1,method="ML") ;coeftest(model2,method="ML");coeftest(model3,method="ML");coeftest(model4,method="ML");coeftest(model5,method="ML")
data.frame(AIC(model1),AIC(model2),AIC(model3),AIC(model4),BIC(model1),BIC(model2),BIC(model3),BIC(model4))


#model yang dipilih model 2, yaitu arima(0,1,2)

#estimasi parameter
model <- Arima(temp,order=c(0,1,2))
model


#diagnosis model
#uji asumsi residual stasioner, independen, berdistribusi normal
#uji stasioner residual
adf.test(model$residuals)

#uji independensi residual ljung-box
checkresiduals(model$residuals)

#uji normalitas residual
shapiro.test(model$residuals)
jarque.bera.test(model$residuals)
#karena ga normal cek qqplot
qqPlot(model$residuals)

#overfitting model
#menambahkan order AR
overfit1 <- Arima(temp, order=c(1,1,2))
model ;overfit1

#uji t overfitting ARIMA(1,1,2)
stat_uji_1 <- overfit1$coef[['ar1']]/0.2608
stat_uji_1 
df <- length(temp)-1
df
daerah_kritis <- qt(0.025, df)
daerah_kritis
stat_uji_1;daerah_kritis
2*(pt(stat_uji_1, df))

#menambahkan order MA
overfit2 <- Arima(temp, order=c(0,1,3))
model;overfit2

#uji t overfitting ARIMA(0,1,3)
stat_uji_2 <- overfit2$coef[['ma3']]/0.0661
stat_uji_2
df <- length(temp)-1
df
daerah_kritis <- qt(0.025, df)
daerah_kritis
stat_uji_2;daerah_kritis
2*(pt(stat_uji_2, df))

#forecasting
#data testing
test <- window(temp, start=as.Date(as.character("2022-06-01")),end=as.Date(as.character("2022-08-31")))
#data training
train <- window(temp, start=as.Date(as.character("2022-09-01")),end=as.Date(as.character("2022-12-31")))
trainmodel <- Arima(train, order = c(0, 1, 2))
#forecasting data training untuk 3 periode ke depan
testfc <- forecast(trainmodel, h = 3) ; testfc
plot(testfc)


#Forecasting (Periode = 3)
fc <- forecast(model, h = 3)
plot(fc)
fc

