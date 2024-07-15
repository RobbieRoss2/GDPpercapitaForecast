##############################
#This report...
##############################

##############################
#install and load all required packages
##############################

rm(list=ls())
install.packages("fpp2")
install.packages("forecast")
install.packages("tseries")
install.packages("WDI")

require(fpp2)
require(forecast)
require(tseries)
require(WDI)

##############################
#load and clean data
##############################

#load the WDI data
rawdata <- WDI(country = c("GB", "US", "CA","CH", "JP", "DK", "NO"), 
            indicator = c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"), 
            start = 1960, end = 2000)

#tidy column names
names(rawdata) <- c("Country","iso2c","iso3c", "Year", "PerCapitaGDP", "GDP")

#subset data
myvars <- c("Country", "Year", "PerCapitaGDP")
cleandata <- rawdata[myvars]

#view clean data
ggplot(cleandata, mapping = aes(Year, PerCapitaGDP, colour = Country, linetype = Country))+ geom_line()

#get UK data
ukdata <- cleandata$PerCapitaGDP [cleandata$Country == "United Kingdom"]

##############################
#create time series
##############################

#create time series for UK from min and max date within cleandata
ukts <- ts(ukdata, start = min(cleandata$Year), end = max(cleandata$Year))

#visualize time series
autoplot(ukts, xlab = "Year", ylab = "GDP Per Capita (Current US$)")

##############################
#initial analysis of time series
#check that it is stationary - constant mean/variance/autocorrelation with time
#check for seasonality
##############################

autoplot(ukts, xlab = "Year", ylab = "GDP Per Capita (Current US$)") + autolayer(ma(ukts, 5), series = "5 Year Moving Average")
#does not appear stationary from initial view - clear positive trend, increase in variation

#Augmented Dickey-Fueller Test - H0: This series is non-stationary
adf.test(ukts)
#p-value > 0.05 hence we can't reject the null and conclude that this is likely to be stationary

#autocorrelation test
acf(ukts, main = "UK Time Series Autocorrelation Test")
#outside CIs so this is does not show constant autocorrelation - dampens over lags

##############################
#Transformations
#The time series is clearly non-stationary so requires transformations before a model can be fit
#don't need to investigate seasonality as it is yearly data
##############################

#check how many differences are required for the time series to become stationary
ndiffs(x = ukts)

diffukts <- diff(ukts)

autoplot(diffukts, xlab = "Year", ylab = "GDP Per Capita (Current US$)") + autolayer(ma(diffukts, 5), series = "5 Year Moving Average")
acf(diffukts) #looks better however still shows very small autocorrelation at one lags, it is often expected for around 1/20 to be significatn due to random fluctuations
adf.test(diffukts)

##############################
#Benchmark Models
#Naive
#Average
#Drift
##############################
fit_naive <- naive(diffukts)
summary(fit_naive) #sd = 1274
checkresiduals(fit_naive) #p-value = 0.1441

fit_average <- meanf(diffukts)
summary(fit_average) #sd = 1130
checkresiduals(fit_average) #p-value = 0.01147

fit_drift <- rwf(diffukts, drift = TRUE)
summary(fit_drift) #sd = 1291
checkresiduals(fit_drift) #p-value = 0.1441

autoplot(diffukts) + autolayer(fit_naive, PI = FALSE, series = "Naive") + autolayer(fit_drift, PI = FALSE, series = "Drift") + autolayer(fit_average, PI = FALSE, series = "Mean")

##############################
#ETS Model
##############################
fit_ets <- ets(ukts)
summary(fit_ets)
checkresiduals(fit_ets)
#Ljung-Box pvalue < 0.05 so reject null and conclude that there is autocorrelation in the residuals in this model

##############################
#ARIMA model
##############################
fit_arima <- auto.arima(x = ukts, approximation = FALSE)
arima_forecast <- forecast(fit_arima, h = 5)
autoplot(arima_forecast, include = 20)
summary(fit_arima) #sd = 1071
checkresiduals(fit_arima) #p-value = 0.136 > 0.05 - all values in ACF CI, hence white noise

##############################
#Evaluate Model
##############################

#compare benchmark fit naive with arima
summary(fit_naive) #sd = 1274 MAE = 836.4339 RMSE = 1274.847
summary(fit_arima) #sd = 1071 AIC = 675.74 BIC = 68028 MAE = 774.5241 RMSE = 1031.501
#all error values are clearly lower for arima hence this is the better model

realdata <- WDI(country = c("GB"), 
                           indicator = c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"), 
                           start = 2001, end = 2005)

names(realdata) <- c("Country","iso2c","iso3c", "Year", "PerCapitaGDP", "GDP")

myvars2 <- c("Country", "Year", "PerCapitaGDP")
cleanrealdata <- realdata[myvars2]
ukrealdata <- cleanrealdata$PerCapitaGDP [cleanrealdata$Country == "United Kingdom"]


realdatats <- ts(ukrealdata,start = min(cleanrealdata$Year), end = max(cleanrealdata$Year))
autoplot(realdatats)

autoplot(arima_forecast, include = 10,xlab = "Year", ylab = "GDP Per Capita (Current US$)", main = "ARIMA Forecast") + autolayer(realdatats, series = "Real Data")
#two year forecast is accurate but later years is less accurate (obviously)
#We can see that the 2001 and 2002 predictions are great as the true value lies within the 80% confidence interval.
#The 2003 true value lies just above the 95% confidence interval
#The predicted values for 2004 and 2005 are far off of the true values 

##############################
#Conclusion
##############################

#The results of this forecast were fairly accurate - 2 spot on, 1 just out of range, and the final 2 quite far off

#It is very important to predict the GDP per capita as it is a great measure of how the economy is performing in a country
#A growing economy means that companies are doing better and therefore bringing in more money - this money will then be taxed and can be used by the government to provide better goods and services to their communities
#By predicting the economic growth it is then easier for governments to predict how much more money they will have available for this

#If this was to be repeated, I would spend more time investigating transformations in hopes to fit a better model as well as explore other forecasting methods and models in hopes to find a better fit


