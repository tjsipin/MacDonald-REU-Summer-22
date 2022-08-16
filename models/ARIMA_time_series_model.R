# Loading Packages and Libraries

install.packages("vars")
library(vars)
install.packages("mFilter")
library(mFilter)
library(tseries)
install.packages("TSstudio")
library(TSstudio)
library(forecast)
install.packages("fpp")
library(fpp)
library(tidyverse)



# loading the data set
aad <- read.csv("models/data/aad.csv")
aad <-subset(aad, aad$Cutaneous.Leishmaniasis > 0)
aad <- subset(aad, !is.na(aad$Cutaneous.Leishmaniasis))
quantile(aad$Cutaneous.Leishmaniasis, probs = c(0.3, 0.4, 0.5, 0.6, 0.7))
aad <-subset(aad, aad$Year < 2019)
head(aad)


monthly_df <- read.csv("models/data/monthly_df.csv")
monthly_df <- subset(monthly_df, !is.na(monthly_df$Cutaneous.Leishmaniasis))
monthly_df <-subset(monthly_df, monthly_df$Cutaneous.Leishmaniasis > 0)
monthly_df <- subset(monthly_df, !is.na(monthly_df$Year))



tidy_df <- aad %>%
  dplyr::select(c("Year", "Population", "Cutaneous.Leishmaniasis", "LST_Day", "Precip", "SWOccurrence", "NDVI", "EVI", "pland_forest", "te_forest", "enn_mn_forest", "AvgRad")) %>%
  filter(Year > 2013)
#tidy_df <- subset(tidy_df, !is.na(tidy_df$AvgRad))
tidy_df <- subset(tidy_df, !is.na(tidy_df$SWOccurrence))
tidy_df <- subset(tidy_df, !is.na(tidy_df$LST_Day))
# tidy_df <- subset(tidy_df, tidy_df$AvgRad > 0)
# tidy_df <- subset(tidy_df, tidy_df$SWOccurrence > 0)
tidy_df$pland_forest <- ifelse(is.na(tidy_df$pland_forest), 
                                        0, tidy_df$pland_forest)
tidy_df$te_forest <- ifelse(is.na(tidy_df$te_forest), 
                                     0, tidy_df$te_forest)
tidy_df$enn_mn_forest <- ifelse(is.na(tidy_df$enn_mn_forest), 
        
                                                                 0, tidy_df$enn_mn_forest)
library(dplyr)

mean_Cutaneous <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(Cutaneous.Leishmaniasis), list(name = mean))
mean_Cutaneous

mean_Population <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(Population), list(name = mean))
mean_Population

mean_LST_Day <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(LST_Day), list(name = mean))
mean_LST_Day

mean_Precip <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(Precip), list(name = mean))
mean_Precip

mean_NDVI <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(NDVI), list(name = mean))
mean_NDVI

mean_EVI <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(EVI), list(name = mean))
mean_EVI

mean_SWOccurrence <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(SWOccurrence), list(name = mean))
mean_SWOccurrence

mean_AvgRad <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(AvgRad), list(name = mean))
mean_AvgRad

mean_pland_forest <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(pland_forest), list(name = mean))
mean_pland_forest

mean_te_forest <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(te_forest), list(name = mean))
mean_te_forest

mean_enn_mn_forest <- aad %>%
  group_by(Year) %>%
  summarise_at(vars(enn_mn_forest), list(name = mean))
mean_enn_mn_forest




tsdisplay(Population)

# creating our time series data sets
ts_df <- ts(tidy_df,start = 2014)
Cutaneous.Leishmaniasis <- ts(tidy_df$Cutaneous.Leishmaniasis,
                            start = min(tidy_df$Year), 
                            end = max(tidy_df$Year), 
                            frequency = 50)

Cutaneous.Leishmaniasis_diff1 <- diff(Cutaneous.Leishmaniasis, differences = 1)
Cutaneous.Leishmaniasis_diff2 <- diff(Cutaneous.Leishmaniasis, differences = 2)


Population <- ts(mean_Population, start = 2001)
Precip <- ts(mean_Precip, start = 2001)
AvgRad <- ts(mean_AvgRad, start = 2014)
SWOccurrence <- ts(mean_SWOccurrence, start = 2001)
LST_Day <- ts(mean_LST_Day, start = 2001)
NDVI <- ts(mean_NDVI, start = 2001)
EVI <- ts(mean_EVI, start = 2001)
pland_forest <- ts(mean_pland_forest, start = 2001)
te_forest <- ts(mean_te_forest, start = 2001)
enn_mn_forest <- ts(mean_enn_mn_forest, start = 2001)

# ARIMA time series forecasting model
plot.ts(Cutaneous.Leishmaniasis)
ARIMA_model <- auto.arima(Cutaneous.Leishmaniasis)
ARIMA_model
ARIMA_model_forecast <- forecast(ARIMA_model, 2)
ARIMA_model_accuracy <- accuracy(ARIMA_model_forecast)
ARIMA_model_accuracy

#plot actual vs forecast
plot(ARIMA_model_forecast)


#displaying the time trends for each of our variables
tsdisplay(ts_df[,"Cutaneous.Leishmaniasis"])
tsdisplay(ts_df[,"Population"])
tsdisplay(ts_df[,"LST_Day"])
tsdisplay(ts_df[,"NDVI"])
tsdisplay(ts_df[,"EVI"])
tsdisplay(ts_df[,"Precip"])
tsdisplay(ts_df[,"AvgRad"])
tsdisplay(ts_df[,"SWOccurrence"])
tsdisplay(ts_df[,"pland_forest"])
tsdisplay(ts_df[,"te_forest"])
tsdisplay(ts_df[,"enn_mn_forest"])


autoplot(ts_df[,"Cutaneous.Leishmaniasis"]) +
  ggtitle("Cases of Cutaneous Leishmaniasis between 
          2001 and 2018") +
  xlab("Year") +
  ylab("Cases per Thousands")

qplot(ts_df[,"Cutaneous.Leishmaniasis"], ts_df[,"Precip"], data=as.data.frame(tidy_df)) +
  ylab("Precip") + xlab("Cases per Thousand")

autoplot(ts_df[,1:11], facets=TRUE) +
  ylab("All variables")

GGally::ggpairs(ts_df[,1:11])

a <- window(ts_df, start=2001)
gglagplot(a)

ts_fit <- tslm(ts_df[,"Cutaneous.Leishmaniasis"] ~ 
       ts_df[,"Population"] + ts_df[,"NDVI"] + 
       ts_df[,"EVI"] + ts_df[,"Precip"] +
       ts_df[,"AvgRad"] + ts_df[,"SWOccurrence"] +
       ts_df[,"pland_forest"] + ts_df[,"te_forest"] +
       ts_df[,"enn_mn_forest"] + ts_df[,"LST_Day"])

fcast.ave <- forecast(ts_fit, )

library(tseries)

adf_Population = adf.test(Population)
adf_Population
# stationary

adf_LST_Day = adf.test(LST_Day)
adf_LST_Day
# stationary

adf_NDVI = adf.test(NDVI)
adf_NDVI
# stationary

adf_EVI = adf.test(EVI)
adf_EVI
# stationary

adf_Precip = adf.test(Precip)
adf_Precip
# stationary

adf_AvgRad = adf.test(AvgRad)
adf_AvgRad
# stationary

adf_SWOccurrence = adf.test(SWOccurrence)
adf_SWOccurrence
# stationary

adf_pland_forest = adf.test(pland_forest)
adf_pland_forest
# stationary

adf_te_forest = adf.test(te_forest)
adf_te_forest
# stationary

adf_enn_mn_forest = adf.test(enn_mn_forest)
adf_enn_mn_forest
# stationary


### Model Identification and Estimation

## Method 1: ACF and PACF

# ACF:
acf(Population, lag.max = 20)

# PACF:
pacf(Population, lag.max = 20)
# exceeds the boundary at lag 1 so set p = 1

## Method 2: Minimum AIC / BIC Criteria

#Automatic Selection Algorithm - Fast
auto.arima(Population, trace= TRUE, ic ="aicc", 
           approximation = FALSE)

#Auto Algorithm - Slow but more accurate
auto.arima(Population, trace= TRUE, ic ="aicc", 
           approximation = FALSE, stepwise = FALSE)

# final model 
finalmodel = arima(Population, order = c(5,1,0))
summary(finalmodel)

# compare multiple models
AIC(arima(Population, order = c(5, 1, 0)), # this one is best
    arima(Population, order = c(0, 1, 4)))


## Residual Diagnostics

# Check whether the residuals look like white noise 
# (Independent) p>0.05 then the residuals are independent 
# (white noise)
tsdisplay(residuals(finalmodel))
Box.test(finalmodel$residuals, lag = 20, type = "Ljung-Box")
# p-values shown for the Ljung-Box statistic plot are incorrect so calculate
#critical chi squared value
# Chi-squared 20 d.f. and critical value at the 0.05
qchisq(0.05, 20, lower.tail = F)
# Observed Chi-squared 13.584 < 31.41 so we don't reject null hypothesis
# It means residuals are independent or uncorrelated (white noise) at lags 1-20.  
# whether the forecast errors are normally distributed
qqnorm(finalmodel$residuals); qqline(finalmodel$residuals) # Normality Plot





ts_plot(Cutaneous.Leishmaniasis)
ts_plot(Population)
ts_plot(LST_Day)
ts_plot(NDVI)
ts_plot(EVI)
ts_plot(Precip)
ts_plot(AvgRad)
ts_plot(SWOccurrence)
ts_plot(pland_forest)
ts_plot(te_forest)
ts_plot(enn_mn_forest)
