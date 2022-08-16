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
aad <-subset(aad, aad$Year > 2006)
head(aad)


tidy_df <- aad %>%
  dplyr::select(c("Population", "Cutaneous.Leishmaniasis", "LST_Day", "Precip", "AvgRad", "SWOccurrence", "NDVI", "EVI", "pland_forest", "te_forest", "enn_mn_forest"))
tidy_df <- subset(tidy_df, !is.na(tidy_df$AvgRad))
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

tsdisplay(Population)

# creating our time series data sets
Cutaneous.Leishmaniasis <- ts(tidy_df$Cutaneous.Leishmaniasis, start = c(2000), frequency = 1)
Population <- ts(tidy_df$Population, start = c(2000), frequency = 1)
LST_Day <- ts(tidy_df$LST_Day, start = c(2000), frequency = 1)
NDVI <- ts(tidy_df$NDVI, start = c(2000), frequency = 1)
EVI <- ts(tidy_df$EVI, start = c(2000), frequency = 1)
Precip <- ts(tidy_df$Precip, start = c(2000), frequency = 1)
AvgRad <- ts(tidy_df$AvgRad, start = c(2000), frequency = 1)
SWOccurrence <- ts(tidy_df$SWOccurrence, start = c(2000), frequency = 1)
pland_forest <- ts(tidy_df$pland_forest, start = c(2000), frequency = 1)
te_forest <- ts(tidy_df$te_forest, start = c(2000), frequency = 1)
enn_mn_forest <- ts(tidy_df$enn_mn_forest, start = c(2000), frequency = 1)

#displaying the time trends for each of our variables
tsdisplay(Cutaneous.Leishmaniasis)
tsdisplay(Population)
tsdisplay(LST_Day)
tsdisplay(NDVI)
tsdisplay(EVI)
tsdisplay(Precip)
tsdisplay(AvgRad)
tsdisplay(SWOccurrence)
tsdisplay(pland_forest)
tsdisplay(te_forest)
tsdisplay(enn_mn_forest)


# Population_lambda = BoxCox.lambda(Population)
# Population2 = BoxCox(Population, lambda= Population_lambda)
# tsdisplay(Population2)
# 
# LST_Day_lambda = BoxCox.lambda(LST_Day)
# LST_Day2 = BoxCox(LST_Day, lambda= LST_Day_lambda)
# tsdisplay(LST_Day2)
# 
# NDVI_lambda = BoxCox.lambda(NDVI)
# NDVI2 = BoxCox(NDVI, lambda= NDVI_lambda)
# tsdisplay(NDVI2)
# 
# EVI_lambda = BoxCox.lambda(EVI)
# EVI2 = BoxCox(EVI, lambda= EVI_lambda)
# tsdisplay(EVI2)
# 
# Precip_lambda = BoxCox.lambda(Precip)
# Precip2 = BoxCox(Precip, lambda= Precip_lambda)
# tsdisplay(Precip2)
# 
# AvgRad_lambda = BoxCox.lambda(AvgRad)
# AvgRad2 = BoxCox(AvgRad, lambda= AvgRad_lambda)
# tsdisplay(AvgRad2)
# 
# SWOccurrence_lambda = BoxCox.lambda(SWOccurrence)
# SWOccurrence2 = BoxCox(SWOccurrence, lambda= SWOccurrence_lambda)
# tsdisplay(SWOccurrence2)
# 
# pland_forest_lambda = BoxCox.lambda(pland_forest)
# pland_forest2 = BoxCox(pland_forest, lambda= pland_forest_lambda)
# tsdisplay(pland_forest2)


# Unit Ratio Tests (Checking to see if each variable is 
# stationary versus non-stationary)
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
