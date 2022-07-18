library(tidyverse)
library(dplyr)
library(glmnet)
library(pscl)
require(sandwich)
require(ggplot2)
require(msm)
library(lattice)
library(MASS)
library(lmtest)
library(rsample)

options(scipen = 999)

data = read.csv('models/data/aad.csv')

# data = na.omit(data['Cutaneous.Leishmaniasis'])

new_df <- subset(data, !is.na(data$Cutaneous.Leishmaniasis))


early_data <- new_df %>%
  filter(Year < 2014) %>%
  dplyr::select(-c(29:69)) %>%
  dplyr::select(-c("AvgRad", "Zika", "Chikungunya")) %>%
  dplyr::select(c('Population', 
           'LST_Day', 
           'LST_Night',
           'NDVI',
           'EVI', 
           'Precip', 
           'StableLights',
           'Cutaneous.Leishmaniasis',
           'SWOccurrence'))

poisson_early_data <- early_data %>%
  mutate(Cutaneous.Leishmaniasis = round(1000*Cutaneous.Leishmaniasis)) 
# %>%
  # filter(Cutaneous.Leishmaniasis > 0)

# training - testing split
data_split <- initial_split(poisson_early_data,
                            strata = Cutaneous.Leishmaniasis,
                            prop = 0.9)
data_train <- training(data_split)
data_test <- testing(data_split)




summary(m1 <- glm(Cutaneous.Leishmaniasis ~ . , data = data_train, family = 'poisson'))

# overdispersion ??
e2 <- resid(m1, type = 'pearson')
n <- nrow(poisson_early_data)
p <- length(coef(m1))
sum(e2^2) / (n - p) # overdispersion

# neg bin
m2 <- glm.nb(Cutaneous.Leishmaniasis ~ ., 
             data = data_train,
             link = 'logit')

m3 <- zeroinfl(Cutaneous.Leishmaniasis ~ .,
               data = data_train,
               dist = "negbin")

m4 <- hurdle(Cutaneous.Leishmaniasis ~ .,
             data = data_train,
             dist = "negbin")

predictions = predict(m4, data = data_test)

testRMSE = sqrt(mean((predictions - data_test$Cutaneous.Leishmaniasis)^2))
