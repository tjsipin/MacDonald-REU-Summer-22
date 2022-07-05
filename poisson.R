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


options(scipen = 999)


early_data <- new_df %>%
  filter(Year < 2014) %>%
  select(-c(29:69)) %>%
  select(-c("AvgRad", "Zika", "Chikungunya")) %>%
  select(c('Population', 
           'LST_Day', 
           'LST_Night',
           'OptTemp_Obs',
           'Dengue_Alb_OptTemp',
           'Dengue_Aeg_OptTemp',
           'Chik_Alb_OptTemp',
           'Zika_OptTemp',
           'Malaria_OptTemp',
           'NDVI',
           'EVI', 
           'Precip', 
           'StableLights',
           'Cutaneous.Leishmaniasis',
           'SWOccurrence'))

poisson_early_data <- early_data %>%
  mutate(Cutaneous.Leishmaniasis = round(1000*Cutaneous.Leishmaniasis)) %>%
  filter(Cutaneous.Leishmaniasis > 0)

summary(m1 <- glm(Cutaneous.Leishmaniasis ~ . , data = poisson_early_data, family = 'poisson'))

# overdispersion ??
e2 <- resid(m1, type = 'pearson')
n <- nrow(poisson_early_data)
p <- length(coef(m1))
sum(e2^2) / (n - p) # overdispersion

# neg bin
m2 <- glm.nb(Cutaneous.Leishmaniasis ~ .^2 - 1 , data = poisson_early_data)
