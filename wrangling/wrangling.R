# install packages
library(tidyverse)
library(dplyr)
library(glmnet)
library(xgboost)
library(ranger)
library(lsr)
library(corrr)
library(tidyr)
library(car)
library(moments)
library(ggpubr)
library(BBmisc)

# data

data = read.csv('data/Amazon_Data_Annual_for_TJ_5_22.csv')

# data = na.omit(data['Cutaneous.Leishmaniasis'])

new_df <- subset(data, !is.na(data$Cutaneous.Leishmaniasis))

summary(new_df$Cutaneous.Leishmaniasis[new_df$Cutaneous.Leishmaniasis > 0])

summary(new_df$Visceral.Leishmaniasis)

# split missing data

early_data <- new_df %>%
  filter(Year < 2014) %>%
  select(-c(29:69)) %>%
  select(-c("AvgRad", "Zika", "Chikungunya"))

later_data <- new_df %>%
  filter(Year >= 2014) %>%
  select(-c(29:69)) %>%
  select(-c("StableLights"))

# correlation ?
correlation_early <- select_if(early_data, is.numeric) %>%
  select(-c('Code', 'Year')) %>%
  correlate()

rplot(correlation_early, colors = c('blue','yellow')) + 
  theme(axis.text.x = element_text(angle = 90))

correlation_later <- select_if(later_data, is.numeric) %>%
  select(-c('Code', 'Year')) %>%
  correlate()

rplot(correlation_later, colors = c('blue','yellow')) + 
  theme(axis.text.x = element_text(angle = 90))

# qqplot - FIX
qq_data <- rnorm(early_data$Cutaneous.Leishmaniasis)
qqnorm(qq_data)
qqline(qq_data)



# transform Cutaneous.Leishmaniasis

early_data_t <- early_data  %>% 
  mutate(T.Cutaneous.Leishmaniasis = 
           log(Cutaneous.Leishmaniasis)) # log(X+1) does not fix

ggdensity(early_data_t, x = "Cutaneous.Leishmaniasis") + 
  stat_overlay_normal_density(color = 'red', linetype = 'dashed')
skewness(early_data_t$Cutaneous.Leishmaniasis)

# density plot
ggdensity(early_data_t, x = "T.Cutaneous.Leishmaniasis") + 
  stat_overlay_normal_density(color = 'red', linetype = 'dashed')

skewness(early_data_t$Cutaneous.Leishmaniasis, na.rm = T)


# Remove obs with CL == 0?

early_data_t <- early_data_t %>%
  filter(Cutaneous.Leishmaniasis > 0)

ggdensity(early_data_t, x = "T.Cutaneous.Leishmaniasis") + 
  stat_overlay_normal_density(color = 'red', linetype = 'dashed')

skewness(early_data_t$T.Cutaneous.Leishmaniasis, na.rm = T)

# rename column
names(early_data_t)[names(early_data_t) == 'T.Cutaneous.Leishmaniasis'] <- 'log.Cutaneous.Leishmaniasis'
