library(finalfit)
library(tidyverse)
library(dplyr)

early_aad <- aad %>%
  filter(Year < 2014) %>%
  filter(!is.na(Cutaneous.Leishmaniasis)) %>% 
  filter(Cutaneous.Leishmaniasis > 0) %>%
  dplyr::select(c('Cutaneous.Leishmaniasis', 'LST_Day', # include LST_Night?
                  'NDVI', 'EVI', 'Precip', 
                  'StableLights', 'SWOccurrence', 'pland_forest',
                  'te_forest', 'enn_mn_forest','Population'))

cl_aad <- aad %>% 
  select(-c(Chikungunya, 
            Dengue,
            Malaria,
            Mucosal.Leishmaniasis,
            Visceral.Leishmaniasis,
            Yellow.Fever,
            Zika)) %>% 
  group_by(Country)
missing_plot(cl_aad,
             dependent = 'Cutaneous.Leishmaniasis',
             explanatory = c('LST_Day', # include LST_Night?
                             'NDVI', 'EVI', 'Precip', 
                             'StableLights', 'SWOccurrence', 'pland_forest',
                             'te_forest', 'enn_mn_forest','Population'))

monthly <- read_csv(file = './data/monthly_df.csv')

missing_plot(monthly %>% group_by(Country, Year),
             dependent = 'Cutaneous Leishmaniasis',
             explanatory = c('Population', 'LST_Day', # include LST_Night?
                             'NDVI', 'EVI', 'Precip', 
                             'StableLights', 'SWOccurrence', 'pland_forest',
                             'te_forest', 'enn_mn_forest'))


monthly <- rename(monthly, 'Cutaneous.Leishmaniasis' = 'Cutaneous Leishmaniasis')
explanatory = c('Population', 'LST_Day', # include LST_Night?
                'NDVI', 'EVI', 'Precip', 
                'StableLights', 'SWOccurrence', 'pland_forest',
                'te_forest', 'enn_mn_forest')
dependent = 'Cutaneous.Leishmaniasis'

monthly %>% 
  select(c('Cutaneous.Leishmaniasis', 'Population', 'LST_Day', # include LST_Night?
         'NDVI', 'EVI', 'Precip', 
         'StableLights', 'SWOccurrence', 'pland_forest',
         'te_forest', 'enn_mn_forest')) %>%   
  missing_pairs(dependent, explanatory)

