library(readr)
library(finalfit)
library(dplyr)


monthly <- read_csv('data/monthly_df.csv')

quantile_data <- monthly_df %>% 
  filter(!is.na(Cutaneous.Leishmaniasis)) %>% 
  filter(Cutaneous.Leishmaniasis > 0) %>% 
  select(Cutaneous.Leishmaniasis)

quantile <- (quantile_data$Cutaneous.Leishmaniasis %>% 
  quantile(.4))[[1]]

monthly_df <- monthly %>% 
  rename('Cutaneous.Leishmaniasis' = 'Cutaneous Leishmaniasis') %>% 
  group_by('Year') %>% 
  select(c('LST_Day', 'Year', # include LST_Night?
           'NDVI', 'EVI', 'Precip', 
           'AvgRad', 'SWOccurrence', 'pland_forest',
           'te_forest', 'enn_mn_forest', 'area_mn_forest', 'Population',
           'Cutaneous.Leishmaniasis')) %>% 
  mutate(Cutaneous.Leishmaniasis = ifelse(Cutaneous.Leishmaniasis < quantile, 'low', 'high') %>% as.factor)


missing_plot(monthly)


monthly %>% 
  group_by(Year) %>% 
  summary()

library(naniar)
library(ggplot2)

ggplot(monthly_df,
       aes(x = te_forest,
           y = Cutaneous.Leishmaniasis)) + 
  geom_miss_point() + 
  facet_wrap(~ Year) + 
  theme_dark()

gg_miss_var(monthly_df,
            facet = Year,
            show_pct = T)

gg_miss_fct(x = monthly_df$te_forest,
            fct = monthly_df$Cutaneous.Leishmaniasis)
