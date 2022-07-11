library(tidyverse)
set
aad <- read.csv('./models/data/aad.csv')

mean_CL <- aad %>% 
  group_by(Year) %>%
  summarise_at(vars(Cutaneous.Leishmaniasis), list(name = mean))

