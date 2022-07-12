data <- gap_inp %>%
  select(-c(28:67))

data <- cbind(data, aad %>% select(c(1,4,28:69)))

data <- data %>%
  relocate(Code, .after = 1) %>% 
  relocate(Name, .after = 2)

save(data, file = './data/imp.R')

