load(file = './models/data/gap_inp')

gap_inp <- gap.inp$ximp
aad <- read.csv('./models/data/aad.csv')

data <- gap_inp %>%
  dplyr::select(-c('Chikungunya':'Zika',28:67))

data <- cbind(data, aad %>% select(c(1,4,'Chikungunya':'Zika',28:69)))

data <- data %>%
  relocate(Code, .after = 1) %>% 
  relocate(Name, .after = 2)

save(data, file = './data/imp')