library(rsample)
library(recipes)
library(randomForest)
library(parsnip)
library(workflows)
library(tune)
library(dials)
library(yardstick)

# PREDICTIVE MODELING FOR CUTANEOUS LEISHMANIASIS



# linear model

early_data_t[sapply(early_data_t, is.character)] <- lapply(
  early_data_t[sapply(early_data_t, is.character)], 
  as.factor)

early_data_t[, c('Code', 'Year')] <- lapply(
  early_data_t[, c('Code', 'Year')], factor)

early_t_lm <- lm(log.Cutaneous.Leishmaniasis ~ Country, data = early_data_t)

plot(log.Cutaneous.Leishmaniasis ~ Country, data = early_data_t)

# splits
## CV train and test split



# recipe

recipe <- recipe(
  Cutaneous.Leishmaniasis ~ Year + Country + Population +
    Dengue + Malaria + Mucosal.Leishmaniasis + 
    Visceral.Leishmaniasis + Yellow.Fever +
    LST_Day + LST_Night + OptTemp_Obs + 
    NDVI + EVI + Precip + StableLights + SWOccurrence, 
  data = data_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_predictors())

# k-fold CV

data_folds <- vfold_cv(data_train)

# Boosted Tree

boost_model <- boost_tree(min_n = tune(),
                          mtry = tune(),
                          learn_rate = tune(),
                          mode = "regression") %>%
  set_engine("xgboost")

boost_wflow <-
  workflow() %>%
  add_recipe(recipe) %>%
  add_model(boost_model)

boost_params <- parameters(boost_model) %>%
  update(mtry = mtry(range(c(2,12))))

# define grid

boost_grid <- grid_regular(boost_params,
                           levels=3)

# tuning model

boost_tune <- boost_wflow %>%
  tune_grid(resamples = data_folds,
            grid = boost_grid)

save(boost_tune, file = 'rda/boost_tune.rda')

# boost results for metrics

boost_res <- boost_wflow %>%
  tune_grid(resamples = data_folds,
            grid = boost_grid
  )

save(boost_res, file = 'rda/boost_res.rda')

autoplot(boost_tune)
