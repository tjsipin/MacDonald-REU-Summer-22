# helper packages
library(tidyverse)
library(tidymodels)
library(stacks)


# load and split the early data using imputed data

aad <- read_csv('models/data/aad.csv')


quantile_aad <- aad %>% 
  filter(!is.na(Cutaneous.Leishmaniasis)) %>% 
  filter(Cutaneous.Leishmaniasis > 0) %>% 
  dplyr::select(Cutaneous.Leishmaniasis)

quantile <- (quantile_aad$Cutaneous.Leishmaniasis %>% 
               quantile(0.4))[[1]] # PARAMETER

aad <- aad %>%
  filter(Year < 2014) %>%
  filter(!is.na(Cutaneous.Leishmaniasis)) %>% 
  filter(Cutaneous.Leishmaniasis > 0) %>%
  dplyr::select(c('Cutaneous.Leishmaniasis', 'LST_Day', # include LST_Night?
                  'NDVI', 'EVI', 'Precip', 
                  'StableLights', 'SWOccurrence', 'pland_forest',
                  'te_forest', 'enn_mn_forest','Population')) %>% 
  subset(!is.na(LST_Day)) %>%
  subset(!is.na(SWOccurrence))


aad$pland_forest <- ifelse(is.na(aad$pland_forest), 0, aad$pland_forest)
aad$te_forest <- ifelse(is.na(aad$te_forest), 0, aad$te_forest)
aad$enn_mn_forest <- ifelse(is.na(aad$enn_mn_forest), 0, aad$enn_mn_forest)

aad$Cutaneous.Leishmaniasis <- as.factor(ifelse(aad$Cutaneous.Leishmaniasis < quantile, 0, 1))

set.seed(123) # for reproducibility


split <- initial_split(aad)

aad_train <- training(split)
aad_test_40 <- testing(split) 

# data_test_40 <-  (aad %>%
#                     select(c(colnames(data_test_40_temp))))[Index_temp,] %>%
#   subset(!is.na(LST_Day)) %>%
#   subset(!is.na(SWOccurrence))


aad_test_40$pland_forest <- ifelse(is.na(aad_test_40$pland_forest), 0, aad_test_40$pland_forest)
aad_test_40$te_forest <- ifelse(is.na(aad_test_40$te_forest), 0, aad_test_40$te_forest)
aad_test_40$enn_mn_forest <- ifelse(is.na(aad_test_40$enn_mn_forest), 0, aad_test_40$enn_mn_forest)


# use a 5-fold cross-validation
folds <- rsample::vfold_cv(aad_train, 
                           v = 5, 
                           strata = Cutaneous.Leishmaniasis)

# set up a basic recipe
aad_rec <-
  recipe(Cutaneous.Leishmaniasis ~ LST_Day + NDVI + 
           EVI + Precip + StableLights + SWOccurrence + pland_forest + 
           te_forest + enn_mn_forest + Population, data = aad_train) %>%
  step_dummy(all_nominal() - all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric())

# define a minimal workflow
aad_wflow <-
  workflow() %>% 
  add_recipe(aad_rec)

# add metric
metric <- metric_set(sensitivity, accuracy)

# save assessment set predictions and workflow used to fit the resamples
ctrl_grid <- control_stack_grid()
ctrl_res <- control_stack_resamples()

# models: SVM, XGBoost, RF
## models to try: logistic regression 

# toy model
log_reg_spec <-
  logistic_reg() %>%
  set_engine('glm')

log_reg_wflow <- 
  aad_wflow %>%
  add_model(log_reg_spec)

set.seed(123)
log_reg_res <-
  fit_resamples(
    log_reg_wflow,
    resamples = folds,
    metrics = metric,
    control = ctrl_res
  )

# define svm model using parsnip
svm_spec <- 
  svm_rbf(
    cost = parsnip::tune(),
    rbf_sigma = parsnip::tune(),
    engine = 'kernlab',
    mode = 'classification'
  ) 

# add it to a workflow
svm_wflow <- 
  aad_wflow %>% 
  add_model(svm_spec)

# tune cost and rbf_sigma and fit to the 5-fold cv
set.seed(123)
svm_res <-
  tune_grid(
    svm_wflow,
    resamples = folds,
    grid = 5,
    control = ctrl_grid
  )

save(svm_res, file = 'models/stacking/svm_res_early_CL_classif_40_nonimp')

# define xgboost model using parsnip

set.seed(123)
xgb_spec <- 
  boost_tree(
    mtry = tune(),
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>% 
  set_engine('xgboost') %>% 
  set_mode('classification')

# add it to a workflow
xgb_wflow <- 
  aad_wflow %>%
  add_model(xgb_spec)

# tune mtry, trees, min_n, tree_depth, etc.
xgb_res <-
  tune_grid(
    xgb_wflow,
    resamples = folds,
    grid = 5,
    control = ctrl_grid
  )

save(xgb_res, file = 'models/stacking/xgb_res_early_CL_classif_40_nonimp')
# 

# define rf model using parsnip

set.seed(123)
rf_spec <- 
  rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) %>% 
  set_engine('ranger') %>% 
  set_mode('classification')

# add it to a workflow
rf_wflow <- 
  aad_wflow %>%
  add_model(rf_spec)

# tune mtry, trees, min_n
rf_res <-
  tune_grid(
    rf_wflow,
    resamples = folds,
    grid = 5,
    control = ctrl_grid
  )


save(rf_res, file = 'models/stacking/rf_res_early_CL_classif_40_nonimp')
# 


library(discrim) # for engine = 'naivebayes' or 'klaR'
library(agua) # for engine = 'h2o'

# define nb model using parsnip
nb_spec <- 
  naive_Bayes(
    mode = 'classification',
    smoothness = tune(),
    Laplace = tune(),
    engine = 'naivebayes'
  )



# add it to a workflow
nb_wflow <- 
  aad_wflow %>%
  add_model(nb_spec)

# tune smoothness and Laplace
nb_res <-
  tune_grid(
    nb_wflow,
    resamples = folds,
    grid = 5,
    control = ctrl_grid
  )

save(nb_res, file = 'models/stacking/nb_res_early_CL_classif_40_nonimp')

load(file = 'models/stacking/svm_res_early_CL_classif_40_nonimp')
load(file = 'models/stacking/xgb_res_early_CL_classif_40_nonimp')
load(file = 'models/stacking/rf_res_early_CL_classif_40_nonimp')
load(file = 'models/stacking/nb_res_early_CL_classif_40_nonimp')


aad_st_40 <- 
  stacks() %>% 
  add_candidates(xgb_res) %>% 
  add_candidates(rf_res) %>% 
  add_candidates(svm_res) %>% 
  add_candidates(nb_res)


save(aad_st_40, file = 'models/stacking/aad_st_early_CL_classif_40_nonimp')
load('models/stacking/aad_st_early_CL_classif_40_nonimp')

# creating a model stack
## ready to evaluate how it is that we need to combine predictions from
## each candidate ensembe member

model_st_40 <-
  aad_st_40 %>% 
  blend_predictions()

model_st_40 <-
  model_st_40 %>% 
  fit_members()

save(model_st_40, file = 'models/stacking/model_st_early_CL_classif_40_nonimp')

load('models/stacking/model_st_early_CL_classif_40_nonimp')

aad_test_40 <-
  aad_test_40 %>%
  bind_cols(predict(model_st_40, .))

save(aad_test_40, file = 'models/stacking/aad_test_early_CL_classif_40_nonimp')


# confusion matrix for stacks
conf_mat_early_CL_classif_40 <- caret::confusionMatrix(data = aad_test_40$.pred_class, 
                                                       reference = aad_test_40$Cutaneous.Leishmaniasis,
                                                       positive = '1')

save(conf_mat_early_CL_classif_40, file = 'models/stacking/conf_mat_early_CL_classif_40_nonimp')

load('models/stacking/conf_mat_early_CL_classif_40_nonimp')
conf_mat_early_CL_classif_40

# confusion matrix for base models

member_preds <- 
  aad_test_40 %>% 
  dplyr::select(Cutaneous.Leishmaniasis) %>% 
  bind_cols(
    predict(
      model_st_40,
      aad_test_40,
      members = TRUE
    )
  )
