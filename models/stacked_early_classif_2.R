# helper packages
library(tidyverse)
library(tidymodels)
library(stacks)


# load and split the early data using imputed data
load('./data/imp')

median_data <- data %>% 
  filter(!is.na(Cutaneous.Leishmaniasis)) %>% 
  filter(Cutaneous.Leishmaniasis > 0) %>% 
  dplyr::select(Cutaneous.Leishmaniasis) 

median <- median_data$Cutaneous.Leishmaniasis %>% 
  median()

data <- data %>%
  filter(Year < 2014) %>%
  filter(!is.na(Cutaneous.Leishmaniasis)) %>% 
  filter(Cutaneous.Leishmaniasis > 0) %>%
  dplyr::select(c('Cutaneous.Leishmaniasis', 'LST_Day', # include LST_Night?
           'OptTemp_Obs', 'NDVI', 'EVI', 'Precip', 
           'StableLights', 'SWOccurrence', 'pland_forest',
           'te_forest', 'enn_mn_forest'))


data$pland_forest <- ifelse(is.na(data$pland_forest), 0, data$pland_forest)
data$te_forest <- ifelse(is.na(data$te_forest), 0, data$te_forest)
data$enn_mn_forest <- ifelse(is.na(data$enn_mn_forest), 0, data$enn_mn_forest)

data$Cutaneous.Leishmaniasis <- as.factor(ifelse(data$Cutaneous.Leishmaniasis < median, 0, 1))

set.seed(123) # for reproducibility

split <- initial_split(data)

data_train <- training(split)
data_test <- testing(split)

# use a 5-fold cross-validation
folds <- rsample::vfold_cv(data_train, v = 5)

# set up a basic recipe
data_rec <-
  recipe(Cutaneous.Leishmaniasis ~ LST_Day + OptTemp_Obs + NDVI + 
           EVI + Precip + StableLights + SWOccurrence + pland_forest + 
           te_forest + enn_mn_forest, data = data_train) %>%
  step_dummy(all_nominal() - all_outcomes()) %>% 
  step_zv(all_predictors())

# define a minimal workflow
data_wflow <-
  workflow() %>% 
  add_recipe(data_rec)

# add metric rmse (same as malaria)
metric <- metric_set(f_meas) #.784 for roc_auc

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
  data_wflow %>%
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
    cost = tune(),
    rbf_sigma = tune()
  ) %>% 
  set_engine('kernlab') %>% 
  set_mode('classification')

# add it to a workflow
svm_wflow <- 
  data_wflow %>% 
  add_model(svm_spec)

# tune cost and rbf_sigma and fit to the 10-fold cv
set.seed(123)
svm_res <-
  tune_grid(
    svm_wflow,
    resamples = folds,
    grid = 5,
    control = ctrl_grid
  )

save(svm_res, file = 'models/stacking/svm_res_early_classif_2')
# load(file = 'models/stacking/svm_res_early_classif_2')

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
  data_wflow %>%
  add_model(xgb_spec)

# tune mtry, trees, min_n, tree_depth, etc.
xgb_res <-
  tune_grid(
    xgb_wflow,
    resamples = folds,
    grid = 5,
    control = ctrl_grid
  )

save(xgb_res, file = 'models/stacking/xgb_res_early_classif_2')

# load(file = 'models/stacking/xgb_res_early_classif_2')

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
  data_wflow %>%
  add_model(rf_spec)

# tune mtry, trees, min_n
rf_res <-
  tune_grid(
    rf_wflow,
    resamples = folds,
    grid = 5,
    control = ctrl_grid
  )

save(rf_res, file = 'models/stacking/rf_res_early_classif_2')

load(file = 'models/stacking/rf_res_early_classif_2')

data_st <- 
  stacks() %>% 
  add_candidates(svm_res) %>% 
  add_candidates(xgb_res) %>% 
  add_candidates(rf_res)

data_st

as_tibble(data_st)

# # A tibble: 23,915 × 31
# Cutaneous.Le…¹ .pred…² .pred…³ .pred…⁴ .pred…⁵ .pred…⁶ .pred…⁷ .pred…⁸ .pred…⁹ .pred…˟ .pred…˟ .pred…˟ .pred…˟ .pred…˟ .pred…˟
# <fct>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#   1 0                0.770   0.769   0.660   0.813   0.764   0.230   0.231   0.340   0.187   0.236   0.823  0.921   0.877   0.895 
# 2 0                0.822   0.787   0.661   0.830   0.776   0.178   0.213   0.339   0.170   0.224   0.889  0.991   0.903   0.941 
# 3 0                0.777   0.779   0.660   0.824   0.770   0.223   0.221   0.340   0.176   0.230   0.866  0.882   0.865   0.893 
# 4 0                0.839   0.793   0.661   0.850   0.785   0.161   0.207   0.339   0.150   0.215   0.932  0.975   0.975   0.977 
# 5 1                0.358   0.557   0.659   0.378   0.558   0.642   0.443   0.341   0.622   0.442   0.472  0.268   0.430   0.488 
# 6 1                0.838   0.774   0.661   0.607   0.770   0.162   0.226   0.339   0.393   0.230   0.383  0.0129  0.0277  0.0552
# 7 0                0.857   0.749   0.662   0.693   0.769   0.143   0.251   0.338   0.307   0.231   0.724  0.992   0.960   0.937 
# 8 0                0.805   0.763   0.662   0.813   0.766   0.195   0.237   0.338   0.187   0.234   0.701  0.999   0.899   0.858 
# 9 0                0.830   0.780   0.662   0.830   0.773   0.170   0.220   0.338   0.170   0.227   0.880  0.998   0.966   0.957 
# 10 1                0.394   0.576   0.659   0.532   0.570   0.606   0.424   0.341   0.468   0.430   0.577  0.327   0.664   0.541 
# # … with 23,905 more rows, 16 more variables: .pred_0_xgb_res_1_5 <dbl>, .pred_1_xgb_res_1_1 <dbl>, .pred_1_xgb_res_1_2 <dbl>,
# #   .pred_1_xgb_res_1_3 <dbl>, .pred_1_xgb_res_1_4 <dbl>, .pred_1_xgb_res_1_5 <dbl>, .pred_0_rf_res_1_4 <dbl>,
# #   .pred_0_rf_res_1_5 <dbl>, .pred_0_rf_res_1_1 <dbl>, .pred_0_rf_res_1_3 <dbl>, .pred_0_rf_res_1_2 <dbl>,
# #   .pred_1_rf_res_1_4 <dbl>, .pred_1_rf_res_1_5 <dbl>, .pred_1_rf_res_1_1 <dbl>, .pred_1_rf_res_1_3 <dbl>,
# #   .pred_1_rf_res_1_2 <dbl>, and abbreviated variable names ¹​Cutaneous.Leishmaniasis, ²​.pred_0_svm_res_1_1,
# #   ³​.pred_0_svm_res_1_3, ⁴​.pred_0_svm_res_1_4, ⁵​.pred_0_svm_res_1_2, ⁶​.pred_0_svm_res_1_5, ⁷​.pred_1_svm_res_1_1,
# #   ⁸​.pred_1_svm_res_1_3, ⁹​.pred_1_svm_res_1_4, ˟​.pred_1_svm_res_1_2, ˟​.pred_1_svm_res_1_5, ˟​.pred_0_xgb_res_1_1, …
# # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# creating a model stack
## ready to evaluate how it is that we need to combine predictions from
## each candidate ensembe member

model_st <-
  data_st %>% 
  blend_predictions()

model_st <- 
  model_st %>% 
  fit_members()

data_test <- 
  data_test %>% 
  bind_cols(predict(model_st, .))

# confusion matrix for stacks
caret::confusionMatrix(data_test$Cutaneous.Leishmaniasis, 
      data_test$.pred_class,
      positive = '1')


# confusion matrix for base models

member_preds <- 
  data_test %>% 
  select(Cutaneous.Leishmaniasis) %>% 
  bind_cols(
    predict(
      model_st,
      data_test,
      members = TRUE
    )
  )

colnames(member_preds) %>% 
  map_dfr(
    .f = accuracy,
    truth = Cutaneous.Leishmaniasis,
    data = member_preds
  ) %>% 
  mutate(member = colnames(member_preds))
