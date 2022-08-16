library(tidyverse)
library(tidymodels)
library(stacks)

wind_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')
wind <-
  wind_raw %>%
  select(
    province_territory, 
    total_project_capacity_mw,
    turbine_rated_capacity_kw = turbine_rated_capacity_k_w,
    rotor_diameter_m,
    hub_height_m,
    year = commissioning_date
  ) %>%
  group_by(province_territory) %>%
  mutate(
    year = as.numeric(year),
    province_territory = case_when(
      n() < 50 ~ "Other",
      TRUE ~ province_territory
    )
  ) %>%
  filter(!is.na(year)) %>%
  ungroup()

# split into training and testing sets
set.seed(1)
wind_split <- initial_split(wind)
wind_train <- training(wind_split)
wind_test  <- testing(wind_split)

# use a 5-fold cross-validation
set.seed(1)
folds <- rsample::vfold_cv(wind_train, v = 5)

# set up a basic recipe
wind_rec <- 
  recipe(turbine_rated_capacity_kw ~ ., data = wind_train) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

# define a minimal workflow
wind_wflow <- 
  workflow() %>% 
  add_recipe(wind_rec)

metric <- metric_set(rmse)

ctrl_grid <- control_stack_grid()
ctrl_res <- control_stack_resamples()

# create a linear model definition
lin_reg_spec <-
  linear_reg() %>%
  set_engine("lm")

# add it to a workflow
lin_reg_wflow <- 
  wind_wflow %>% 
  add_model(lin_reg_spec)

# fit to the 5-fold cv
set.seed(1)
lin_reg_res <- 
  fit_resamples(
    lin_reg_wflow,
    resamples = folds,
    metrics = metric,
    control = ctrl_res
  )

# modify the recipe and use the same linear reg spec
spline_rec <- 
  wind_rec %>%
  step_ns(rotor_diameter_m, deg_free = tune::tune("length"))

# add it to a workflow
spline_wflow <- 
  workflow() %>% 
  add_recipe(spline_rec) %>% 
  add_model(lin_reg_spec)

# tune deg_free and fit to the 5-fold cv
set.seed(1)
spline_res <- 
  tune_grid(
    spline_wflow,
    resamples = folds,
    metrics = metric,
    control = ctrl_grid
  )

# define a model using parsnip
svm_spec <- 
  svm_rbf(
    cost = tune(), 
    rbf_sigma = tune()
  ) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# add it to a workflow
svm_wflow <- 
  wind_wflow %>% 
  add_model(svm_spec)

# tune cost and rbf_sigma and fit to the 5-fold cv
set.seed(1)
svm_res <- 
  tune_grid(
    svm_wflow, 
    resamples = folds, 
    grid = 5,
    control = ctrl_grid
  )