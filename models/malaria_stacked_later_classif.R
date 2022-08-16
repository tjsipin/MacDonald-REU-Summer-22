# helper packages
library(tidyverse)
library(tidymodels)
library(stacks)


# load and split the early data using imputed data
aad <- read.csv("models/data/aad.csv")
aad <- subset(aad, !is.na(aad$Malaria))
aad <-subset(aad, aad$Malaria > 0)
summary(aad$Malaria)



install.packages("caret")


library(caret)


# Extracting Later Data and Tidying it:

## splitting the data into a before 2014 set and an after 2014 set
library(dplyr)
early_data <- aad %>%
  filter(Year < 2014)
later_data <- aad %>%
  filter(Year > 2013)

#names(early_data)

## removing unnecessary variables

early_data <- early_data %>%
  select(-c("AvgRad"))
later_data <- later_data %>%
  select(-c("StableLights"))

# later_data$OptTemp_Obs <- as.numeric(later_data$OptTemp_Obs)
# later_data$Year <- as.numeric(later_data$Year)
# later_data$Population <- as.numeric(later_data$Population)

later_data_small <- later_data %>%
  select(c("Population", "Malaria", "LST_Day", "Precip", "AvgRad", 
           "SWOccurrence", "NDVI", "EVI", "pland_forest", "te_forest", "enn_mn_forest"))
later_data_small <- subset(later_data_small, !is.na(later_data_small$LST_Day))
later_data_small <- subset(later_data_small, !is.na(later_data_small$SWOccurrence))
later_data_small$pland_forest <- ifelse(is.na(later_data_small$pland_forest), 
                                        0, later_data_small$pland_forest)
later_data_small$te_forest <- ifelse(is.na(later_data_small$te_forest), 
                                     0, later_data_small$te_forest)
later_data_small$enn_mn_forest <- ifelse(is.na(later_data_small$enn_mn_forest), 
                                         0, later_data_small$enn_mn_forest)


library(dplyr)
summary(later_data_small$Malaria)
cat_df <- later_data_small
cat_df$Malaria <- cut(later_data_small$Malaria, 
                                      breaks = c(0, 2.7879048, 10^3), 
                                      labels = c("low", "high")) # median


skimr::skim(cat_df)
round(prop.table(table(cat_df$Malaria)), 2)

set.seed(123) # for reproducibility

split <- initial_split(cat_df)

data_train <- training(split)
data_test <- testing(split)

# use a 5-fold cross-validation
folds <- rsample::vfold_cv(data_train, v = 5)

# set up a basic recipe
data_rec <-
  recipe(Malaria ~ Population + LST_Day + NDVI + 
           EVI + Precip + AvgRad + SWOccurrence + pland_forest + 
           te_forest + enn_mn_forest, data = data_train) %>%
  step_dummy(all_nominal() - all_outcomes()) %>% 
  step_zv(all_predictors())

# define a minimal workflow
data_wflow <-
  workflow() %>% 
  add_recipe(data_rec)

# add metric rmse (same as malaria)
metric <- metric_set(f_meas) #.784 for roc_auc

library(stacks)

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

save(svm_res, file = 'models/stacking/svm_res_later_classif_2')
# load(file = 'models/stacking/svm_res_later_classif_2')

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

save(xgb_res, file = 'models/stacking/xgb_res_later_classif_2')

# load(file = 'models/stacking/xgb_res_later_classif_2')

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

save(rf_res, file = 'models/stacking/rf_res_later_classif_2')

load(file = 'models/stacking/rf_res_later_classif_2')

data_st <- 
  stacks() %>% 
  add_candidates(svm_res) %>% 
  add_candidates(xgb_res) %>% 
  add_candidates(rf_res)

data_st

as_tibble(data_st)

# # A tibble: 10,225 × 31
# Cutaneous…¹ .pred…² .pred…³ .pred…⁴ .pred…⁵ .pred…⁶ .pred…⁷ .pred…⁸ .pred…⁹ .pred…˟ .pred…˟ .pred…˟
# <fct>         <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#   1 low           0.321  0.299    0.508   0.322  0.295    0.679   0.701   0.492  0.678    0.705  0.549 
# 2 high          0.269  0.246    0.508   0.287  0.244    0.731   0.754   0.492  0.713    0.756  0.372 
# 3 high          0.463  0.537    0.510   0.687  0.509    0.537   0.463   0.490  0.313    0.491  0.651 
# 4 low           0.568  0.594    0.508   0.767  0.553    0.432   0.406   0.492  0.233    0.447  0.670 
# 5 high          0.408  0.420    0.510   0.363  0.409    0.592   0.580   0.490  0.637    0.591  0.0234
# 6 high          0.125  0.0433   0.511   0.174  0.0509   0.875   0.957   0.489  0.826    0.949  0.187 
# 7 low           0.468  0.478    0.508   0.778  0.447    0.532   0.522   0.492  0.222    0.553  0.836 
# 8 low           0.607  0.641    0.508   0.971  0.595    0.393   0.359   0.492  0.0287   0.405  0.978 
# 9 high          0.468  0.521    0.510   0.335  0.517    0.532   0.479   0.490  0.665    0.483  0.337 
# 10 high          0.503  0.591    0.511   0.718  0.558    0.497   0.409   0.489  0.282    0.442  0.0384
# # … with 10,215 more rows, 19 more variables: .pred_low_xgb_res_1_1 <dbl>,
# #   .pred_low_xgb_res_1_3 <dbl>, .pred_low_xgb_res_1_4 <dbl>, .pred_low_xgb_res_1_5 <dbl>,
# #   .pred_high_xgb_res_1_2 <dbl>, .pred_high_xgb_res_1_1 <dbl>, .pred_high_xgb_res_1_3 <dbl>,
# #   .pred_high_xgb_res_1_4 <dbl>, .pred_high_xgb_res_1_5 <dbl>, .pred_low_rf_res_1_4 <dbl>,
# #   .pred_low_rf_res_1_5 <dbl>, .pred_low_rf_res_1_1 <dbl>, .pred_low_rf_res_1_3 <dbl>,
# #   .pred_low_rf_res_1_2 <dbl>, .pred_high_rf_res_1_4 <dbl>, .pred_high_rf_res_1_5 <dbl>,
# #   .pred_high_rf_res_1_1 <dbl>, .pred_high_rf_res_1_3 <dbl>, .pred_high_rf_res_1_2 <dbl>, and …
# # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
# # creating a model stack
# ## ready to evaluate how it is that we need to combine predictions from
# ## each candidate ensembe member

model_st <-
  data_st %>% 
  blend_predictions()

model_st <- 
  model_st %>% 
  fit_members()

data_test70 <- 
  data_test %>% 
  bind_cols(predict(model_st, .))

# confusion matrix for stacks
caret::confusionMatrix(data_test60$Malaria, 
                       data_test60$.pred_class,
                       positive = 'high')

plot(data_test60$Malaria, data_test60$.pred_class)


## fit a logistic regression to the data...
glm.fit30=glm(data_test30$.pred_class ~ data_test30$Malaria, family=binomial)
lines(data_test30$Malaria, glm.fit$fitted.values)

glm.fit40=glm(data_test40$.pred_class ~ data_test40$Malaria, family=binomial)
lines(data_test30$Malaria, glm.fit$fitted.values)

glm.fit50=glm(data_test50$.pred_class ~ data_test50$Malaria, family=binomial)
lines(data_test30$Malaria, glm.fit$fitted.values)

glm.fit60=glm(data_test60$.pred_class ~ data_test60$Malaria, family=binomial)
lines(data_test30$Malaria, glm.fit$fitted.values)

glm.fit70=glm(data_test70$.pred_class ~ data_test70$Malaria, family=binomial)
lines(data_test30$Malaria, glm.fit$fitted.values)


roc(data_test30$.pred_class, glm.fit30$fitted.values, plot=TRUE)

## Now let's configure R so that it prints the graph as a square.
##
par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region
roc(data_test30$.pred_class, glm.fit30$fitted.values, plot=TRUE)


roc(data_test30$.pred_class, glm.fit30$fitted.values, plot=TRUE, legacy.axes=TRUE)


roc(data_test30$.pred_class, glm.fit30$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")

## If we want to find out the optimal threshold we can store the 
## data used to make the ROC graph in a variable...
roc.info <- roc(data_test30$.pred_class, glm.fit30$fitted.values, legacy.axes=TRUE)
str(roc.info)


## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "obese".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "not obese". 
## Thus, TPP = 0% and FPP = 0%

roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

## We can calculate the area under the curve...
roc(data_test30$.pred_class, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

## ...and the partial area under the curve.
roc(data_test30$.pred_class, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(60, 40), auc.polygon = TRUE, auc.polygon.col = "#377eb822")


## We can calculate the area under the curve...
roc(data_test30$.pred_class, glm.fit30$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

roc(data_test40$.pred_class, glm.fit40$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="green", lwd=4, print.auc=TRUE)

roc(data_test50$.pred_class, glm.fit50$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="red", lwd=4, print.auc=TRUE)

roc(data_test60$.pred_class, glm.fit60$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="purple", lwd=4, print.auc=TRUE)

roc(data_test70$.pred_class, glm.fit70$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="orange", lwd=4, print.auc=TRUE)

## Now let's fit the data with a random forest...
##
#######################################
rf.model <- randomForest(factor(data_test30$.pred_class) ~ data_test30$Malaria)

## ROC for random forest
roc(data_test30$.pred_class, rf.model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)


## Now layer logistic regression and random forest ROC graphs..
##
#######################################
roc(data_test30$.pred_class, glm.fit30$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.y=80)

plot.roc(data_test40$.pred_class, glm.fit40$fitted.values, percent=TRUE, col="green", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=75)
plot.roc(data_test50$.pred_class, glm.fit50$fitted.values, percent=TRUE, col="red", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=70)
plot.roc(data_test60$.pred_class, glm.fit60$fitted.values, percent=TRUE, col="purple", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=65)
plot.roc(data_test70$.pred_class, glm.fit70$fitted.values, percent=TRUE, col="orange", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=60)
legend("bottomright", legend=c("30th Percentile", "40th Percentile", "50th Percentile", "60th Percentile", "70th Percentile"), col=c("#377eb8", "green", "red", "purple","orange"), lwd=4)


# confusion matrix for base models

member_preds <- 
  data_test %>% 
  select(Malaria) %>% 
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
    truth = Malaria,
    data = member_preds
  ) %>% 
  mutate(member = colnames(member_preds))


# # A tibble: 5 × 4
# .metric  .estimator .estimate member                 
# <chr>    <chr>          <dbl> <chr>                  
#   1 accuracy binary         1     Malaria                
# 2 accuracy binary         0.788 .pred_class            
# 3 accuracy binary         0.615 .pred_class_svm_res_1_3
# 4 accuracy binary         0.769 .pred_class_xgb_res_1_5
# 5 accuracy binary         0.788 .pred_class_rf_res_1_5 
