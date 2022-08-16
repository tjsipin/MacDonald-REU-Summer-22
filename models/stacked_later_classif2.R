# helper packages
library(tidyverse)
library(tidymodels)
library(stacks)
library(dplyr)
library(pROC)

# load and split imputed data
getwd()
load("./data/imp")

data <- data %>%
  filter(Year > 2013) %>%
  filter(Year < 2019) %>%
  filter(!is.na(Cutaneous.Leishmaniasis)) %>%
  filter(Cutaneous.Leishmaniasis > 0) %>%
  dplyr::select(c("Population", "Cutaneous.Leishmaniasis", "LST_Day", "Precip", "AvgRad", "SWOccurrence", "NDVI", "EVI", "pland_forest", "te_forest", "enn_mn_forest"))

data$pland_forest <- ifelse(is.na(data$pland_forest), 0, data$pland_forest)
data$te_forest <- ifelse(is.na(data$te_forest), 0, data$te_forest)
data$enn_mn_forest <- ifelse(is.na(data$enn_mn_forest), 0, data$enn_mn_forest)

library(dplyr)
summary(data$Cutaneous.Leishmaniasis)
cat_df <- data
cat_df$Cutaneous.Leishmaniasis <- cut(cat_df$Cutaneous.Leishmaniasis, 
                                      breaks = c(0, 0.1170344 , 10^3), 
                                      labels = c("low", "high")) # 30th percentile


skimr::skim(cat_df)
round(prop.table(table(cat_df$Cutaneous.Leishmaniasis)), 2)

set.seed(123) # for reproducibility

split <- initial_split(cat_df)

data_train <- training(split)
data_test <- testing(split)

# use a 5-fold cross-validation
folds <- rsample::vfold_cv(data_train, v = 5)

# set up a basic recipe
data_rec <-
  recipe(Cutaneous.Leishmaniasis ~ Population + LST_Day + NDVI + 
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

# load(file = 'models/stacking/rf_res_later_classif_2')

data_st <- 
  stacks() %>% 
  add_candidates(svm_res) %>% 
  add_candidates(xgb_res) %>% 
  add_candidates(rf_res) 

data_st

as_tibble(data_st)

# # A tibble: 10,807 × 31
# Cutaneous…¹ .pred…² .pred…³ .pred…⁴ .pred…⁵ .pred…⁶ .pred…⁷ .pred…⁸ .pred…⁹ .pred…˟ .pred…˟ .pred…˟
# <fct>         <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#   1 1             0.816   0.744   0.489   0.218   0.752   0.184   0.256   0.511   0.782   0.248   0.313
# 2 1             0.737   0.705   0.495   0.284   0.734   0.263   0.295   0.505   0.716   0.266   0.507
# 3 0             0.897   0.839   0.489   0.666   0.828   0.103   0.161   0.511   0.334   0.172   0.642
# 4 1             0.867   0.870   0.491   0.752   0.855   0.133   0.130   0.509   0.248   0.145   0.754
# 5 0             0.822   0.835   0.495   0.701   0.836   0.178   0.165   0.505   0.299   0.164   0.651
# 6 0             0.846   0.747   0.489   0.556   0.742   0.154   0.253   0.511   0.444   0.258   0.686
# 7 1             0.833   0.705   0.489   0.338   0.722   0.167   0.295   0.511   0.662   0.278   0.543
# 8 0             0.859   0.810   0.491   0.328   0.847   0.141   0.190   0.509   0.672   0.153   0.932
# 9 1             0.359   0.227   0.491   0.237   0.252   0.641   0.773   0.509   0.763   0.748   0.410
# 10 1             0.465   0.293   0.489   0.109   0.331   0.535   0.707   0.511   0.891   0.669   0.108
# # … with 10,797 more rows, 19 more variables: .pred_0_xgb_res_1_2 <dbl>, .pred_0_xgb_res_1_3 <dbl>,
# #   .pred_0_xgb_res_1_4 <dbl>, .pred_0_xgb_res_1_5 <dbl>, .pred_1_xgb_res_1_1 <dbl>,
# #   .pred_1_xgb_res_1_2 <dbl>, .pred_1_xgb_res_1_3 <dbl>, .pred_1_xgb_res_1_4 <dbl>,
# #   .pred_1_xgb_res_1_5 <dbl>, .pred_0_rf_res_1_4 <dbl>, .pred_0_rf_res_1_5 <dbl>,
# #   .pred_0_rf_res_1_1 <dbl>, .pred_0_rf_res_1_3 <dbl>, .pred_0_rf_res_1_2 <dbl>,
# #   .pred_1_rf_res_1_4 <dbl>, .pred_1_rf_res_1_5 <dbl>, .pred_1_rf_res_1_1 <dbl>,
# #   .pred_1_rf_res_1_3 <dbl>, .pred_1_rf_res_1_2 <dbl>, and abbreviated variable names …
# # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

model_st <-
  data_st %>% 
  blend_predictions()

model_st <- 
  model_st %>% 
  fit_members()

data_test <- 
  data_test %>% 
  bind_cols(predict(model_st, .))

# data_test$.pred_class = ifelse (
#   data_test$.pred_low < 0.525, "high", "low"
# ) %>% as.factor()

# confusion matrix for stacks
caret::confusionMatrix(data_test$Cutaneous.Leishmaniasis, 
                       data_test$.pred_class,
                       positive = 'high')

plot(data_test$Cutaneous.Leishmaniasis, data_test$.pred_class)


## fit a logistic regression to the data...
glm.fit30=glm(data_test30$.pred_class ~ data_test30$Cutaneous.Leishmaniasis, family=binomial)
lines(data_test30$Cutaneous.Leishmaniasis, glm.fit$fitted.values)

glm.fit40=glm(data_test40$.pred_class ~ data_test40$Cutaneous.Leishmaniasis, family=binomial)
lines(data_test30$Cutaneous.Leishmaniasis, glm.fit$fitted.values)

glm.fit50=glm(data_test50$.pred_class ~ data_test50$Cutaneous.Leishmaniasis, family=binomial)
lines(data_test30$Cutaneous.Leishmaniasis, glm.fit$fitted.values)

glm.fit60=glm(data_test60$.pred_class ~ data_test60$Cutaneous.Leishmaniasis, family=binomial)
lines(data_test30$Cutaneous.Leishmaniasis, glm.fit$fitted.values)

glm.fit70=glm(data_test70$.pred_class ~ data_test70$Cutaneous.Leishmaniasis, family=binomial)
lines(data_test30$Cutaneous.Leishmaniasis, glm.fit$fitted.values)


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
rf.model <- randomForest(factor(data_test30$.pred_class) ~ data_test30$Cutaneous.Leishmaniasis)

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
  select(Cutaneous.Leishmaniasis) %>% 
  bind_cols(
    predict(
      model_st,
      data_test,
      members = TRUE
    )
  )

# member_preds$.pred_class = ifelse (
#   member_preds$.pred_low < 0.5, "high", "low"
# ) %>% as.factor()

colnames(member_preds) %>% 
  map_dfr(
    .f = accuracy,
    truth = Cutaneous.Leishmaniasis,
    data = member_preds
  ) %>% 
  mutate(member = colnames(member_preds))

