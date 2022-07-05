# packages
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

library(tidyverse)
library(dplyr)
library(glmnet)
library(xgboost)
library(ranger)
library(lsr)
library(corrr)
library(tidyr)
library(car)
library(moments)
library(ggpubr)
library(BBmisc)
library(rsample)
library(recipes)
library(randomForest)
library(parsnip)
library(workflows)
library(tune)
library(dials)
library(yardstick)
require(xgboost)
require(data.table)
require(Matrix)
library(caret)
library(mlr)
# split data into training and testing


early_data <- new_df %>%
  filter(Year < 2014) %>%
  dplyr::select(-c(29:69)) %>%
  dplyr::select(-c("AvgRad", "Zika", "Chikungunya")) %>%
  dplyr::select(c('Population', 
           'LST_Day', 
           'LST_Night',
           'OptTemp_Obs',
           'Dengue_Alb_OptTemp',
           'Dengue_Aeg_OptTemp',
           'Chik_Alb_OptTemp',
           'Zika_OptTemp',
           'Malaria_OptTemp',
           'NDVI',
           'EVI', 
           'Precip', 
           'StableLights',
           'Cutaneous.Leishmaniasis',
           'SWOccurrence')) %>%
  filter(Cutaneous.Leishmaniasis > 0)

early_data$CL_bins <- cut(early_data$Cutaneous.Leishmaniasis,
    breaks = c(0.0000001, 0.09203, 0.87690, 10^3), # -1 instead of 0
    # since noninclusive
    labels = c("low", "moderate", "high"))

early_data <- early_data %>%
  group_by(CL_bins) %>%
  sample_n(size = 5930) %>% 
  ungroup() %>%
  dplyr::select(-c('CL_bins'))

set.seed(321)
data_split <- initial_split(early_data,
                            strata = Cutaneous.Leishmaniasis)
data_train <- training(data_split) 

data_test <- testing(data_split)

data_train <- data.table(data_train)
data_test <- data.table(data_test)

# one-hot encode categorical variables
sparse_matrix <- sparse.model.matrix(
  Cutaneous.Leishmaniasis ~ . - 1, data = data_train
  ) # use for data = ?


# split training into predictors and labels
x_train <- as.matrix(data_train %>%
  dplyr::select(-c("Cutaneous.Leishmaniasis")))

x_train[,1:13] = as.numeric(x_train[,1:13])

y_train <- as.matrix(data_train %>%
  dplyr::select(Cutaneous.Leishmaniasis))


x_test <- as.matrix(data_test %>%
                       dplyr::select(-c("Cutaneous.Leishmaniasis")))

x_test[,1:13] = as.numeric(x_test[,1:13])

y_test <- as.matrix(data_test %>%
                       dplyr::select(Cutaneous.Leishmaniasis))

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# Basic training

## Parameters
params <- list(booster = "gbtree", 
               objective = "reg:squarederror",
               eta = 0.3,
               gamma = 100,
               max.depth = 1, 
               min_child_weight = 2,
               subsample = 1,
               colsample_bytree = 1,
               lambda = 0
               )

xgbcv <- xgb.cv(params = params,
                data = x_train, 
                label = y_train, 
                nrounds = 1000,
                nfold = 5,
                showsd = T,
                stratified = T,
                print_every_n = 10,
                early_stopping_rounds = 20,
                maximize = F,
                verbose = 2)  # eta = 1: train-rmse:0.000601 @ [41]
                              # eta = 0.3: train-rsme:0.001090 @ [132]
                              # min_child_weight = 1: 0.530765
xgbcv$best_iteration

# first default - model training
xgb1 <- xgb.train (params = params, 
                   data = dtrain, 
                   nrounds = xgbcv$best_iteration, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print.every.n = 10, 
                   early_stopping_rounds = 10, 
                   maximize = F , 
                   eval_metric = "error")

# model prediction
xgbpred <- predict(xgb1, dtest)


ggplot() + 
  geom_line(aes(x = xgpred$data$id,
                y = xgpred$data$truth),
            color = 'red',
            size = 1) + 
  geom_line(aes(x = xgpred$data$id,
                y = xgpred$data$response),
            color = 'blue',
            size = 1,
            alpha = 0.7) + 
  labs(color = "Truth vs. Response")

# var imp plot
mat <- xgb.importance(feature_names = colnames(x_train),
                      model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:ncol(x_train)])

# Accuracy check

mse = mean((y_test - xgbpred)^2)
mae = caret::MAE(y_test, xgbpred)
rmse = caret::RMSE(y_test, xgbpred)

cat("MSE: ", mse, "MAE: ", mae, "RMSE: ", rmse)


# Create tasks
traintask <- makeRegrTask(data = data_train, target = "Cutaneous.Leishmaniasis")
testtask <- makeRegrTask(data = data_test, target = "Cutaneous.Leishmaniasis")

# Create learner
lrn <- makeLearner("regr.xgboost", predict.type = "response")
lrn$par.vals <- list(objective = 'reg:squarederror',
                     eval_metric = 'error',
                     nrounds = 200L
                     )

# Set parameter space
params <- makeParamSet(makeDiscreteParam('booster',
                                         values = c('gbtree', 'gblinear')),
                       makeIntegerParam('max_depth',
                                        lower = 1L,
                                        upper = 10L),
                       makeNumericParam('min_child_weight',
                                        lower = 70L,
                                        upper = 100L),
                       makeNumericParam('subsample',
                                        lower = 0.5,
                                        upper = 1),
                       makeNumericParam('colsample_bytree',
                                        lower = 0.7,
                                        upper = 0.9),
                       makeNumericParam('gamma',
                                        lower = 1L,
                                        upper = 10L),
                       makeNumericParam('eta',
                                        lower = 0.01,
                                        upper = 0.2))


# Set resampling strategy
rdesc <- makeResampleDesc('CV',
                          stratify = F,
                          iters = 5L)

# Search Strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

# Set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

# Parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask,
                     resampling = rdesc,
                     measures = getDefaultMeasure('regr'),
                     par.set = params,
                     control = ctrl,
                     show.info = T)

mytune$y
mytune$x
mytune$opt.path$env$path

# Set hyperparameters
lrn_tune <- setHyperPars(lrn, par.vals = mytune$x)

# Train model
xgmodel <- train(learner = lrn_tune, task = traintask)

# Predict model
xgpred <- predict(xgmodel, testtask)



## Visualize y original test and y predicted data in plot
x = 1:length(y_test)
performance_plot <- plot(x, y_test, col = "red", type = 'l') +
  lines(x, xgpred$data$response, col = "blue", type = 'l')
performance_plot

ggplot() + 
  geom_line(aes(x = xgpred$data$id,
                y = xgpred$data$truth),
            color = 'red',
            size = 0.5) + 
  geom_line(aes(x = xgpred$data$id,
                y = xgpred$data$response),
            color = 'blue',
            size = 0.5,
            alpha = 0.7) + 
  labs(color = "Truth vs. Response")

ggplot() + 
  geom_line(aes(x = xgpred$data$id,
                y = xgpred$data$truth - xgpred$data$response),
            color = 'darkgreen') +  
  labs(color = "Truth vs. Response")

ggplot() +
  geom_line(aes(x = sort(xgpred$data$id),
                y = sort(xgpred$data$truth)),
            color = 'red') +
  geom_line(aes(x = sort(xgpred$data$id),
                y = sort(xgpred$data$response)),
            color = 'blue') +
  labs(color = "Truth vs. Response")

