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


data = read.csv('models/data/aad.csv')

new_df <- subset(data, !is.na(data$Cutaneous.Leishmaniasis))

# split missing data

early_data <- new_df %>%
  filter(Year < 2014) %>%
  select(-c(29:69)) %>%
  select(-c("AvgRad", "Zika", "Chikungunya"))

later_data <- new_df %>%
  filter(Year >= 2014) %>%
  select(-c(29:69)) %>%
  select(-c("StableLights"))

# categorize
set.seed(123)
cat_df <- early_data
cat_df$Cutaneous.Leishmaniasis <- cut(cat_df$Cutaneous.Leishmaniasis,
                                      breaks = c(-0.0000000001, 0.0000001, 0.09203, 0.87690, 10^3), # -1 instead of 0
                                      # since noninclusive
                                      labels = c("no", "low", "moderate", "high"))

# data split
data_split <- initial_split(cat_df %>% select(c('Population', 
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
                                                'Cutaneous.Leishmaniasis')),
                            # strata = Cutaneous.Leishmaniasis,
                            prop = 0.8)
data_train <- training(data_split) %>% 
  group_by(Cutaneous.Leishmaniasis) %>%
  sample_n(size = 6325) # stratified
data_test <- testing(data_split) # DO NOT STRATIFY

# logistic regression for multinomial case (good for ordinal)

data_train[is.na(data_train)] <- 'Missing'
data_test[is.na(data_test)] <- 'Missing'

data_train <- data.table(data_train)
data_test <- data.table(data_test)


data_train$Cutaneous.Leishmaniasis <- as.integer(data_train$Cutaneous.Leishmaniasis) - 1

data_train <- as.data.frame(lapply(data_train, as.numeric))
# data_train <- as.data.table(lapply(data_train, as.numeric))

data_test$Cutaneous.Leishmaniasis <- as.integer(data_test$Cutaneous.Leishmaniasis) - 1

data_test <- as.data.frame(lapply(data_test, as.numeric))
# data_test <- as.data.table(lapply(data_test, as.numeric))


# another split

# split training into predictors and labels
# x_train <- as.matrix(data_train[!(colnames(data_train) == "Cutaneous.Leishmaniasis")])
x_train <- as.matrix(data_train %>%
                       select(-c("Cutaneous.Leishmaniasis")))

y_train <- as.matrix(data_train %>%
                       select(Cutaneous.Leishmaniasis))

x_test <- as.matrix(data_test %>%
                      select(-c("Cutaneous.Leishmaniasis")))

x_test[,1:13] = as.matrix(x_test[,1:13])

y_test <- as.matrix(data_test %>%
                      select(Cutaneous.Leishmaniasis))

# Preparing matrix
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# Default parameters
params <- list(booster = 'gbtree',
               objective = 'multi:softmax',
               eta = 0.3,
               gamma = 0,
               max_depth = 6,
               min_child_weight = 1,
               subsample = 1,
               colsample_bytree = 1,
               "num_class" = 4)

# Set up CV
xgbcv <- xgb.cv(params = params,
                data = dtrain,
                nrounds = 200,
                nfold = 5,
                showsd = T,
                stratified = T,
                print_every_n = 10,
                early_stopping_rounds = 20,
                maximize = F,
                verbose = 2,
                eval_metric = 'merror')


# first default - model training
xgb2 <- xgb.train(params = params,
                  data = dtrain,
                  nrounds = 200, # xgbcv$best_iteration,
                  watchlist = list(val = dtest, train = dtrain),
                  print_every_n = 10,
                  early_stopping_rounds = 10,
                  maximize = F,
                  eval_metric = 'merror')

# model prediction
xgbpred <- predict(xgb2, dtest)


# confusion matrix
xgbpred = as.factor(xgbpred)
y_test = as.factor(y_test)

library(caret)
confusionMatrix(xgbpred, y_test)

# # view var imp plot
mat <- xgb.importance(feature_names = colnames(x_train), model = xgb2)
xgb.plot.importance(importance_matrix = mat[1:nrow(mat)])

# Create tasks
data_train$Cutaneous.Leishmaniasis <- as.factor(data_train$Cutaneous.Leishmaniasis)
data_test$Cutaneous.Leishmaniasis <- as.factor(data_test$Cutaneous.Leishmaniasis)

traintask <- makeClassifTask(data = data_train, target = 'Cutaneous.Leishmaniasis')
testtask <- makeClassifTask(data = data_test, target = 'Cutaneous.Leishmaniasis')

# create learner
lrn <- makeLearner('classif.xgboost', 
                   predict.type = 'response')
lrn$par.vals <- list(objective = 'multi:softprob',
                     eval_metric = 'auc',
                     nrounds = 100)

# set parameter space
params <- makeParamSet(
  makeDiscreteParam('booster',
                    values = c('gbtree', 'gblinear', 'dart')),
  makeIntegerParam('max_depth', 
                   lower = 3L, 
                   upper = 12L),
  makeNumericParam('min_child_weight',
                   lower = 6L,
                   upper = 12L),
  makeNumericParam('subsample',
                   lower = 0.4,
                   upper = 1),
  makeNumericParam('colsample_bytree',
                   lower = 0.4, 
                   upper = 1),
  makeIntegerParam('gamma',
                   lower = 0,
                   upper = 10),
  makeNumericParam('eta',
                   lower = 0.01,
                   upper = 0.2)
)

# set resampling strategy
rdesc <- makeResampleDesc("CV", 
                          stratify = T, 
                          iters = 5L)

# search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

# set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

# parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask,
                     resampling = rdesc,
                     measures = acc,
                     par.set = params,
                     control = ctrl,
                     show.info = F)

mytune$y



# set hyperparameters
lrn_tune <- setHyperPars(lrn, par.vals = mytune$x)

# train model
xgmodel <- train(learner = lrn_tune, task = traintask)

# predict model
xgpred <- predict(xgmodel, testtask)

# confusion matrix
#eta01 <- confusionMatrix(xgpred$data$response, xgpred$data$truth)
# eta01
# #eta08 <- confusionMatrix(xgpred$data$response, xgpred$data$truth)
# eeta08
# #eta055 <- confusionMatrix(xgpred$data$response, xgpred$data$truth)
# eta055
# #eta09 <- confusionMatrix(xgpred$data$response, xgpred$data$truth)
# eta09
# eta03 <- confusionMatrix(xgpred$data$response, xgpred$data$truth)
# eta03
confusionMatrix(xgpred$data$response, xgpred$data$truth)
mytune$opt.path$env$path

# 
# # plot 
# x = 1:length(y_test)
# 
# ggplot() + 
#   geom_line(aes(x = xgpred$data$id,
#                 y = xgpred$data$truth),
#             color = 'red') + 
#   geom_line(aes(x = xgpred$data$id,
#                 y = xgpred$data$response),
#             color = 'blue') + 
#   labs(color = "Truth vs. Response")
# 
# ggplot() + 
#   geom_line(aes(x = sort(xgpred$data$id),
#                 y = sort(xgpred$data$truth)),
#             color = 'red') + 
#   geom_line(aes(x = sort(xgpred$data$id),
#                 y = sort(xgpred$data$response)),
#             color = 'blue') + 
#   labs(color = "Truth vs. Response")
# 
# 


# params <- list(booster = 'gbtree',
#                objective = 'multi:softmax',
#                eta = 0.3,
#                gamma = 0,
#                max_depth = 3,
#                min_child_weight = 0.2499749,
#                subsample = 1,
#                colsample_bytree = 0.67,
#                "num_class" = 3)
