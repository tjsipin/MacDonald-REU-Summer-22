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
library(data.table)
library(mlr)


aad <- read.csv("Annual_Amazon_Data.csv")
view(aad)

cutaneous <- aad$Cutaneous.Leishmaniasis
mucosal <- aad$Mucosal.Leishmaniasis
visceral <- aad$Visceral.Leishmaniasis
new_data <- subset(aad, !is.na(cutaneous))
#View(new_data)
library(tidyverse)
library(dplyr)
#names(new_data)

## splitting the data into a before 2014 set and an after 2014 set

early_data <- new_data %>%
  filter(Year < 2014)%>%
  select(-c(29:69))
later_data <- new_data %>%
  filter(Year > 2013) %>%
  select(-c(29:69))

#names(early_data)

## removing unnecessary variables

early_data <- early_data %>%
  select(-c("AvgRad"))
later_data <- later_data %>%
  select(-c("StableLights")) %>%
  mutate(later_data$OptTemp_Obs <- as.numeric(later_data$OptTemp_Obs)) %>%
  mutate(later_data$Year <- as.numeric(later_data$Year)) %>%
  mutate(later_data$Population <- as.numeric(later_data$Population))


later_data_small <- later_data %>%
  select(c("Cutaneous.Leishmaniasis", "LST_Day", "OptTemp_Obs", "NDVI", "EVI", "Precip", "AvgRad", "SWOccurrence")) %>%
  na.omit(later_data_small)
later_data_t <- later_data_small %>%
  mutate(Cutaneous.Leishmaniasis = (Cutaneous.Leishmaniasis))

# splitting data into training and test data
set.seed(2)
library(caTools)
split <- sample.split(later_data_t, SplitRatio = 0.7)
split
train <- subset(later_data_t, split = "TRUE")
test <- subset(later_data_t, split = "FALSE")
train
test

# one-hot encode categorical variables
sparse_matrix <- sparse.model.matrix(
  Cutaneous.Leishmaniasis ~ . - 1, data = train
) # use for data = ?


# split training into predictors and labels
x_train <- as.matrix(train %>%
                       select(-c("Cutaneous.Leishmaniasis")))

x_train[,1:7] = as.numeric(x_train[,1:7])

y_train <- as.matrix(train %>%
                       select(Cutaneous.Leishmaniasis))

x_test <- as.matrix(test %>%
                      select(-c("Cutaneous.Leishmaniasis")))

x_test[,1:7] = as.numeric(x_test[,1:7])

y_test <- as.matrix(test %>%
                      select(Cutaneous.Leishmaniasis))

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# Basic training

## Parameters
params <- list(booster = "gbtree", 
               objective = "reg:squarederror",
               eta = 0.3,
               gamma = 20,
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
                   early.stop.round = 10, 
                   maximize = F , 
                   eval_metric = "error")

# model prediction
xgbpred <- predict(xgb1, dtest)

# var imp plot
mat <- xgb.importance(feature_names = colnames(x_train),
                      model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:ncol(x_train)])

# Accuracy check

mse = mean((y_test - pred)^2)
mae = caret::MAE(y_test, pred)
rmse = caret::RMSE(y_test, pred)

cat("MSE: ", mse, "MAE: ", mae, "RMSE: ", rmse)


# Create tasks
traintask <- makeRegrTask(data = train, target = "Cutaneous.Leishmaniasis")
testtask <- makeRegrTask(data = test, target = "Cutaneous.Leishmaniasis")

# Create learner
lrn <- makeLearner("regr.xgboost", predict.type = "response")
lrn$par.vals <- list(objective = 'reg:squarederror',
                     eval_metric = 'error',
                     nrounds = 200L,
                     eta = 0.3)

# Set parameter space
set.seed(2)
params <- makeParamSet(makeDiscreteParam('booster',
                                         values = c('gbtree', 'gblinear')),
                       makeIntegerParam('max_depth',
                                        lower = 1L,
                                        upper = 100L),
                       makeNumericParam('min_child_weight',
                                        lower = 0L,
                                        upper = 30),
                       makeNumericParam('subsample',
                                        lower = 0,
                                        upper = 2),
                       makeNumericParam('colsample_bytree',
                                        lower = 0,
                                        upper = 2),
                       makeIntegerParam('gamma',
                                        lower = 1L,
                                        upper = 100L))


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
            color = 'red') + 
  geom_line(aes(x = xgpred$data$id,
                y = xgpred$data$response),
            color = 'blue') + 
  labs(color = "Truth vs. Response")

