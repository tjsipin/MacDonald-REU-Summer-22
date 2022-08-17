# helper packages
library(rsample)  # for creating our train-test splits
library(recipes)  # for minor feature engineering tasks
library(dplyr)
library(caret)
library(kernlab)
library(ROCR)
library(e1071)



# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zahradnik/2/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()
# load and split the early data using imputed data
load('./data/imp')

median <- 0.54770 # Lyndsey's median

data <- data %>%
  filter(Year < 2014) %>%
  filter(!is.na(Cutaneous.Leishmaniasis)) %>% 
  filter(Cutaneous.Leishmaniasis > 0) %>%
  select(c('Cutaneous.Leishmaniasis', 'LST_Day', # include LST_Night?
           'OptTemp_Obs', 'NDVI', 'EVI', 'Precip', 
           'StableLights', 'SWOccurrence', 'pland_forest',
           'te_forest', 'enn_mn_forest'))

data$Cutaneous.Leishmaniasis <- as.factor(ifelse(data$Cutaneous.Leishmaniasis < median, 0, 1))

set.seed(123) # for reproducibility

split <- initial_split(data, strata = Cutaneous.Leishmaniasis)

data_train <- training(split)
data_test <- testing(split)

# make sure we have consistent categorical levels
blueprint <- recipe(Cutaneous.Leishmaniasis ~ ., data = data_train) %>%
  step_other(all_nominal(), threshold = 0.005)

# create training and test sets for h2o
train_h2o <- prep(blueprint, training = data_train, retain = TRUE) %>%
  juice() %>%
  as.h2o()

test_h2o <- prep(blueprint, training = data_train) %>%
  bake(new_data = data_test) %>%
  as.h2o()

# get response and predictor names
Y <- "Cutaneous.Leishmaniasis"
X <- setdiff(names(data_train), Y)


# training base models

# xgboost
best_xgb <- h2o.xgboost(
  x = X, 
  y = Y, 
  training_frame = train_h2o, 
  ntrees = 5000,
  learn_rate = 0.05,
  max_depth = 3, 
  min_rows = 3,
  sample_rate = 0.8,
  nfolds = 10, 
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE, 
  seed = 123, 
  stopping_rounds = 50,
  stopping_metric = "RMSE", 
  stopping_tolerance = 0
)


# svm
best_svm <- h2o.psvm(
  x = X,
  y = Y,
  folds = 10
)