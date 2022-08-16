## Random Forest with Resampling


library(tidymodels)   # packages for modeling and statistical analysis
library(tune)         # For hyperparemeter tuning
library(workflows)    # streamline process
library(tictoc)       # for 
library(randomForest)

setwd("C:/Users/18586/Desktop/Summer Disease Internship/MacDonald-REU-Summer-22/models")
aad <- read.csv("data/aad.csv")
library(dplyr)
## splitting the data into a before 2014 set and an after 2014 set

early_data <- aad %>%
  filter(Year < 2014)%>%
  dplyr::select(c(7,30,36,40,45))
later_data <- aad %>%
  filter(Year > 2013) %>%
  dplyr::select(c(7,30,36,40,45))

#names(early_data)

## removing unnecessary variables

# early_data <- early_data %>%
#   dplyr::select(-c("AvgRad"))
# later_data <- later_data %>%
#   dplyr::select(-c("StableLights")) 

early_data$enn_mn_forest[is.na(early_data$enn_mn_forest)] = 0
early_data$te_forest[is.na(early_data$te_forest)] = 0
early_data$area_mn_forest[is.na(early_data$area_mn_forest)] = 0
early_data$pland_forest[is.na(early_data$pland_forest)] = 0

later_data$enn_mn_forest[is.na(later_data$enn_mn_forest)] = 0
later_data$te_forest[is.na(later_data$te_forest)] = 0
later_data$area_mn_forest[is.na(later_data$area_mn_forest)] = 0
later_data$pland_forest[is.na(later_data$pland_forest)] = 0

# lapply(early_data, as.numeric)
# lapply(later_data, as.numeric)

early_data_cutaneous <- early_data %>%
  na.omit(early_data$Cutaneous.Leishmaniasis)
later_data_cutaneous <- later_data %>%
  na.omit(early_data$Cutaneous.Leishmaniasis)

library(dplyr)
summary(later_data_cutaneous$Cutaneous.Leishmaniasis[later_data_cutaneous$Cutaneous.Leishmaniasis > 0])

later_data_cutaneous <- subset(later_data_cutaneous, later_data_cutaneous$Cutaneous.Leishmaniasis > 0 & 
                                 later_data_cutaneous$Cutaneous.Leishmaniasis < 50)

later_data_cutaneous$Cutaneous.Leishmaniasis <- cut(later_data_cutaneous$Cutaneous.Leishmaniasis
                                                    [later_data_cutaneous$Cutaneous.Leishmaniasis > 0], 
                                                    breaks = c(0, 0.09295, 0.89880, 10^3), 
                                                    labels = c("low", "moderate", "high")) # 25% 75% 100%
head(later_data_cutaneous)

later_data_cutaneous$Cutaneous.Leishmaniasis = as.factor(later_data_cutaneous$Cutaneous.Leishmaniasis)

table(later_data_cutaneous$Cutaneous.Leishmaniasis)

# Explore the data:
skimr::skim(later_data_cutaneous)

# Check severity of class imbalance
round(prop.table(table(later_data_cutaneous$Cutaneous.Leishmaniasis)), 2)


# Split data into train and test data and create resamples for tuning
library(rsample)
set.seed(2022)
train_test_split_data <- initial_split(later_data_cutaneous, 
                                       strata = "Cutaneous.Leishmaniasis", 
                                       prop = 0.7)
training <- training(train_test_split_data)
testing <-  testing(train_test_split_data)

set.seed(222)
rf <- randomForest(Cutaneous.Leishmaniasis ~., data = training, 
                   mtry = 1, ntree = 501, importance = TRUE,
                   proximity = TRUE)

rf

attributes(rf)

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, training)

confusionMatrix(p1, training$Cutaneous.Leishmaniasis)

# Prediction & Confusion Matrix - test data
p2 <- predict(rf, testing)
confusionMatrix(p2, testing$Cutaneous.Leishmaniasis)


# Error Rate of Random Forest
plot(rf)


# Tuning the Random Forest Model
t <- tuneRF(training[,-1], training[,1],
       stepFactor = 0.5,
       plot =  TRUE,
       ntreeTry = 500,
       trace = TRUE,
       improve = 0.05)

# Number of nodes for the trees
hist(treesize(rf), main = "number of nodes for the trees", col = "green")


# Variable Importance
varImpPlot(rf, sort = T, n.var = 2, main = "Top 2 - Variable Importance")

importance(rf)
varUsed(rf)

# Partial Dependence Plot
partialPlot(rf, training, te_forest, "high")


# Extract a Single Tree
getTree(rf, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, training$Cutaneous.Leishmaniasis)