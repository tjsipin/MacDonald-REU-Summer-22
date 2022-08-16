---
title: "custom stacking on og later cutaneuos leishmaniasis"
author: "Lyndsey Umsted"
date: '2022-07-22'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
library(tidymodels)
library(stacks)

```

```{r}
# load and split the early data using imputed data
getwd()
aad <- read.csv("models/data/aad.csv")
aad <- subset(aad, !is.na(aad$Cutaneous.Leishmaniasis))
aad <-subset(aad, aad$Cutaneous.Leishmaniasis > 0)
summary(aad$Cutaneous.Leishmaniasis)
```

```{r}
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
  select(c("Population", "Cutaneous.Leishmaniasis", "LST_Day", "Precip", "AvgRad", "SWOccurrence", "NDVI", "EVI", "pland_forest", "te_forest", "enn_mn_forest"))
later_data_small <- subset(later_data_small, !is.na(later_data_small$LST_Day))
later_data_small <- subset(later_data_small, !is.na(later_data_small$SWOccurrence))
later_data_small$pland_forest <- ifelse(is.na(later_data_small$pland_forest), 
                                        0, later_data_small$pland_forest)
later_data_small$te_forest <- ifelse(is.na(later_data_small$te_forest), 
                                     0, later_data_small$te_forest)
later_data_small$enn_mn_forest <- ifelse(is.na(later_data_small$enn_mn_forest), 
                                         0, later_data_small$enn_mn_forest)
```


```{r}
library(dplyr)
summary(later_data_small$Cutaneous.Leishmaniasis)
cat_df <- later_data_small
cat_df$Cutaneous.Leishmaniasis <- cut(later_data_small$Cutaneous.Leishmaniasis, breaks = c(0, 0.28177, 10^3), labels = c("low", "high")) #median


skimr::skim(cat_df)
round(prop.table(table(cat_df$Cutaneous.Leishmaniasis)), 2)

```

Splitting Data into Trianing and Testing Sets
```{r}
set.seed(123) # for reproducibility

split <- initial_split(cat_df, prop = 0.8)

data_train <- training(split)
data_test <- testing(split)

k_folds_data <- vfold_cv(data_train)
```

```{r}
data_train
```


```{r}
pca_rec <- recipe(Cutaneous.Leishmaniasis ~., data = data_train) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_lincomb(all_numeric(), -all_outcomes()) %>%
  step_other(all_nominal()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal()) %>%
  step_pca(all_predictors(), num_comp = 5)

spline_rec <- recipe(Cutaneous.Leishmaniasis ~., data = data_train) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_lincomb(all_numeric(), -all_outcomes()) %>%
  step_rm(all_nominal()) %>%
  step_bs(all_predictors()) %>%
  step_YeoJohnson(all_predictors())

tidy_rec <- recipe(Cutaneous.Leishmaniasis ~., data = data_train) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_lincomb(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal())
```

```{r}
pca_rec %>% prep() %>% juice()

spline_rec %>% prep() %>% juice()

tidy_rec %>% prep() %>% juice()
```

```{r}

```




