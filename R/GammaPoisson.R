rm(list = ls())

library(glmnet)

source('R/fonction.R')
df_train <- readRDS('data/clean_train.rds')
df_test <- readRDS('data/clean_test.rds')

# Removing non-variable columns
df_train <- df_train[,-c(1, 2)]

#This ds contains some NULL/empty, I'm gonna relace them with NA
df[df == ""] <- NA

# "Clean" the data by removing non-variable columns
df <- df[, -c(1, 2)]

# Split the dataset into training and test datasets
ListSubset<-train_test_split(0.8, df)
train_ds <- ListSubset[[1]]
test_ds <- ListSubset[[2]]

# Fit the Gamma-Poisson model
library(MASS)
model <- glm(TARGET_AMT ~ ., data = train_ds, family = Gamma(link = "log"))
