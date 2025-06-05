rm(list = ls())

library(ISLR2)
library(glmnet)
df <- read.csv("data/train_auto.csv")
source('R/fonction.R')

#This ds contains some NULL/empty, I'm gonna relace them with NA
df[df == ""] <- NA

# For this second model, we will ignore any row that has a missing value
df <- na.omit(df)

# Convert categorical variables to factors
df$PARENT1 <- as.factor(df$PARENT1)
df$MSTATUS <- as.factor(df$MSTATUS)
df$SEX <- as.factor(df$SEX)
df$EDUCATION <- as.factor(df$EDUCATION)
df$JOB <- as.factor(df$JOB)
df$CAR_TYPE <- as.factor(df$CAR_TYPE)
df$CAR_USE <- as.factor(df$CAR_USE)
df$RED_CAR <- as.factor(df$RED_CAR)
df$REVOKED <- as.factor(df$REVOKED)
df$URBANICITY <- as.factor(df$URBANICITY)

# Convert the Monetary values to a numeric variable
df$INCOME <- as.numeric(gsub("[$,]", "", df$INCOME))
df$HOME_VAL <- as.numeric(gsub("[$,]", "", df$HOME_VAL))
df$BLUEBOOK <- as.numeric(gsub("[$,]", "", df$BLUEBOOK))
df$OLDCLAIM <- as.numeric(gsub("[$,]", "", df$OLDCLAIM))

# "Clean" the data by removing non-variable columns
df <- df[, -c(1, 2)]

# Split the dataset into training, hyper-perameter training and test datasets
ListSubset<-train_test_split(0.8, df)
train_ds <- ListSubset[[1]]
test_ds <- ListSubset[[2]]

ListSubset2 <- train_test_split(0.75, train_ds)
train_ds <- ListSubset2[[1]]
hyper_train_ds <- ListSubset2[[2]]

# Prepare the data for glmnet
x_train <- model.matrix(TARGET_AMT ~ ., data = train_ds)[, -1]
y_train <- train_ds$TARGET_AMT
x_hyper_train <- model.matrix(TARGET_AMT ~ ., data = hyper_train_ds)[, -1]
y_hyper_train <- hyper_train_ds$TARGET_AMT
x_test <- model.matrix(TARGET_AMT ~ ., data = test_ds)[, -1]
y_test <- test_ds$TARGET_AMT

# Fit the Elastic Net model
alpha <- seq(0,1,0.02)
