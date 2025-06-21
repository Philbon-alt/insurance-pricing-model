rm(list = ls())

set.seed(0)

library(ISLR2)
library(glmnet)
source('R/fonction.R')
df_train <- readRDS('data/clean_train.rds')
df_test <- readRDS('data/clean_test.rds')

# Removing non-variable columns
df_train <- df_train[,-c(1, 2)]

# Split the dataset into training, hyper-perameter training and test datasets

ListSubset <- train_test_split(0.75, df_train)
reg_train_ds <- ListSubset[[1]]
hyper_train_ds <- ListSubset[[2]]

# Prepare the data for glmnet
x_train <- model.matrix(TARGET_AMT ~ ., data = reg_train_ds)[,-1]
y_train <- reg_train_ds$TARGET_AMT
x_hyper_train <-
  model.matrix(TARGET_AMT ~ ., data = hyper_train_ds)[,-1]
y_hyper_train <- hyper_train_ds$TARGET_AMT
x_test <- model.matrix(p_target ~ ., data = df_test)[,-24]
y_test <- df_test$p_target


grid <- 10 ^ seq(10,-2, length = 200)
index <- which(colnames(x_train) == "TARGET_AMT")
cv.out <- cv.glmnet(x_train, y_train, alpha = 0.5, lambda = grid)
bestlam <- cv.out$lambda.min

# Fit the Elastic Net model
alpha <- seq(0, 1, 0.02)

grid <- 10 ^ seq(10,-2, length = 200)

sortie <-
  Select_alpha(alpha, x_train, y_train, x_hyper_train, y_hyper_train)

cv.out <-
  cv.glmnet(x_train, y_train, alpha = sortie[[1]], lambda = grid)
plot(cv.out)
bestlam <- cv.out$lambda.min

ENfit <-
  glmnet(x_train, y_train, alpha = sortie[[1]], lambda = sortie[[2]])
ENfit$beta

x_test <- x_test[,-1]

predEN <- predict(ENfit, s = bestlam, newx = x_test)

EN_EQM = EQM(predEN, y_test)

modeleEN <- list(
  mod = ENfit,
  alpha = sortie[[1]],
  lambda = sortie[[2]],
  bestlam = bestlam
)

saveRDS(modeleEN, file = "models/ElasticNetModel.rds")