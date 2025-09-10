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

n <- length(y_test)
mse <- EN_EQM
coef_en <- coef(ENfit, s = bestlam)
df_en <- sum(coef_en != 0) - 1
AIC_EN <- n * log(mse) + 2 * df_en

AIC_EN


#===============================================================
# NA section
set.seed(0)

df_train2 <- readRDS('data/clean_train_na.rds')
df_test2 <- readRDS('data/clean_test_na.rds')

# Removing non-variable columns
df_train2 <- df_train2[,-c(1, 2)]

# Split the dataset into training, hyper-perameter training and test datasets

ListSubset2 <- train_test_split(0.75, df_train2)
reg_train_ds2 <- ListSubset2[[1]]
hyper_train_ds2 <- ListSubset2[[2]]

# Prepare the data for glmnet
x_train2 <- model.matrix(TARGET_AMT ~ ., data = reg_train_ds2)[,-1]
y_train2 <- reg_train_ds2$TARGET_AMT
x_hyper_train2 <-
  model.matrix(TARGET_AMT ~ ., data = hyper_train_ds2)[,-1]
y_hyper_train2 <- hyper_train_ds2$TARGET_AMT
x_test2 <- model.matrix(p_target ~ ., data = df_test2)[,-24]
y_test2 <- df_test2$p_target


grid <- 10 ^ seq(10,-2, length = 200)
index2 <- which(colnames(x_train2) == "TARGET_AMT")
cv.out2 <- cv.glmnet(x_train2, y_train2, alpha = 0.5, lambda = grid)
bestlam2 <- cv.out2$lambda.min

# Fit the Elastic Net model
alpha2 <- seq(0, 1, 0.02)

grid <- 10 ^ seq(10,-2, length = 200)

sortie2 <-
  Select_alpha(alpha2, x_train2, y_train2, x_hyper_train2, y_hyper_train2)

cv.out2 <-
  cv.glmnet(x_train2, y_train2, alpha2 = sortie2[[1]], lambda = grid)
plot(cv.out2)
bestlam2 <- cv.out2$lambda.min

ENfit2 <-
  glmnet(x_train2, y_train2, alpha = sortie2[[1]], lambda = sortie2[[2]])
ENfit2$beta

x_test2 <- x_test2[,-1]
x_test2
predEN2 <- predict(ENfit2, s = bestlam2, newx = x_test2)

EN_EQM_na = EQM(predEN2, y_test2)

modeleEN_na <- list(
  mod = ENfit2,
  alpha = sortie2[[1]],
  lambda = sortie2[[2]],
  bestlam = bestlam2
)

n2 <- length(y_test2)
mse2 <- EN_EQM_na
coef_en2 <- coef(ENfit2, s = bestlam2)
df_en2 <- sum(coef_en2 != 0) - 1
AIC_EN_na <- n2 * log(mse2) + 2 * df_en2

AIC_EN_na
