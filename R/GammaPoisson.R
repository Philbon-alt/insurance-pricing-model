rm(list = ls())

library(glmnet)

source('R/fonction.R')
df_train <- readRDS('data/clean_train.rds')
df_test <- readRDS('data/clean_test.rds')

# The dataset doesn't contain an exposure variable, so we will create a dummy one
# If this doesn't work, we'll just use a different dataset whose more shaped for this model
# I'll modify the TIF variable to be between 0 and 1, then use it as the offset

#Les TIF de 0 causent problèmes, on les remplace par 0.001 pour que le glm soit possible
df_train$TIF[df_train$TIF <= 0 | is.na(df_train$TIF)] <- 0.01
df_test$TIF[df_test$TIF <= 0 | is.na(df_test$TIF)] <- 0.01

df_train$exp <- (df_train$TIF) / (max(df_train$TIF))
df_test$exp <- (df_test$TIF) / (max(df_test$TIF))

# Remove non-variable columns
df_train <- df_train[,-c(1,2) ]
df_test <- df_test[,-c(1,2) ]

# Fit the Gamma-Poisson model
library(MASS)
model <- glm(TARGET_AMT ~ KIDSDRIV + offset(log(exp)) + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL + MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + URBANICITY  , data = df_train)

summary(model)

# Predict on the test set
pred <- predict.glm(model, df_test)
EQM_PG <- EQM(df_test$TARGET_AMT, pred)
EQM_PG

#try with the na categories databank
df_train_na <- readRDS('data/clean_train_na.rds')
df_test_na <- readRDS('data/clean_test_na.rds')
df_train_na$TIF[df_train_na$TIF <= 0 | is.na(df_train_na$TIF)] <- 0.01
df_test_na$TIF[df_test_na$TIF <= 0 | is.na(df_test_na$TIF)] <- 0.01
df_train_na$exp <- (df_train_na$TIF) / (max(df_train_na$TIF))
df_test_na$exp <- (df_test_na$TIF) / (max(df_test_na$TIF))

# Remove non-variable columns
df_train_na <- df_train_na[,-c(1,2) ]
df_test_na <- df_test_na[,-c(1,2) ]

# Fit the Gamma-Poisson model with na categories
modelna <- glm(TARGET_AMT ~ KIDSDRIV + offset(log(exp)) + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL + MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + URBANICITY  , data = df_train_na)

summary(modelna)

predna <- predict.glm(modelna, df_test_na)
EQM_PG_na <- EQM(df_test_na$TARGET_AMT, predna)
EQM_PG_na

# Do the mean test

df_mean <- readRDS("data/mean_test.rds")
df_na_mean <- readRDS("data/mean_test_na.rds")

df_mean$TIF[df_mean$TIF <= 0 | is.na(df_mean$TIF)] <- 0.01
df_mean$exp <- (df_mean$TIF) / (max(df_mean$TIF))

df_na_mean$TIF[df_na_mean$TIF <= 0 | is.na(df_na_mean$TIF)] <- 0.01
df_na_mean$exp <- (df_na_mean$TIF) / (max(df_na_mean$TIF))

pred_mean <- mean(predict.glm(model, df_mean))
pred_na_mean <- mean(predict.glm(modelna, df_na_mean))

# Save the results
results_PG <- list(model = "Gamma-Poisson", EQM = EQM_PG, EQM_na = EQM_PG_na, MEAN = pred_mean, MEAN_na = pred_na_mean, AIC = AIC(model), AIC_na = AIC(modelna))
saveRDS(results_PG, 'output/results_PG.rds')