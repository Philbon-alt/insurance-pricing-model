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

df_train <- df_train[,-c(1,2) ]

# Fit the Gamma-Poisson model
library(MASS)
model <- glm(TARGET_AMT ~ KIDSDRIV + offset(log(exp)) + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL + MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + URBANICITY  , data = df_train)

summary(model)

# Predict on the test set
pred <- predict.glm(model, df_test)
EQM_PG <- EQM(df_test$p_target, pred)
EQM_PG
