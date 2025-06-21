rm(list = ls())

source('R/fonction.R')
df_train <- readRDS('data/clean_train.rds')
df_test <- readRDS('data/clean_test.rds')

# Removing non-variable columns
df_train <- df_train[,-c(1, 2)]

# Linear Regression Model
lm_model <- lm(TARGET_AMT ~ ., data = df_train)

# Summary of the model
summary(lm_model)

# Save the model
pred <- predict.lm(lm_model, df_test)
LM_EQM <- EQM(pred, df_test$p_target)
LM_EQM

saveRDS(lm_model, file = "models/LinearModel.rds")