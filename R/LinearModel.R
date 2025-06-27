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

# Now let's try a model that takes the dataset where the NAs are categories
df_train_na <- readRDS('data/clean_train_na.rds')
df_test_na <- readRDS('data/clean_test_na.rds')

# Removing non-variable columns
df_train_na <- df_train_na[,-c(1, 2)]

# Linear Regression Model with NAs as categories
lm_model_na <- lm(TARGET_AMT ~ ., data = df_train_na)

# Summary of the model with NAs as categories
summary(lm_model_na)

# Let's try the precision now
pred_na <- predict.lm(lm_model_na, df_test_na)
LM_EQM_na <- EQM(pred_na, df_test_na$p_target)
LM_EQM_na < LM_EQM

