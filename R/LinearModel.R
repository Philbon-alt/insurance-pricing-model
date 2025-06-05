rm(list = ls())

df <- read.csv("data/train_auto.csv")
source('R/fonction.R')

#This ds contains some NULL/empty, I'm gonna relace them with NA
df[df == ""] <- NA

# For this first model, we will ignore any row that has a missing value
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

# Split the dataset into training and test datasets
ListSubset<-train_test_split(0.8, df)
train_ds <- ListSubset[[1]]
test_ds <- ListSubset[[2]]

# Linear Regression Model
lm_model <- lm(TARGET_AMT ~ ., data = train_ds)

# Summary of the model
summary(lm_model)

# Save the model
saveRDS(lm_model, file = "models/LinearModel.rds")