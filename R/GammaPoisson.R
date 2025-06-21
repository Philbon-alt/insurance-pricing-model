rm(list = ls())

library(glmnet)

df <- read.csv("data/train_auto.csv")
source('R/fonction.R')

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
