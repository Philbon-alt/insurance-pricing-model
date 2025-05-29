train_ds <- read.csv("data/train_auto.csv")
test_ds <- read.csv("data/test_auto.csv")

# For this first model, we will ignore any row that has a missing value
train_ds <- na.omit(train_ds)

# Convert categorical variables to factors
train_ds$PARENT1 <- as.factor(train_ds$PARENT1)
train_ds$MSTATUS <- as.factor(train_ds$MSTATUS)
train_ds$SEX <- as.factor(train_ds$SEX)
train_ds$EDUCATION <- as.factor(train_ds$EDUCATION)
train_ds$JOB <- as.factor(train_ds$JOB)
train_ds$CAR_TYPE <- as.factor(train_ds$CAR_TYPE)
train_ds$CAR_USE <- as.factor(train_ds$CAR_USE)
train_ds$RED_CAR <- as.factor(train_ds$RED_CAR)
train_ds$REVOKED <- as.factor(train_ds$REVOKED)
train_ds$URBANICITY <- as.factor(train_ds$URBANICITY)


# "Clean" the data by removing non-variable columns
train_ds <- train_ds[, -c(1, 2)]

# Linear Regression Model
lm_model <- lm(TARGET_AMT ~ ., data = train_ds)

# Summary of the model
summary(lm_model)

