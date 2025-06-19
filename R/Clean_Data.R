rm(list = ls())

df <- read.csv("data/train_auto.csv")

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

# Save this as Clean data.

saveRDS(df, file = "data/clean_train.rds")

df2 <- read.csv("data/test_auto.csv")
df3 <- read.csv("data/SHELL_AUTO.csv")

merged <- merge.data.frame(df2, df3, by="INDEX")

merged <- merged[,-c(1,2,3)]

merged$PARENT1 <- as.factor(merged$PARENT1)
merged$MSTATUS <- as.factor(merged$MSTATUS)
merged$SEX <- as.factor(merged$SEX)
merged$EDUCATION <- as.factor(merged$EDUCATION)
merged$JOB <- as.factor(merged$JOB)
merged$CAR_TYPE <- as.factor(merged$CAR_TYPE)
merged$CAR_USE <- as.factor(merged$CAR_USE)
merged$RED_CAR <- as.factor(merged$RED_CAR)
merged$REVOKED <- as.factor(merged$REVOKED)
merged$URBANICITY <- as.factor(merged$URBANICITY)

# Convert the Monetary values to a numeric variable
merged$INCOME <- as.numeric(gsub("[$,]", "", merged$INCOME))
merged$HOME_VAL <- as.numeric(gsub("[$,]", "", merged$HOME_VAL))
merged$BLUEBOOK <- as.numeric(gsub("[$,]", "", merged$BLUEBOOK))
merged$OLDCLAIM <- as.numeric(gsub("[$,]", "", merged$OLDCLAIM))

merged[merged == ""] <- NA

merged <- na.omit(merged)

saveRDS(merged, file = "data/clean_test.rds")