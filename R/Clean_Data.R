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

# Make dataset where we have NA categories for the factor variables

df_na <- read.csv("data/train_auto.csv")
df_na[df_na == ""] <- NA

#df_na[115]$job
#df_na$job
#df_na[["JOB"]]


cat_vars <- c("PARENT1", "MSTATUS", "SEX", "EDUCATION", "JOB",
              "CAR_TYPE", "CAR_USE", "RED_CAR", "REVOKED", "URBANICITY")

for (var in cat_vars) {
  if (var %in% names(df_na)) {
    df_na[[var]] <- as.character(df_na[[var]])
    df_na[[var]][is.na(df_na[[var]])] <- "Missing"
    df_na[[var]] <- as.factor(df_na[[var]])
  }
}

df_na <- na.omit(df_na)

# Convert categorical variables to factors
df_na$PARENT1 <- as.factor(df_na$PARENT1)
df_na$MSTATUS <- as.factor(df_na$MSTATUS)
df_na$SEX <- as.factor(df_na$SEX)
df_na$EDUCATION <- as.factor(df_na$EDUCATION)
df_na$JOB <- as.factor(df_na$JOB)
df_na$CAR_TYPE <- as.factor(df_na$CAR_TYPE)
df_na$CAR_USE <- as.factor(df_na$CAR_USE)
df_na$RED_CAR <- as.factor(df_na$RED_CAR)
df_na$REVOKED <- as.factor(df_na$REVOKED)
df_na$URBANICITY <- as.factor(df_na$URBANICITY)

# Convert the Monetary values to a numeric variable
df_na$INCOME <- as.numeric(gsub("[$,]", "", df_na$INCOME))
df_na$HOME_VAL <- as.numeric(gsub("[$,]", "", df_na$HOME_VAL))
df_na$BLUEBOOK <- as.numeric(gsub("[$,]", "", df_na$BLUEBOOK))
df_na$OLDCLAIM <- as.numeric(gsub("[$,]", "", df_na$OLDCLAIM))

# Save this as Clean data with NA categories
saveRDS(df_na, file = "data/clean_train_na.rds")

df_na2 <- read.csv("data/test_auto.csv")
df_na3 <- read.csv("data/SHELL_AUTO.csv")
merged_na <- merge.data.frame(df_na2, df_na3, by="INDEX")
merged_na <- merged_na[,-c(1,2,3)]

for (var in cat_vars) {
  if (var %in% names(merged_na)) {
    merged_na[[var]] <- as.character(merged_na[[var]])
    merged_na[[var]][is.na(merged_na[[var]])] <- "Missing"
    merged_na[[var]] <- as.factor(merged_na[[var]])
  }
}
merged_na <- na.omit(merged_na)
# Convert categorical variables to factors
merged_na$PARENT1 <- as.factor(merged_na$PARENT1)
merged_na$MSTATUS <- as.factor(merged_na$MSTATUS)
merged_na$SEX <- as.factor(merged_na$SEX)
merged_na$EDUCATION <- as.factor(merged_na$EDUCATION)
merged_na$JOB <- as.factor(merged_na$JOB)
merged_na$CAR_TYPE <- as.factor(merged_na$CAR_TYPE)
merged_na$CAR_USE <- as.factor(merged_na$CAR_USE)
merged_na$RED_CAR <- as.factor(merged_na$RED_CAR)
merged_na$REVOKED <- as.factor(merged_na$REVOKED)
merged_na$URBANICITY <- as.factor(merged_na$URBANICITY)
# Convert the Monetary values to a numeric variable
merged_na$INCOME <- as.numeric(gsub("[$,]", "", merged_na$INCOME))
merged_na$HOME_VAL <- as.numeric(gsub("[$,]", "", merged_na$HOME_VAL))
merged_na$BLUEBOOK <- as.numeric(gsub("[$,]", "", merged_na$BLUEBOOK))
merged_na$OLDCLAIM <- as.numeric(gsub("[$,]", "", merged_na$OLDCLAIM))
# Save this as Clean data with NA categories
saveRDS(merged_na, file = "data/clean_test_na.rds")
