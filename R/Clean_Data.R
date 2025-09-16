rm(list = ls())

df <- read.csv("data/train_auto.csv")

#This ds contains some NULL/empty, I'm gonna relace them with NA
df[df == ""] <- NA

#Replace missing categories with "Missing"
cat_vars <- c("PARENT1", "MSTATUS", "SEX", "EDUCATION", "JOB",
              "CAR_TYPE", "CAR_USE", "RED_CAR", "REVOKED", "URBANICITY")

for (var in cat_vars) {
  if (var %in% names(df)) {
    df[[var]] <- as.character(df[[var]])
    df[[var]][is.na(df[[var]])] <- "Missing"
    df[[var]] <- as.factor(df[[var]])
  }
}

#Remove the remaining NAs (rows with NAs in numeric variables)
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

#Since they are in order of INDEX, there is no need to shuffle the data
ntest<-round(nrow(df)/4,0)

clean_test_na <- df[0:ntest, ]
clean_train_na <- df[(ntest+1):nrow(df), ]

# Save this as na data.
saveRDS(clean_train_na, file = "data/clean_train_na.rds")
saveRDS(clean_test_na, file = "data/clean_test_na.rds")

# Now let's make the clean data without NA categories
df_noNA <- read.csv("data/train_auto.csv")
df_noNA[df_noNA == ""] <- NA
df_noNA <- na.omit(df_noNA)

# Convert categorical variables to factors
df_noNA$PARENT1 <- as.factor(df_noNA$PARENT1)
df_noNA$MSTATUS <- as.factor(df_noNA$MSTATUS)
df_noNA$SEX <- as.factor(df_noNA$SEX)
df_noNA$EDUCATION <- as.factor(df_noNA$EDUCATION)
df_noNA$JOB <- as.factor(df_noNA$JOB)
df_noNA$CAR_TYPE <- as.factor(df_noNA$CAR_TYPE)
df_noNA$CAR_USE <- as.factor(df_noNA$CAR_USE)
df_noNA$RED_CAR <- as.factor(df_noNA$RED_CAR)
df_noNA$REVOKED <- as.factor(df_noNA$REVOKED)
df_noNA$URBANICITY <- as.factor(df_noNA$URBANICITY)

# Convert the Monetary values to a numeric variable
df_noNA$INCOME <- as.numeric(gsub("[$,]", "", df_noNA$INCOME))
df_noNA$HOME_VAL <- as.numeric(gsub("[$,]", "", df_noNA$HOME_VAL))
df_noNA$BLUEBOOK <- as.numeric(gsub("[$,]", "", df_noNA$BLUEBOOK))
df_noNA$OLDCLAIM <- as.numeric(gsub("[$,]", "", df_noNA$OLDCLAIM))

# Again, since these are index, no need to shuffle
ntestnoNA<-round(nrow(df_noNA)/4,0)

clean_test <- df_noNA[0:ntestnoNA, ]
clean_train <- df_noNA[(ntestnoNA+1):nrow(df_noNA), ]

# Save this as Clean data without NA categories
saveRDS(clean_train, file = "data/clean_train.rds")
saveRDS(clean_test, file = "data/clean_test.rds")

# Let's do the test set, just to test the mean since we don't have the target amount
df2 <- read.csv("data/test_auto.csv")
df3 <- read.csv("data/MEAN_AUTO.csv")

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

saveRDS(merged, file = "data/mean_test.rds")

# Make one for the test set with NA categories

df_na2 <- read.csv("data/test_auto.csv")
df_na3 <- read.csv("data/MEAN_AUTO.csv")
merged_na <- merge.data.frame(df_na2, df_na3, by="INDEX")
merged_na <- merged_na[,-c(1,2,3)]

merged_na[merged_na == ""] <- NA

for (v in cat_vars) {
  if (v %in% names(merged_na)) {
    merged_na[[v]] <- as.character(merged_na[[v]])
    merged_na[[v]][is.na(merged_na[[v]])] <- "Missing"
    merged_na[[v]] <- as.factor(merged_na[[v]])
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
merged_na <- na.omit(merged_na)

saveRDS(merged_na, file = "data/mean_test_na.rds")