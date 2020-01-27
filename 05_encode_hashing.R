source("01_Synthetic_Data_Preparation.R")

# Using the FeatureHashing package
library(FeatureHashing)
library(xgboost)
m.mat <- hashed.model.matrix(c("city", "hml"), df, hash.size = 2 ^ 10,
                             create.mapping = TRUE)

# Extract the mapping
mapping <- hash.mapping(m.mat)

# Check collision rate
mean(duplicated(mapping))

# Names
names(mapping)

# Hashed values
hashed.value(names(mapping))

# Testing with xgboost
bst <- xgboost(data = m.mat, label = df$target, nround = 10, params = list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "auc"
))
detach("package:xgboost", unload = TRUE)
detach("package:FeatureHashing", unload = TRUE)