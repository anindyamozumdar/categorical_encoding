# Libraries
library(maps)
library(data.table)

# Get American city names from the world.cities dataset in the maps package
cities <- world.cities[world.cities["country.etc"] == "USA", "name"]
cities <- unique(cities)

# Take 100 unique cities
set.seed(42)
cities <- sample(cities, 100)

# Define an ordinal variable called high/medium/low
hml <- c("High", "Medium", "Low")

# Define a dataset comprising these two variables
set.seed(42)
df <- data.table(
  city = sample(cities, 5000, replace = TRUE),
  hml = sample(hml, 5000, replace = TRUE)
)

# Randomly set 5% of the values of the city and hml to missing
set.seed(317)
sr <- sample(1:5000, 250)
df[sr, "city"] <- NA_character_
set.seed(111)
sr <- sample(1:5000, 250)
df[sr, "hml"] <- NA_character_

# Create the target variable; it has a relationship with the hml variable but
# not the city
df[["target"]] <- 0
table(df$hml, useNA = "ifany")
set.seed(42)
sr <- sample(which(df[["hml"]] == "High"), 40)
df[sr, "target"] <- 1
sr <- sample(which(df[["hml"]] == "Medium"), 150)
df[sr, "target"] <- 1
sr <- sample(which(df[["hml"]] == "Low"), 500)
df[sr, "target"] <- 1
sr <- sample(which(is.na(df[["hml"]])), 125)
df[sr, "target"] <- 1
sum(df$target)
