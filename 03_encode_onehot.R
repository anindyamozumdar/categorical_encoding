source("01_Synthetic_Data_Preparation.R")

# Using model.matrix
new_df <- df
new_df$city <- factor(new_df$city, exclude = NULL)
new_df$hml <- factor(new_df$hml, exclude = NULL)
new_df <- model.matrix(~.-1, data = new_df[, c("city", "hml")],
                       contrasts.arg = list(
                         city = contrasts(new_df$city, contrasts = FALSE),
                         hml = contrasts(new_df$hml, contrasts = FALSE)
                       ))

# Using vtreat - Recommended
tz <- vtreat::designTreatmentsZ(df, c("city", "hml"))
new_df <- vtreat::prepare(tz, df, extracols = "target")

# Using caret
library(caret)
new_df <- df
new_df$city <- factor(new_df$city, exclude = NULL)
new_df$hml <- factor(new_df$hml, exclude = NULL)
new_df$city <- addNA(new_df$city)
new_df$hml <- addNA(new_df$hml)
dv <- caret::dummyVars(" ~ city + hml", data = new_df)
new_df <- data.frame(predict(dv, newdata = df))
