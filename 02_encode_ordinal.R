source("01_Synthetic_Data_Preparation.R")

# Base R
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

table(df[["hml"]], encode_ordinal(df[["hml"]]), useNA = "ifany")
table(df[["hml"]],
      encode_ordinal(df[["hml"]], order = c(NA, "Low", "Medium", "High")),
      useNA = "ifany")
table(df[["hml"]],
      encode_ordinal(df[["hml"]], order = c("Low", "Medium", "High", NA)),
      useNA = "ifany")
table(df[["hml"]],
      encode_ordinal(df[["hml"]], order = c("Low", "Medium", NA, "High")),
      useNA = "ifany")

new_df <- df
new_df[["hml_encoded"]] <- encode_ordinal(df[["hml"]])
new_df[["city_encoded"]] <- encode_ordinal(df[["city"]])