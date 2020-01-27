source("01_Synthetic_Data_Preparation.R")

new_df <- df

# Base R
encode_ordinal <- function(x, order = unique(x), missing_lowest = FALSE) {
  x <- as.numeric(factor(x, levels = order))
  if (!missing_lowest) {
    x[is.na(x)] <- max(x, na.rm = TRUE) + 1
  } else {
    x[is.na(x)] <- 0
    x <- x + 1
  }
  x
}

table(df[["hml"]], encode_ordinal(df[["hml"]]), useNA = "ifany")
table(df[["hml"]],
      encode_ordinal(df[["hml"]], missing_lowest = TRUE),
      useNA = "ifany")
table(df[["hml"]],
      encode_ordinal(df[["hml"]], order = c("Low", "Medium", "High")),
      useNA = "ifany")

new_df[["hml_encoded"]] <- encode_ordinal(df[["hml"]])