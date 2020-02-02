source("01_Synthetic_Data_Preparation.R")

encode_target <- function(x, y, sigma = NULL) {
  d <- aggregate(y, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  m <- d[is.na(as.character(d[, 1])), 2]
  l <- d[, 2]
  names(l) <- d[, 1]
  l <- l[x]
  l[is.na(l)] <- m
  if (!is.null(sigma)) {
    l <- l * rnorm(length(l), mean = 1, sd = sigma)
  }
  l
}

table(encode_target(df[["hml"]], df[["target"]]), df[["hml"]], useNA = "ifany")

new_df <- df
new_df[["hml_encoded"]] <- encode_target(df[["hml"]], df[["target"]])
new_df[["hml_encoded2"]] <- encode_target(df[["hml"]], df[["target"]],
                                          sigma = 0.05)
new_df[["city_encoded"]] <- encode_target(df[["city"]], df[["target"]])
