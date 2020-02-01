source("01_Synthetic_Data_Preparation.R")

encode_woe <- function(x, y, sigma = NULL) {
  d <- aggregate(y, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  d[["woe"]] <- log(((1 / d[, 2]) - 1) *
                      (sum(y) / sum(1-y)))
  m <- d[is.na(as.character(d[, 1])), 3]
  l <- d[, 3]
  names(l) <- d[, 1]
  l <- l[x]
  l[is.na(l)] <- m
  if (!is.null(sigma)) {
    l <- l + rnorm(length(l), mean = 0, sd = sigma)
  }
  l
}

table(encode_woe(df[["hml"]], df[["target"]]), df[["hml"]], useNA = "ifany")

new_df <- df
new_df[["hml_encoded"]] <- encode_woe(df[["hml"]], df[["target"]])
new_df[["hml_encoded2"]] <- encode_woe(df[["hml"]], df[["target"]],
                                       sigma = 0.05)
new_df[["city_encoded"]] <- encode_woe(df[["city"]], df[["target"]])
