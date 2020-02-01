source("01_Synthetic_Data_Preparation.R")

# https://kiwidamien.github.io/james-stein-encoder.html

encode_james_stein <- function(x, y, sigma = NULL) {
  n_all <- length(y)
  p_all <- mean(y)
  var_all <- (p_all * (1 - p_all)) / n_all
  
  d <- aggregate(y, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  d2 <- aggregate(y, list(factor(x, exclude = NULL)), length)
  g <- names(d)[1]
  d <- merge(d, d2, by = g, all = TRUE)
  d[, 4] <- (d[, 2] * (1 - d[, 2])) / d[, 3]
  d[, 5] <- d[, 4] / (d[, 4] + var_all)
  d[, 6] <- (1 - d[, 5]) * d[, 2] + d[, 5] * p_all
  
  m <- d[is.na(as.character(d[, 1])), 6]
  l <- d[, 6]
  names(l) <- d[, 1]
  l <- l[x]
  l[is.na(l)] <- m
  if (!is.null(sigma)) {
    l <- l * rnorm(length(l), mean = 1, sd = sigma)
  }
  l
}

table(encode_james_stein(df[["hml"]], df[["target"]]),
      df[["hml"]], useNA = "ifany")

new_df <- df
new_df[["hml_encoded"]] <- encode_james_stein(df[["hml"]], df[["target"]])
new_df[["hml_encoded2"]] <- encode_james_stein(df[["hml"]], df[["target"]],
                                               sigma = 0.05)
new_df[["city_encoded"]] <- encode_james_stein(df[["city"]], df[["target"]])