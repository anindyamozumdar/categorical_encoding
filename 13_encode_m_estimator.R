source("01_Synthetic_Data_Preparation.R")

# https://en.wikipedia.org/wiki/Additive_smoothing
# http://contrib.scikit-learn.org/categorical-encoding/mestimate.html

encode_m_estimator <- function(x, y, m = 1, sigma = NULL) {
  p_all <- mean(y)
  
  d <- aggregate(y, list(factor(x, exclude = NULL)), sum, na.rm = TRUE)
  d2 <- aggregate(y, list(factor(x, exclude = NULL)), length)
  g <- names(d)[1]
  d <- merge(d, d2, by = g, all = TRUE)
  d[, 4] <- (d[, 2] + p_all * m) / (d[, 3] + m)
  
  m <- d[is.na(as.character(d[, 1])), 4]
  l <- d[, 4]
  names(l) <- d[, 1]
  l <- l[x]
  l[is.na(l)] <- m
  if (!is.null(sigma)) {
    l <- l * rnorm(length(l), mean = 1, sd = sigma)
  }
  l
}

table(encode_m_estimator(df[["hml"]], df[["target"]]),
      df[["hml"]], useNA = "ifany")

new_df <- df
new_df[["hml_encoded"]] <- encode_m_estimator(df[["hml"]], df[["target"]])
new_df[["hml_encoded2"]] <- encode_m_estimator(df[["hml"]], df[["target"]],
                                               sigma = 0.05)
new_df[["city_encoded"]] <- encode_m_estimator(df[["city"]], df[["target"]])