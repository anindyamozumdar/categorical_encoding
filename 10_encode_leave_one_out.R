source("01_Synthetic_Data_Preparation.R")

encode_leave_one_out <- function(x, y, sigma = NULL) {
  n <- length(x)
  x[is.na(x)] <- "__MISSING"
  x2 <- vapply(1:n, function(i) {
    xval <- x[i]
    yloo <- y[-i]
    xloo <- x[-i]
    yloo <- yloo[xloo == xval]
    mean(yloo, na.rm = TRUE)
  }, numeric(1))
  if (!is.null(sigma)) {
    x2 <- x2 * rnorm(n, mean = 1, sd = sigma)
  }
  x2
}

new_df <- df
new_df[["hml_encoded"]] <- encode_leave_one_out(df[["hml"]], df[["target"]])
new_df[["city_encoded"]] <- encode_leave_one_out(df[["city"]], df[["target"]])
head(new_df)
set.seed(77)
new_df[["city_encoded"]] <- encode_leave_one_out(df[["city"]], df[["target"]],
                                                 sigma = 0.05)
head(new_df)
set.seed(1123)
new_df[["city_encoded"]] <- encode_leave_one_out(df[["city"]], df[["target"]],
                                                 sigma = 0.05)
head(new_df)
