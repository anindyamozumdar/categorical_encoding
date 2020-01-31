source("01_Synthetic_Data_Preparation.R")

encode_polynomial <- function(df, var) {
  x <- df[[var]]
  x <- unique(x)
  n <- length(x)
  d <- as.data.frame(contr.poly(n))
  d[[var]] <- x
  names(d) <- c(paste0(var, 1:(n-1)), var)
  d
}

d <- encode_polynomial(df, "hml")
d

df[1, "hml"] <- NA_character_
d <- encode_polynomial(df, "hml")
d

d <- data.frame(v = sample(c("a", "b", "c", "d", "e"), 100, replace = TRUE))
d <- encode_polynomial(d, "v")
d
