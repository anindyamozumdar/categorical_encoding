source("01_Synthetic_Data_Preparation.R")

# https://stats.stackexchange.com/questions/411134/how-to-calculate-helmert-coding
# https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

helmert <- function(n) {
  m <- t((diag(seq(n-1, 0)) - upper.tri(matrix(1, n, n)))[-n,])
  t(apply(m, 1, rev))
}

encode_helmert <- function(df, var) {
  x <- df[[var]]
  x <- unique(x)
  n <- length(x)
  d <- as.data.frame(helmert(n))
  d[[var]] <- rev(x)
  names(d) <- c(paste0(var, 1:(n-1)), var)
  d
}

d <- encode_helmert(df, "hml")
d
df[1, "hml"] <- NA_character_
d <- encode_helmert(df, "hml")
d

d <- data.frame(v = sample(c("a", "b", "c", "d", "e"), 100, replace = TRUE))
d <- encode_helmert(d, "v")
d
