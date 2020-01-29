source("01_Synthetic_Data_Preparation.R")

# https://stats.stackexchange.com/questions/411134/how-to-calculate-helmert-coding
# https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

helmert <- function(n) {
  m <- t((diag(seq(n-1, 0)) - upper.tri(matrix(1, n, n)))[-n,])
  t(apply(m, 1, rev))
}

encode_helmert <- function(df, var) {
  x <- df[[var]]
  x <- factor(x, exclude = NULL)
  d <- as.data.frame(helmert(length(levels(x))))
  d[[var]] <- rev(unique(x))
  names(d) <- c(paste0(var, 1:(length(levels(x)) - 1)), var)
  d
}

d <- encode_helmert(df, "hml")
d
df[1, "hml"] <- NA_character_
d <- encode_helmert(df, "hml")
d