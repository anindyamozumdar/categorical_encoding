source("01_Synthetic_Data_Preparation.R")

backward_difference <- function(n) {
  m <- matrix(0:(n-1), nrow = n, ncol = n)
  m <- m + upper.tri(matrix(1, n, n))
  m2 <- matrix(-(n-1), n, n)
  m2[upper.tri(m2)] <- 0
  m <- (m + m2) / n
  m <- (t(m))[, -n]
  m
}

encode_backward_difference <- function(df, var) {
  x <- df[[var]]
  x <- unique(x)
  n <- length(x)
  d <- as.data.frame(backward_difference(n))
  d[[var]] <- x
  names(d) <- c(paste0(var, 1:(n-1)), var)
  d
}

d <- encode_backward_difference(df, "hml")
d

df[1, "hml"] <- NA_character_
d <- encode_backward_difference(df, "hml")
d

d <- data.frame(v = sample(c("a", "b", "c", "d", "e"), 100, replace = TRUE))
d <- encode_backward_difference(d, "v")
d
