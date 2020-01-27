source("01_Synthetic_Data_Preparation.R")

# Using the binaryLogic package
library(binaryLogic)
encode_binary <- function(x, order = unique(x), name = "v_") {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x2 <- as.binary(x)
  maxlen <- max(sapply(x2, length))
  x2 <- lapply(x2, function(y) {
    l <- length(y)
    if (l < maxlen) {
      y <- c(rep(0, (maxlen - l)), y)
    }
    y
  })
  d <- as.data.frame(t(as.data.frame(x2)))
  rownames(d) <- NULL
  colnames(d) <- paste0(name, 1:maxlen)
  d
}

new_df <- cbind(df, encode_binary(df[["hml"]], name = "hml_"))
new_df <- cbind(new_df, encode_binary(df[["city"]], name = "city_"))
detach("package:binaryLogic", unload = TRUE)
