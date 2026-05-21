gini_standard <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA)
  x <- sort(as.numeric(x))
  n <- length(x)
  total <- sum(x)
  if (total == 0) return(NA)
  
  w <- x / total
  G <- (2 / n) * sum(w * seq(1, n)) - (n + 1) / n
  return(G)
}
