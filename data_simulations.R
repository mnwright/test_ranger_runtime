
## Count number of odd digits - number of even digits per row
digit_data <- function(n, p, num_levels, type = "class") {
  x_num <- data.frame(replicate(p, sample(num_levels, n, replace = TRUE)))
  x_fac <- data.frame(lapply(x_num, as.factor))
  
  ## +1 for odd, -1 for even
  scores <- data.frame(lapply(x_num, function(x) {
    2*(x %% 2) - 1
  }))
  
  if (type == "reg") {
    y <- rowSums(scores)
  } else if (type == "class") {
    y <- factor(rowSums(scores) > 0)
  } else if (type == "surv") {
    stop("Survival not yet implemented.")
  } else {
    stop("Unknown type.")
  }
  
  data.frame(y, x_fac)
}

