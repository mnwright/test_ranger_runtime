
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
    time <- 100 * (rowSums(scores) - min(rowSums(scores)))
    status <- rbinom(n, 1, 0.75)
    y <- data.frame(time, status)
  } else {
    stop("Unknown type.")
  }
  
  data.frame(y, x_fac)
}

## Binary data without effects
binary_data <- function(n, p, type = "class") {
  x <- data.frame(replicate(p, rbinom(n, 1, 0.5)))
  
  if (type == "reg") {
    y <- runif(n, -5, 5)
  } else if (type == "class") {
    y <- factor(rbinom(n, 1, 0.5))
  } else if (type == "surv") {
    time <- 100 * rexp(n, rate = 0.5)
    status <- rbinom(n, 1, 0.75)
    y <- data.frame(time, status)
  } else {
    stop("Unknown type.")
  }
  
  data.frame(y, x)
}

## Continuous data without effects
cont_data <- function(n, p, type = "class") {
  x <- data.frame(replicate(p, rnorm(n, 0, 1)))
  
  if (type == "reg") {
    y <- runif(n, -5, 5)
  } else if (type == "class") {
    y <- factor(rbinom(n, 1, 0.5))
  } else if (type == "surv") {
    time <- 100 * rexp(n, rate = 0.5)
    status <- rbinom(n, 1, 0.75)
    y <- data.frame(time, status)
  } else {
    stop("Unknown type.")
  }
  
  data.frame(y, x)
}
