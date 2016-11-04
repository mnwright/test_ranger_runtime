
## Measure runtime on data sets and return table with runtimes in seconds
measure_runtime <- function(dats, scenarios, ...) {
  ## Runtime
  runtimes <- lapply(dats, function(dat) {
    microbenchmark(ranger = ranger(dependent.variable.name = "y", data = dat, 
                                   num.threads = 1, ...), 
                   times = 3, unit = "s")
  })
  
  ## Merge to data table
  res <- mapply(function(scenario, runtime) {
    data.frame(n = scenario["n"], p = scenario["p"], runtime, row.names = 1:nrow(runtime))
  }, scenarios, runtimes, SIMPLIFY = FALSE)
  dt <- rbindlist(res)
  
  ## Median over replicates
  median_time <- dt[, median(time)/10^9, by = list(n, p, expr)]
  tab <- dcast(median_time, n + p ~ expr, value.var = "V1")
  
  ## Name by current version
  colnames(tab)[ncol(tab)] <- paste0("ranger_", packageVersion("ranger"))
  
  ## Return table
  tab
}

## Load old runtimes
load_old_runtimes <- function(pattern) {
  ## Load old runtimes
  files <- list.files("save", pattern = pattern, full.names = TRUE)
  old_tabs <- lapply(files, readRDS)
  old_cols <- do.call(cbind, lapply(old_tabs, function(x) {
    x[, grep("ranger_.", colnames(x)), with = FALSE]
  }))
  old_cols <- old_cols[, colnames(old_cols) != grep("ranger_.", colnames(tab), value = TRUE), with = FALSE]
  old_cols
}

## Save new runtimes and load old runtimes
save_load <- function (tab, pattern) {
  ## Save to file
  saveRDS(tab, paste0("save/", pattern, "_", packageVersion("ranger"), ".Rds"))
  
  ## Load old runtimes
  old_cols <- load_old_runtimes(pattern)
  
  ## Merge
  if (ncol(old_cols) > 0) {
    cbind(tab[, list(n, p)], old_cols, tab[, grep("ranger_.", colnames(tab)), with = FALSE])
  } else {
    tab
  }
}

## Print table, highlight changes
print_table <- function(tab, diff, caption) {
  tab <- as.data.frame(tab)
  print_tab <- tab
  print_tab[, ncol(tab)] <- round(print_tab[, ncol(tab)], 2)
  colnames(print_tab) <- gsub("_", "\\\\_", colnames(print_tab))
  
  if (length(grep("ranger_.", colnames(tab))) >= 2) {
    ## Find rows with relative difference > diff
    slower <- tab[, ncol(tab)] > (1+diff) * tab[, (ncol(tab)-1)]
    faster <- tab[, ncol(tab)] < (1-diff) * tab[, (ncol(tab)-1)]
    
    ## Mark rows with difference
    if (sum(slower) > 0) {
      print_tab[slower, ncol(tab)] <- paste0("\\textcolor{red}{", print_tab[slower, ncol(tab)], "}")
    }
    if (sum(faster) > 0) {
      print_tab[faster, ncol(tab)] <- paste0("\\textcolor{green}{", print_tab[faster, ncol(tab)], "}")
    }
  }
  
  ## Show table
  print(xtable(print_tab, digits = c(0, 0, 0, rep(2, ncol(print_tab)-2)), 
               caption = caption), 
        sanitize.text.function = function(x) {x})
}
