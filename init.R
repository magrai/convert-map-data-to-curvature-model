library(ggplot2)
library(dplyr)
library(geosphere)
library(zoo)

source("R/curvature.R")
source("R/misc.R")
source("R/osm-data-collection.R")
source("R/osm-data-correction.R")

intrpldf <- 
  function (dat, colname4ref, min = NULL, max = NULL, stepsize = 1, 
            colnames2excl = NULL, binary_vars = NULL, showLog = F, colname_intrpld = "intrpld", 
            replace_preceding = T) 
  {
    #outputFunProc(R)
    if (is.null(min)) {
      min <- min(dat[, colname4ref], na.rm = T)
      min <- round(min, getDecimalPlaces(stepsize))
    }
    if (is.null(max)) {
      max <- max(dat[, colname4ref], na.rm = T)
      max <- round(max, getDecimalPlaces(stepsize))
    }
    template <- seq(min, max, stepsize)
    template <- round(template, getDecimalPlaces(stepsize))
    template <- as.character(template, str)
    template <- data.frame(template)
    colnames(template) <- colname4ref
    dat[, colname4ref] <- round(dat[, colname4ref], getDecimalPlaces(stepsize))
    dat[, colname4ref] <- as.character(dat[, colname4ref])
    dat <- dat %>% group_by_(colname4ref) %>% summarise_all("max") %>% 
      arrange_(colname4ref)
    dat <- left_join(template, dat, by = colname4ref)
    dat[, colname4ref] <- as.numeric(dat[, colname4ref])
    col_n <- ncol(dat)
    col_finder <- grep(colname4ref, colnames(dat), value = T, 
                       invert = T)[1]
    rows_w_dat <- which(!is.na(dat[, col_finder]))
    rows_w_dat_min <- min(rows_w_dat)
    rows_na <- which(is.na(dat[, col_finder]))
    rows_na_preceding <- rows_na[rows_na < min(rows_w_dat)]
    colnames_backup <- colnames(dat)
    colnames2excl <- c(colnames2excl, colname4ref)
    dat_new <- lapply(colnames(dat), function(currentcol) {
      if (!currentcol %in% colnames2excl) {
        if (replace_preceding & length(rows_na_preceding) > 
            0) {
          dat[1:(rows_w_dat_min - 1), currentcol] <- dat[rows_w_dat_min, 
                                                         currentcol]
        }
        if (is.numeric(dat[, currentcol]) & !currentcol %in% 
            binary_vars) {
          newvals <- zoo::na.approx(dat[, currentcol], 
                                    na.rm = F)
        }
        else {
          newvals <- zoo::na.locf(dat[, currentcol], na.rm = F)
        }
      }
      else {
        newvals <- dat[, currentcol]
      }
    })
    dat_new <- as.data.frame(dat_new, stringsAsFactors = F)
    colnames(dat_new) <- colnames_backup
    dat_new[, colname_intrpld] <- F
    dat_new[rows_na, colname_intrpld] <- T
    # if (showLog) {
    #   cat("* Row numbers before:", length(rows_w_dat), "\n")
    #   cat("* Row numbers after: ", length(rows_w_dat) + length(rows_na), 
    #       "\n")
    # }
    #outputDone()
    return(dat_new)
  }



getDecimalPlaces <- 
  function (values#, 
            #output = puttytat4R_env$outputFunProc_status
            ) 
  {
    #outputFunProc(R)
    options_backup <- options()
    options(scipen = 100)
    values <- values[which(!is.na(values))]
    decplaces <- sapply(values, function(x) {
      if ((x%%1) != 0) {
        x <- as.character(x)
        decplaces <- strsplit(sub("0+$", "", x), ".", fixed = TRUE)[[1]][[2]]
        decplaces <- nchar(decplaces)
      }
      else {
        decplaces <- 0
      }
      return(decplaces)
    })
    decplaces <- max(decplaces)
    options(options_backup)
    # if (output) {
    #   cat("* Number of decimal places: ", decplaces, "\n", sep = "")
    #   outputDone()
    # }
    return(decplaces)
  }