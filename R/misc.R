#' Compute rolling mean
#'
#' @description Compute rolling mean over values. 
#' Optional: Fill truncated areas at the beginning or end of the sequences with 
#' mean of remaining values
#' @param x Values to be averaged
#' @param k k values to calculate the average on (default: 3)
#' @param align (default: "center")
#' @param fill_trunced Fill truncated areas (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
compute_roll_mean <- function(x, 
                              k = 3, 
                              align = "center", 
                              fill_trunced = T) {
  
  x_n <- length(x)
  y <- rep(NA, x_n)
  
  if (align %in% c("c", "center")) {

    i_from <- ceiling(k / 2)
    i_to <- x_n - floor(k / 2)
    for (i in i_from:i_to) {
      start <- i - i_from + 1
      end <- start + k - 1
      y[i] <- mean(x[start:end], na.rm = T)
    }
    
    if (fill_trunced) {
      
      # From start to center area
      for (i in 1:floor(k / 2)) {
        y[i] <- mean(x[1:i], na.rm = T)
      }
      
      # From center area to end
      for (i in (x_n - (floor(k / 2) - 1)):x_n) {
        y[i] <- mean(x[(i - 1):x_n], na.rm = T)
      }
    }
    
  }
  
  
  if (align %in% c("l", "left")) {
    
    for (i in 1:(x_n - k + 1)) {
      start <- i
      end <- i + k + 1
      y[i] <- mean(x[start:end], na.rm = T)
    }
    
    if (fill_trunced) {
      for (i in (x_n - (k - 1)):x_n) {
        y[i] <- mean(x[i:x_n], na.rm = T)
      }
    }
    
  }
  
  if (align %in% c("r", "right")) {
    
    for (i in k:x_n) {
      start <- i - k + 1
      end <- i
      y[i] <- mean(x[start:end], na.rm = T)
    }
    
    if (fill_trunced) {
      for (i in 1:(k - 1)) {
        y[i] <- mean(x[i:1], na.rm = T)
      }
    }
    
  }
  
  return(y)
  
}



# rollAvg <- function(dat, k = 3, align = "center", avg_trunc = T) {
#   
#   dat_rollavg <- rep(NA, length(dat))
#   
#   # Implementation of moving average with filling truncated areas
#   # First value remains the same
#   # Second to kth value is truncated, and averaged over values < k
#   
#   if (align == "left") {
#     for(i in 1:(length(dat)-k+1) ) {
#       start <- i
#       end <- (i+k-1)
#       dat_rollavg[i] <- mean(dat[start:end], na.rm = T)
#     }
#     
#     if (avg_trunc) {
#       for (i in (length(dat) - (k-1)):length(dat)) {
#         dat_rollavg[i] <- mean(dat[i:length(dat_rollavg)], na.rm = T)
#       }
#     }
#     
#   }
#   
#   if (align == "center") {
#     
#     if (avg_trunc) {
#       for (i in 1:floor(k/2)) {
#         dat_rollavg[i] <- mean(dat[1:i], na.rm = T)
#       }
#       
#       for (i in (length(dat) - (floor(k/2)-1)):length(dat)) {
#         dat_rollavg[i] <- mean(dat[(i-1):length(dat)], na.rm = T)
#       }
#     }
#     
#     from <- ceiling(k/2)
#     to <- length(dat) - floor(k/2)
#     for(i in from:to) {
#       start <- i - from + 1
#       end <- start + k - 1
#       dat_rollavg[i] <- mean(dat[start:end], na.rm = T)
#     }
#     
#     
#     
#   }
#   
#   if (align == "right") {
#     for(i in k:length(dat) ) {
#       start <- (i-k+1)
#       end <- i
#       dat_rollavg[i] <- mean(dat[start:end], na.rm = T)
#     }
#     
#     if (avg_trunc) {
#       for (i in 1:(k-1)) {
#         dat_rollavg[i] <- mean(dat[i:1], na.rm = T)
#       }
#     }
#     
#   }
#   
#   return(dat_rollavg)
# }




#' Normalize value
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
normalize <- function(x) {
  y = (x - min(x)) / (max(x) - min(x))
  return(y)
}




#' Find index for start of peak in values
#'
#' @param x 
#' @param plot 
#'
#' @return
#' @export
#'
#' @references 
#' https://stackoverflow.com/questions/42175710/finding-start-and-end-of-a-peak-in-time-series-in-r
#'
#' @examples
find_start_end_of_peak <- function(x, 
                                   plot = F, 
                                   min_gap_before = NULL,
                                   max_gap_before = NULL,
                                   min_gap_after = NULL,
                                   diff_threshold = NULL,
                                   find_start_end_of_peak = NULL,
                                   show_log = F) {
  
  # index of max value
  i.mx <- which.max(x)
  if (show_log) {
    text <- paste("Index of maximum value:", i.mx)
    cat(text, "\n")
  }
  
  # pad values with Inf and get indexes of all local minima
  t1 <- diff(c(Inf, x, Inf))
  if (!is.null(diff_threshold)) {
    finder <- which(abs(t1) < diff_threshold)
    t1[finder] <- NA
    #plot(x[t1])
  }
  t2 <- sign(t1)
  if (!is.null(diff_threshold)) {
    finder <- is.na(t2)
    t2[finder] <- -1
  }
  t3 <- diff(t2)
  #print(t3)
  
  i.mins <- which(t3 == 2)
  
  # Original
  #i.mins  <- which(diff(sign(diff(c(Inf, x, Inf)))) == 2)
  
  # Workaround in case there no 2 jumps (e.g. pos_id 38)
  flag1 <- F
  n_i.minx_below_i.mx <- length(which(i.mins <= i.mx))
  #print(n_i.minx_below_i.mx)
  #if (length(i.mins) == 0) {
  if (n_i.minx_below_i.mx == 1 & i.mins[1] == 1) {
    
    if (show_log) {
      cat("Searching for 1-diffs", "\n")
    }
    flag1 <-  T
    #i.mins <- which(abs(t3) == 1)
    
    #t1 <- diff(c(Inf, x, Inf))
    #finder <- which(t1 < 0.00001)
    #finder <- which(t1 < diff_threshold)
    #print(finder)
    #t1[finder] <- integer(1)
    #t1[finder] <- NA
    #print(finder)
    #print(t1)
    #plot(t1)
    #print(t1)
    #plot(t1)
    #t1[finder] <- NA
    #t2 <- sign(t1)
    #finder <- is.na(t2)
    #t2[finder] <- -1
    #print(t2)
    #plot(t2)
    #t3 <- diff(t2)
    #print(t3)
    #plot(t3)
    # Workaround in case there no 2 jumps (e.g. pos_id 38)
    i.mins <- which(abs(t3) == 1)
    #print(i.mins)
    
  }
  #i.mins  <- which(diff(sign(diff(c(Inf, x, Inf)))) == 2)
  #i.mins  <- which(diff(sign(diff(c(0, x, 0)))) == 2)
  
  
  
  
  # combine indexes of local minima and the max
  i <- sort(c(i.mins, i.mx))
  
  # select the two minima on either side of the max 
  
  # First 
  #ix1_temp <- i[which(i == i.mx) - 1]
  ix1_temp <- i
  #print(ix1_temp)
  
  if (!is.null(min_gap_before)) {
    
    ix1_temp <- ix1_temp[which(ix1_temp <= i.mx - min_gap_before)]
    #print(ix1_temp)
  }
  
  if (!is.null(max_gap_before)) {
    
    ix1_temp <- ix1_temp[which(ix1_temp <= i.mx & ix1_temp >= i.mx - max_gap_before)]
    #print(ix1_temp)
    if (length(ix1_temp) == 0) {
      ix1_temp <- i.mx - max_gap_before
    }
    
  }
  
  
  
  # if (!is.null(min_gap_before) & !is.null(max_gap_before)) {
  #   
  #   print("here")
  #   ix1_temp <- i[which( (i <= i.mx - min_gap_before) & (i <= i.mx & i >= i.mx - max_gap_before) )]
  #   
  # } else {
  #   
  #   # left minimum have minimum gap to the maximum
  #   if (!is.null(min_gap_before)) {
  #     
  #     ix1_temp <- i[which(i <= i.mx - min_gap_before)]
  #     #print(ix1_temp)
  #     
  #   } else {
  #     
  #     if (!is.null(max_gap_before)) {
  #       print("here2")
  #       ix1_temp <- i[which(i <= i.mx & i >= i.mx - max_gap_before)]
  #       
  #     } 
  #     
  #   }
  #   
  # }
  
  
  # Extract final value
  ix1_temp <- tail(ix1_temp, 1)
  #print(t1[ix1_temp])
  
  # right minimum have minimum gap to the maximum
  if (!is.null(min_gap_after)) {
    
    ix2_temp <- i[which(i >= i.mx + min_gap_after)]
    #print(ix2_temp)
    ix2_temp <- head(ix2_temp, 1)
    
  } else {
    
    ix2_temp <- i[which(i == i.mx) + 1]
    
  }
  
  #ix <- i[which(i == i.mx) + c(-1, 1)]  
  ix <- c(ix1_temp, ix2_temp)
  
  if (show_log) {
    text <- paste("Indices for surrounding minima:", paste(ix, collapse = ", "))
    cat(text, "\n") 
  }
  
  if (plot) {
    
    plot(x, type = "b")
    points(x = c(ix[1], i.mx, ix[2]),
           y = c(x[ix[1]], max(x), x[ix[2]]),
           col = c("blue", "red", "blue"), pch = 19, cex = 2)
    if (!is.null(min_gap_before)) {
      abline(v = i.mx - min_gap_before) 
    }
    if (!is.null(max_gap_before)) {
      abline(v = i.mx - max_gap_before) 
    }
  }
  
  
  dat_return <- list(i_min1 = ix[1], i_max = i.mx, i_min2 = ix[2], flag1 = flag1)
  #print(dat_return)
  #return(ix[1])
  return(dat_return)
  
}



#' Filter data section
#'
#' @param dat 
#' @param col_name_filter 
#' @param t1 
#' @param t2 
#' @param plot 
#'
#' @return
#' @export
#'
#' @examples
filter_dat_section <- function(dat, 
                               col_name_filter, 
                               t1 = NULL, 
                               t2 = NULL,
                               plot) {
  
  # Error handling
  if (is.null(t1) & is.null(t2)) {
    cat("Please specify at least one threshold t1 or t2")
  }
  
  if (is.null(t1)) {
    t1 <- min(dat[, col_name_filter])
  }
  
  if (is.null(t2)) {
    t1 <- max(dat[, col_name_filter])
  }
  
  # Filter data section
  finder <- which(dat[, col_name_filter] >= t1 & dat[, col_name_filter] <= t2)
  dat_section <- dat[finder, ]
  
  # Create plot
  if (plot) {
    
    dat_plot <- 
      ggplot() +
      # Full path
      geom_path(dat = dat,
                aes(x = gps_lon,
                    y = gps_lat),
                size = 1) +
      # Relevant section
      geom_path(dat = dat_section,
                aes(x = gps_lon,
                    y = gps_lat),
                color = "red",
                size = 1) + 
      # Starting point
      geom_point(dat = dat[1,],
                 aes(x = gps_lon,
                     y = gps_lat),
                 size = 5,
                 col = "green",
                 alpha = 0.5) +
      # Reference point
      geom_point(data = dat %>% filter(!!sym(col_name_filter) == 0),
                 aes(x = gps_lon,
                     y = gps_lat),
                 size = 5,
                 alpha = 0.5) + 
      ggtitle("Filtered data section")
    
    plot(dat_plot)
  }
  
  dat_return <- list(dat = dat_section, i_from = min(finder), i_to = max(finder))
  return(dat_return)
  
}



#' Compute distance to position
#'
#' @param x 
#' @param gps1 
#' @param gps2 
#'
#' @return
#' @export
#'
#' @examples
compute_pos_dist <- function(x, gps1, gps2) {
  
  dist_m <- c()
  for (i in 1:length(x)) {
    
    dist_m_temp <- geosphere::distHaversine(
      c(gps1[[1]][i], gps1[[2]][i]),
      c(gps2$gps_lon, gps2$gps_lat))
    
    dist_m <- c(dist_m, dist_m_temp)
    
  }
  
  index_min <- which(dist_m == min(dist_m))
  x_min <- x[index_min]
  pos_dist <- x - x_min
  
  return(pos_dist)
}



#' Create directory path
#' 
#'
#' @param dirs 
#'
#' @return
#' @export
#'
#' @examples
create_dir_path <- function(dirs, date = NULL) {
  
  path <- file.path(dirs[1])
  dir.create(file.path(path), showWarnings = F, recursive = T)
  for (dir in dirs[-1]) {
    path <- file.path(path, dir)
    if (!file.exists(path)) 
      dir.create(file.path(path), showWarnings = F, recursive = T)
  }

  return(path)
}