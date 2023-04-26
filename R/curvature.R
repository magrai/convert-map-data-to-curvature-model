#' Compute circle passing 2 pts
#'
#' @param pt1 
#' @param pt2 
#' @param r 
#'
#' @return
#' @export
#'
#' @references 
#' https://stackoverflow.com/questions/4914098/centre-of-a-circle-that-intersects-two-points
#' https://lydxlx1.github.io/blog/2020/05/16/circle-passing-2-pts-with-fixed-r/
#'
#' @examples
compute_circle_2pt <- function(pt1, pt2, r) {
  
  x1 <- pt1[1]
  y1 <- pt1[2]
  
  x2 <- pt2[1]
  y2 <- pt2[2]
  
  q = sqrt( (x2 - x1 )^2 + (y2 - y1)^2 )
  
  x3 = (x1 + x2)/2
  y3 = (y1 + y2)/2
  
  # Circle 1
  circle1_x = x3 + sqrt(r^2 - (q/2)^2) * (y1 - y2)/q
  circle1_y = y3 + sqrt(r^2 - (q/2)^2) * (x2 - x1)/q  
  
  # Circle 2
  circle2_x = x3 - sqrt(r^2 - (q/2)^2) * (y1 - y2)/q
  circle2_y = y3 - sqrt(r^2 - (q/2)^2) * (x2 - x1)/q 
  
  output <- list(
    circle1 = c(x = circle1_x, y = circle1_y),
    circle2 = c(x = circle2_x, y = circle2_y)
  )
  
  return(output)
  
}



#' Compute circle passing three points
#'
#' @param pt1 Vector of first point coordinates
#' @param pt2 Vector of second point coordinates
#' @param pt3 Vector of third point coordinates
#'
#' @return List including values for center and radius
#' 
#' @importFrom matlib gaussianElimination
#' @export
#'
#' @examples
#' pt1 <- c(-37.901658, 145.234691)
#' pt2 <- c(-37.904875, 145.237030)
#' pt3 <- c(-37.908769, 145.236665)
#' results <- compute_three_point_radius(pt1, pt2, pt3)
#' print(results$center)
#' print(results$radius)
compute_circle_3pt <- function(pt1, pt2, pt3) {
  
  # Extract points into x and y values
  x1 <- pt1[1]
  y1 <- pt1[2]
  
  x2 <- pt2[1]
  y2 <- pt2[2]
  
  x3 <- pt3[1]
  y3 <- pt3[2]
  
  # Coefficients
  x_coeff <- c(x2 - x1, y2 - y1)
  y_coeff <- c(x3 - x1, y3 - y1)
  coefficients <- matrix(c(x_coeff, y_coeff), byrow = T, nrow = 2, ncol = 2)
  
  # Constants
  constants <- c(
    (x2^2 + y2^2 - x1^2 - y1^2) / 2,
    (x3^2 + y3^2 - x1^2 - y1^2) / 2
  )
  
  # Apply Gaussian elimination to solve equation and find the center
  center <- matlib::gaussianElimination(coefficients, constants)
  center <- c(center[1,3], center[2,3])
  
  # Compute radius
  radius <- sqrt( (x1 - center[1])^2 + (y1 - center[2])^2 )
  
  output <- list(
    center = center,
    radius = radius
  )
  
  return(output)
  
}



# test_data <- dat_coords_limit[c(1, 101, 201),]
# pt1 <- c(test_data$gps_lon[1], test_data$gps_lat[1])
# pt2 <- c(test_data$gps_lon[2], test_data$gps_lat[2])
# pt3 <- c(test_data$gps_lon[3], test_data$gps_lat[3])
# results <- compute_circle_3pt(pt1, pt2, pt3)
# print(results$center)
# print(results$radius)
# 
# data_points <-  data.frame(
#   x = c(pt1[1], pt2[1], pt3[1]),
#   y = c(pt1[2], pt2[2], pt3[2]))
# plot(data_points)
# data_circle <- compute_circle_values(results$center, results$radius)
# lines(data_circle, col = "blue")
# 
# 
# ### Good till here
# 
# dat_coords_curv <- batch(dat_coords_limit, 10, 10)
# 
# plot(dat_coords_curv$gps_lon, dat_coords_curv$gps_lat)
# index_min <- which(dat_coords_curv$pos_dist_m == min(abs(dat_coords_curv$pos_dist_m)))
# points(dat_coords_curv$gps_lon[index_min], dat_coords_curv$gps_lat[index_min], col="red")
# 
# 
# 
# circle_values <- compute_circle_values(
#   c(dat_coords_curv$circle_x[index_min],
#     dat_coords_curv$circle_y[index_min]),
#   dat_coords_curv$radius[index_min]
# )
# 
# plot(circle_values[[1]], circle_values[[2]], type = "l")
# 
# dat_test <- dat_coords_curv[c(1100:1300), ]
# 
# i = 110
# 
# plot(dat_test$gps_lon, dat_test$gps_lat, 
#      xlim = c(11.64645, 11.64648),
#      ylim = c(48.10358, 48.10360))
# points(dat_test$gps_lon[i], dat_test$gps_lat[i], col = "red", lwd = 5)
# 
# radius = dat_test$radius[i]
# center_x = dat_test$circle_x[i]
# center_y = dat_test$circle_y[i]
# theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
# 
# x_circle = radius * cos(theta) + center_x
# y_circle = radius * sin(theta) + center_y
# plot(x_circle, y_circle, col = "green")
# lines(x_circle, y_circle, col = "green")




#' Compute path radius
#'
#' @param x Values for x-coordinates
#' @param y Values for y-coordinates
#' @param step Gap between value pairs to compute radius on
#'
#' @return

#' @importFrom zoo na.locf0
#' @export
#'
#' @examples
compute_path_radius <- function(x, 
                                y, 
                                step1 = 50, 
                                step2 = 50, 
                                fill_missing = T) {
  
  x_n <- length(x)
  
  # Initialize radius
  c <- data.frame(x = rep(NA, x_n), y = rep(NA, x_n))
  r <- rep(NA, x_n)
  
  ## For all rows: Extract three points and compute radius
  for (i in (step1 + 1):(x_n - step2)) {
    
    #print(i)
    
    # Select points according to step size    
    ids <- c(i - step1, i, i + step2)
    x_temp <- x[ids]
    y_temp <- y[ids]
    
    # Deprecated: Workaround using circumcircle
    #r[i] <- tripack::circumcircle(x_temp, y_temp)$radius
    
    pt1 <- c(x_temp[1], y_temp[1])
    pt2 <- c(x_temp[2], y_temp[2])
    pt3 <- c(x_temp[3], y_temp[3])
    
    circle <- compute_circle_3pt(pt1, pt2, pt3)
    #print(circle$center)
    c[i,] <- circle$center
    #print(circle$center)
    r[i] <- circle$radius
    
    # print(pt1)
    # print(pt2)
    # print(pt3)
    # print(circle)
    
    # plot(x, y)
    # points(x_temp, y_temp, col = "red")
    # 
    # xx <- rbind(pt1, pt2, pt3)
    # plot(xx)
    # 
    # dat_circle <- compute_circle_values(circle$center, circle$radius)
    # #plot(dat_circle$x)
    # #plot(dat_circle$y)
    # plot(dat_circle)
    # print(pt1)
    # #plot(pt1[1], pt1[2], col = "red", cex = 1)
    # points(pt2[1], pt2[2], col = "red", cex = 3)
    # lines(dat_circle$x, dat_circle$y, col = "green")
    # 
    # readline(prompt="Press [enter] to continue")
  }
  
  r_avg <- rep(NA, x_n)
  #if (average) {
  
  for (i in (step1 + 1):(x_n - step2)) {
    #ids <- c(i - step1, i, i + step2)
    ids <- c(i - step1, i)
    #ids <- c((i - step1):i)
    #ids <- c((i - step1):(x_n - step2))
    r_avg[i] <- mean(r[ids])
  }
  
  
  # Fill missing values with previous or succeeding values
  if (fill_missing) {
    
    c$x[1:step1] <- NA
    c$x[(x_n - step2):x_n] <- NA
    c$x <- zoo::na.locf0(c$x, fromLast = T)
    c$x <- zoo::na.locf0(c$x)
    
    c$y[1:step1] <- NA
    c$y[(x_n - step2):x_n] <- NA
    c$y <- zoo::na.locf0(c$y, fromLast = T)
    c$y <- zoo::na.locf0(c$y)
    
    r[1:step1] <- NA
    r[(x_n - step2):x_n] <- NA
    r <- zoo::na.locf0(r, fromLast = T)
    r <- zoo::na.locf0(r)
    
    r_avg[1:step1] <- NA
    r_avg[(x_n - step2):x_n] <- NA
    r_avg <- zoo::na.locf0(r_avg, fromLast = T)
    r_avg <- zoo::na.locf0(r_avg)
  }
  
  
  
  #}
  
  #print(c)
  output <- list(center = c, radius = r, radius_avg = r_avg)
  return(output)
}




#' Compute path curvature
#'
#' @param dat 
#' @param step1_radius 
#' @param step2_radius 
#' @param normalize
#' @param k_roll_mean 
#' @param k_align 
#'
#' @return
#' @export
#'
#' @examples
compute_path_curv <- function(dat, 
                              step1_radius = 50, 
                              step2_radius = 50, 
                              normalize = T,
                              k_roll_mean = NULL, 
                              k_align = "center",
                              average = T,
                              plot = F) {
  
  circle = compute_path_radius(
    dat$gps_lon, 
    dat$gps_lat, 
    step1 = step1_radius,
    step2 = step2_radius)
  
  #print(circle$center[[1]])
  #print(circle$center[2])
  
  dat$circle_center_x <- circle$center$x
  dat$circle_center_y <- circle$center$y
  dat$radius <- circle$radius
  dat$radius_avg <- circle$radius_avg
  compute_curvature_and_ref_pos
  if (!average) {
    dat$curv <- 1 / dat$radius
  } else {
    dat$curv <- 1 / dat$radius_avg
  }
  
  if (normalize) {
    dat$curv_scaled <- normalize(dat$curv)
    col_name_to_smooth <- "curv_scaled"
  } else {
    col_name_to_smooth <- "curv"
  }
  
  #dat$curv_scaled <- dat$curv
  if (!is.null(k_roll_mean)) {
    
    dat$curv_smoothed <- compute_roll_mean(dat[, col_name_to_smooth], 
                                           k = k_roll_mean,
                                           align = k_align)
    
  }
  
  #dat$curv_smoothed <- 
  #  compute_roll_mean(dat$curv, k = k_roll_mean, align = k_align)
  
  #plot(dat$pos_dist_m, dat$radius, type = "l")
  #plot(dat$pos_dist_m, dat$curv, type = "l")
  #plot(dat$pos_dist_m, dat$curv_smoothed, type = "l", ylim = c(0, 10000))
  
  
  if (plot) {
    
    plot(dat$curv, type = "l")
    #lines(dat_curv$curv_smoothed, col = "red")
    lines(dat$curv_smoothed, col = "red")
    
  }
  
  return(dat)
  
}
# compute_path_curvature <- function(x
#                                    #x_max = NULL
#                                    #k = NULL,
#                                    #normalize = TRUE
#                                    ) {
#   
#   # Limit value range
#   # if (!is.null(x_max)) {
#   #   x[which(x > x_max)] <- x_max
#   # }
#     
#   # Smooth values
#   # if (!is.null(k)) {
#   #   x <- liebner2013::compute_roll_mean(x)
#   # }
#   # 
#   # Compute curvature
#   curv <- 1 / x
#   #i <- is.infinite(curv)
#   #curv[i] <- NA
# 
#   # Normalize curvature
#   # if (normalize) {
#   #   scale(curv, scale = T)[,1]
#   # }
#   # 
#   return(curv)
# }



#' Get three equally distant points from data
#'
#' @param dat 
#' @param pos_dist 
#' @param step 
#'
#' @return
#' @export
#'
#' @examples
get_3pts <- function(dat, pos_dist, step1, step2) {
  
  index <- which(dat$pos_dist_m == pos_dist)
  indices <- c(index - step1, index, index + step2)
  dat_selected <- dat[indices,]
  
  pt1 <- c(dat_selected$gps_lon[1], dat_selected$gps_lat[1])
  pt2 <- c(dat_selected$gps_lon[2], dat_selected$gps_lat[2])
  pt3 <- c(dat_selected$gps_lon[3], dat_selected$gps_lat[3])
  
  output <- list(pt1 = pt1, pt2 = pt2, pt3 = pt3)
  
  return(output)
}





#' Draw circle at distance to position
#'
#' @param dat 
#' @param pos_dist 
#' @param step 
#'
#' @return
#' @export
#'
#' @examples
draw_circle_at_pos_dist <- function(dat, pos_dist, step1, step2, 
                                    x_lim = NULL,
                                    y_lim = NULL) {
  
  index <- which(dat_driv_limit$pos_dist_m == pos_dist)
  indices <- c(index - step1, index, index + step2)
  dat_selected <- dat[indices,]
  
  dat_points <- get_3pts(dat, pos_dist, step1, step2)
  
  pt1 <- dat_points$pt1
  pt2 <- dat_points$pt2
  pt3 <- dat_points$pt3
  
  circle <- compute_circle_3pt(pt1, pt2, pt3)
  dat_circle <- data.frame(
    x0 = circle$center[1],
    y0 = circle$center[2],
    r = circle$radius)
  
  data_plot <- 
    ggplot2::ggplot() + 
    ggplot2::geom_point(data = dat,
                        aes(x = gps_lon,
                            y = gps_lat)) + 
    ggplot2::geom_point(data = dat %>% filter(pos_dist_m == 0),
                        aes(x = gps_lon,
                            y = gps_lat),
                        size = 5) + 
    ggplot2::geom_point(data = dat_selected,
                        aes(x = gps_lon,
                            y = gps_lat),
                        size = 4,
                        col = "red") +
    ggforce::geom_circle(data = dat_circle,
                         aes(x0 = x0,
                             y0 = y0,
                             r = r),
                         col = "red") 
  
  if (!is.null(x_lim)) {
    data_plot <- 
      data_plot + 
      ggplot2::scale_x_continuous(limits = x_lim)
  }
  
  if (!is.null(y_lim)) {
    data_plot <- 
      data_plot + 
      ggplot2::scale_y_continuous(limits = y_lim)
  }
  
  plot(data_plot)
  
}



#' Find start of curvature peak
#'
#' @param dat 
#' @param col_name_filter 
#' @param col_name_raw 
#' @param col_name_proc 
#' @param t1 
#' @param t2 
#' @param plot 
#' @param return_complete 
#'
#' @return
#' @export
#'
#' @examples
find_start_of_curv_peak <- function(dat,
                                    col_name_filter,
                                    col_name_raw = "curv",
                                    col_name_proc = "curv_smoothed",
                                    t1 = -50,
                                    t2 = 50,
                                    plot = T,
                                    min_gap_before = NULL,
                                    max_gap_before = NULL,
                                    diff_threshold = NULL,
                                    adj_factor = 3,
                                    return_complete = T) {
  
  # Filter relevant section to avoid other turnings to be taken into account
  dat_section <- 
    filter_dat_section(dat, 
                       col_name_filter = col_name_filter, 
                       t1 = t1, 
                       t2 = t2, 
                       plot = plot)
  
  # Save minimum index of extracted data section
  # (required for adjusting identified positions for original data set)
  i_section_from <- dat_section$i_from
  dat_section <- dat_section$dat
  
  # Find index for start of curvature peak for values and processes values
  finder_raw <- 
    find_start_end_of_peak(dat_section[, col_name_raw],
                           plot = plot,
                           min_gap_before = min_gap_before,
                           max_gap_before = max_gap_before,
                           diff_threshold = diff_threshold)
  finder_proc <- 
    find_start_end_of_peak(dat_section[, col_name_proc], 
                           plot = plot, 
                           min_gap_before = min_gap_before,
                           max_gap_before = max_gap_before,
                           diff_threshold = diff_threshold)
  
  # Adjusted position for start of curvature
  # (this workaround is based on experience and face validity)
  finder_adj_min1 <- (finder_raw$i_min1 - finder_proc$i_min1)  / adj_factor
  finder_adj_min1 <- finder_proc$i_min1 + finder_adj_min1
  finder_adj_min1 <- ceiling(finder_adj_min1)
  
  # Visualize identified positions
  if (plot) {
    
    plot(dat_section[, col_name_raw], type = "l")
    lines(dat_section[, col_name_proc], col = "red")
    abline(v = finder_raw$i_min1, col = "darkblue")
    abline(v = finder_proc$i_min1, col = "orange")
    abline(v = finder_adj_min1, col = "purple")
    
  }
  
  dat_return <- 
    list(dat_section = dat_section, 
         i_curv_raw_start = finder_raw$i_min1, 
         i_curv_raw_max = finder_raw$i_max, 
         i_curv_smoothed_start = finder_proc$i_min1, 
         i_curv_start_adj = finder_adj_min1)
  
  # If results should be adjusted to original data set
  if (return_complete) {
    
    # Adjust indices for original data set
    finder_raw$i_min1 <- finder_raw$i_min1 + min(i_section_from) - 1
    finder_raw$i_max <- finder_raw$i_max + min(i_section_from) - 1
    finder_raw$i_min2 <- finder_raw$i_min2 + min(i_section_from) - 1
    
    finder_proc$i_min1 <- finder_proc$i_min1 + min(i_section_from) - 1
    finder_proc$i_max <- finder_proc$i_max + min(i_section_from) - 1
    finder_proc$i_min2 <- finder_proc$i_min2 + min(i_section_from) - 1
    finder_adj_min1 <- finder_adj_min1 + min(i_section_from) - 1
    
    # Visualize identified positions in original data set
    if (plot) {
      
      plot(dat[, col_name_raw], type = "l")
      lines(dat[, col_name_proc], col = "red")
      abline(v = finder_raw$i_min1, col = "darkblue")
      abline(v = finder_proc$i_min1, col = "orange")
      abline(v = finder_adj_min1, col = "purple")
      
    }
    
    dat_return <- 
      list(dat_section = dat_section, 
           i_curv_raw_start = finder_raw$i_min1, 
           i_curv_raw_max = finder_raw$i_max, 
           i_curv_smoothed_start = finder_proc$i_min1, 
           i_curv_start_adj = finder_adj_min1)
    
  }
  
  return(dat_return)
  
}





#' Compute curvature and reference position
#'
#' @param pos_id 
#' @param plot 
#'
#' @return
#' @export
#'
#' @examples
compute_curvature_and_ref_pos <- function(pos_id, plot = F) {
  
  # Preprocess OSM data
  #text <- paste("i:", i, "\n") 
  #cat(text)
  dat <- preprocess_osm_data(pos_id, viz_order = plot, viz_reduction = plot, viz_distance = plot)
  dat <- cbind(pos_id, dat)
  #dat_coll <- rbind(dat_coll, dat)
  
  
  # Interpolate OSM data
  step_size = 0.1
  col_names_exclude <- c("element_no", "node_no", "way_id")
  
  #dat_temp <- dat_coll %>% filter(pos_id == pos_id)
  dat_temp <- dat
  
  dat_temp2 <- 
    intrpldf(dat_temp, 
             "gps_dist_m_cum", 
             stepsize = step_size,
             colnames2excl = col_names_exclude, colname_intrpld = NULL)
  
  for (c in col_names_exclude) {
    dat_temp2[, c] <- zoo::na.locf(dat_temp2[, c])
  }
  
  
  # Compute distance to reference point
  # Load position data for reference point
  query <- "SELECT gps_lon, gps_lat FROM t_positions WHERE pos_id ="
  query <- paste(query, pos_id)
  dat_ref_pos <- dbGetQuery(get(db_conn_name), query)
  
  # Compute distance to reference point
  dat_temp2$pos_dist_m <- 
    compute_pos_dist(
      dat_temp2$gps_dist_m_cum,
      list(dat_temp2$gps_lon, dat_temp2$gps_lat),
      dat_ref_pos
    )
  
  
  # Compute curvature
  dat_curv <- 
    compute_path_curv(
      dat_temp2, 
      step1_radius = 100, 
      step2_radius = 100, 
      normalize = F, 
      k_roll_mean = 100, 
      plot = plot)
  
  
  # Find start of curvature peak
  dat_curv_start <- 
    find_start_of_curv_peak(dat_curv, 
                            col_name_filter = "pos_dist_m", 
                            t1 = -50, 
                            t2 = 50, 
                            plot = plot,
                            min_gap_before = 10,
                            max_gap_before = 200,
                            diff_threshold = 0.000000001,
                            return_complete = T)
  
  
  
  
  # Visualize start of curvature peak on map data
  if (plot) {
  
    # Configure path information for obtaining map data
    file_dir <- "data"
    file_name_plain <- paste0("ggmap_pos_id_", sprintf("%02d", pos_id))
    file_name <- paste0(file_name_plain, ".rds")
    file_path <- file.path(file_dir, file_name)
    
    # If position is not found in map data repository use API to load map data
    if (!file_name %in% list.files(file_dir)) {
      
      cat("Loading map via API and saving data to data directory", "\n")
      
      dat_map <- ggmap::get_map(
        location = c(
          dat_curv$gps_lon[dat_curv_start$i_curv_raw_max], 
          dat_curv$gps_lat[dat_curv_start$i_curv_raw_max]),
        source = "google",
        zoom = 18,
        maptype = "satellite")
      
      base::saveRDS(dat_map, file_path)
      
    } else {
      
      cat("Loading map from disk", "\n")
      dat_map <- base::readRDS(file_path)
      
    }
    
    
    dat_plot <- 
      ggmap(dat_map) +
      # Starting point
      geom_point(data = dat_curv %>% filter(pos_dist_m == -20),
                 aes(x = gps_lon,
                     y = gps_lat),
                 size = 5,
                 alpha = 0.5,
                 color = "green") +
      # Reference point
      geom_point(data = dat_curv %>% filter(pos_dist_m == 0),
                 aes(x = gps_lon,
                     y = gps_lat),
                 size = 5,
                 alpha = 0.5,
                 color = "red") + 
      geom_point(data = dat_curv[dat_curv_start$i_curv_raw_start,],
                 aes(x = gps_lon, 
                     y = gps_lat),
                 size = 3,
                 alpha = 0.5,
                 col = "blue") + 
      geom_point(data = dat_curv[dat_curv_start$i_curv_smoothed_start,],
                 aes(x = gps_lon,
                     y = gps_lat),
                 size = 3,
                 alpha = 0.5,
                 col = "orange") + 
      geom_point(data = dat_curv[dat_curv_start$i_curv_start_adj,],
                 aes(x = gps_lon,
                     y = gps_lat),
                 size = 3,
                 alpha = 0.5,
                 col = "purple")
    
    #plot(dat_plot) 
    
    file_name <- paste0(file_name_plain, ".png")
    file_path <- file.path("output/", file_name)
    plot(dat_plot)
    #ggsave(file_path, plot = dat_plot, width = 1600, height = 1600, units = "px")
  }
  
  # Extract data for new reference point
  ref_pos <- c()
  ref_pos$gps_lat <- dat_curv$gps_lat[dat_curv_start$i_curv_start_adj]
  ref_pos$gps_lon <- dat_curv$gps_lon[dat_curv_start$i_curv_start_adj]
  
  dat_return <- list(dat_curv = dat_curv, ref_pos = ref_pos)
  return(dat_return)
  
}




#' Demonstration: Compute path curvature
#'
#' @param dat 
#' @param from 
#' @param to 
#' @param step 
#'
#' @return
#' @export
#'
#' @examples
compute_path_curv_demo <- function(dat, from, to, step = 50, step1 = 50, step2 = 50) {
  
  for (i in seq(from, to, step)) {
    draw_circle_at_pos_dist(dat, pos_dist = i, step1 = step1, step2 = step2)
    readline(prompt="Press [enter] to continue")
  }
  
}
