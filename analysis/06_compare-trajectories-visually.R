
plot_data <- 
  ggplot() + 
  geom_point(dat = dat_way_nodes_intrpl,
             aes(x = gps_lon,
                 y = gps_lat)) + 
  geom_point(dat = dat_way_nodes_limit,
             aes(x = gps_lon,
                 y = gps_lat),
             color = "red",
             alpha = 0.5) + 
  geom_point(data = dat_way_nodes_intrpl %>% filter(pos_dist_m == 0),
             aes(x = gps_lon,
                 y = gps_lat),
             size = 5) +
  geom_point(data = dat_driv_limit,
             aes(x = gps_lon,
                 y = gps_lat),
             col = "grey50") + 
  geom_point(data = dat_driv_limit %>% filter(pos_dist_m == 0),
             aes(x = gps_lon,
                 y = gps_lat),
             size = 5) +
  # End point of driving data
  geom_point(data = dat_driv_limit[nrow(dat_driv_limit),],
             aes(x = gps_lon,
                 y = gps_lat),
             col = "blue",
             size = 5)

plot(plot_data)