
# Preparatory settings ----------------------------------------------------

text_subtitle_nodes <- 
  "Note: Increasing size of nodes represents order of nodes \n\n"


sett_plot <- c()
sett_plot$title$size <- 11
sett_plot$title$face <- "bold"
sett_plot$subtitle$size <- 10
sett_plot$subtitle$face = "plain"
sett_plot$axis$title$size <- 10
sett_plot$axis$text$size <- 9
sett_plot$axis$text$color <- "black"
  sett_plot$legend$title$size <- 9
  sett_plot$legend$text$size <- 9


  
# Load data ---------------------------------------------------------------

dat_coords <- read.csv2("data/dat_node_coordinates.csv")



# Visualize order of nodes ------------------------------------------------

plot_dat <- 
  ggplot() +
  # geom_line(dat = dat_coords,
  #           aes(x = gps_lon,
  #               y = gps_lat,
  #               color = as.factor(way_id)),
  #           alpha = 0.5) +
  geom_point(dat = dat_coords,
             aes(x = gps_lon,
                 y = gps_lat,
                 size = node_no,
                 color = as.factor(way_id)),
             alpha = 0.5) + 
  coord_cartesian(xlim = c(11.647, 11.651),
                  ylim = c(48.080, 48.085))



# Post-process plot -------------------------------------------------------

plot_dat_post <- 
  plot_dat +
  ggtitle("Original order of nodes",
          subtitle = text_subtitle_nodes) + 
  labs(x = "Longitude",
       y = "Latitude") + 
  scale_color_manual(name = "Way IDs:", 
                     values = c("red2", "blue", "green3")) + 
  guides(size = "none") + 
  
  theme_bw() + 
  theme(legend.position="top",
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-15,-10,-8,0)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  theme(title = element_text(size = sett_plot$title$size, 
                             face = sett_plot$title$face),
        plot.subtitle = element_text(size = sett_plot$subtitle$size, 
                                     face = sett_plot$subtitle$face)) +
  theme(axis.title.x = element_text(size = sett_plot$axis$title$size), 
        axis.title.y = element_text(size = sett_plot$axis$title$size)) +
  theme(axis.text.x = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color),
        axis.text.y = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color)) +
  theme(legend.title = element_text(size = sett_plot$legend$title$size),
        legend.text = element_text(size = sett_plot$legend$text$size)) +
  theme(text = element_text(#size = 7, 
                            family = "Arial"))

plot(plot_dat_post)



# Save plot ---------------------------------------------------------------

ggsave("original_gps_coordinates.png", 
       plot = plot_dat_post,
       width = 12,
       height = 12,
       units = "cm",
       dpi = 600,
       path = "output")



# Identify inverted way segments ------------------------------------------

# Initialize new dataframe
dat_coords_to_inv <- data.frame(
  way_id = unique(dat_coords$way_id),
  to_inv = F
)

# Extract required information:
# Unique way ids
dat_ids <- unique(dat_coords$way_id)
# Number of unique way ids
dat_ids_n <- length(dat_ids)

# Check whether the last node of the first way segment matches ...
# ... the first node of the second way segment
# If not: Flag way segment to be inverted
# Note: Nodes are referred to via reference id

# Get reference ids for first way segment
way_id_finder <- which(dat_coords$way_id == dat_ids[1])
ref_ids_curr <- dat_coords$ref_id[way_id_finder]

# Get reference ids for second way segment
way_id_finder <- which(dat_coords$way_id == dat_ids[1+1])
ref_ids_next <- dat_coords$ref_id[way_id_finder]

# Find matching elements
ref_id_finder_curr <- which(ref_ids_curr %in% ref_ids_next)
if (ref_id_finder_curr != length(ref_ids_curr)) {
  dat_coords_to_inv$to_inv[1] <- T
}



# From the second onwards: 
# For the current element to the following element, check whether the first ... 
# ...node of the current node is matching one of the nodes of the previous node
for (i in 2:(dat_ids_n)) {
  
  way_id_finder <- which(dat_coords$way_id == dat_ids[i-1])
  ref_ids_prev <- dat_coords$ref_id[way_id_finder]
  
  way_id_finder <- which(dat_coords$way_id == dat_ids[i])
  ref_ids_curr <- dat_coords$ref_id[way_id_finder]
  
  ref_id_finder_curr <- which(ref_ids_curr %in% ref_ids_prev)
  #print(ref_id_finder_curr)
  
  if (ref_id_finder_curr != 1) {
    dat_coords_to_inv$to_inv[i] <- T
  } 
}



# Invert flagged way segments ---------------------------------------------

# Invert node numbers
dat_coords_corr <- 
  left_join(dat_coords, dat_coords_to_inv, by = "way_id") %>% 
  group_by(way_id) %>% 
  #mutate(node_no_backup = node_no) %>% 
  mutate(node_no = ifelse(to_inv, abs(node_no - max(node_no)) + 1, node_no)) %>% 
  arrange(element_no, node_no) %>%
  select(-to_inv) %>% 
  data.frame()



# Visualize order of nodes ------------------------------------------------

# Check order of nodes
plot_dat <- 
  ggplot() +
  # geom_line(dat = dat_coords_corr,
  #           aes(x = gps_lon,
  #               y = gps_lat,
  #               color = as.factor(way_id)),
  #           alpha = 0.5,
  #           linetype = "dashed") +
  geom_point(dat = dat_coords_corr,
             aes(x = gps_lon,
                 y = gps_lat,
                 size = node_no,
                 color = as.factor(way_id)),
             alpha = 0.5) + 
  coord_cartesian(xlim = c(11.647, 11.651),
                  ylim = c(48.080, 48.085))



# Post-process plot -------------------------------------------------------

plot_dat_post <- 
  plot_dat +
  ggtitle("Sorted order of nodes",
          subtitle = text_subtitle_nodes) + 
  labs(x = "Longitude",
       y = "Latitude") + 
  scale_color_manual(name = "Way IDs:", 
                     values = c("red2", "blue", "green3")) + 
  guides(size = "none") + 
  
  theme_bw() + 
  theme(legend.position="top",
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-15,-10,-8,0)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  theme(title = element_text(size = sett_plot$title$size, 
                             face = sett_plot$title$face),
        plot.subtitle = element_text(size = sett_plot$subtitle$size, 
                                     face = sett_plot$subtitle$face)) +
  theme(axis.title.x = element_text(size = sett_plot$axis$title$size), 
        axis.title.y = element_text(size = sett_plot$axis$title$size)) +
  theme(axis.text.x = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color),
        axis.text.y = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color)) +
  theme(legend.title = element_text(size = sett_plot$legend$title$size),
        legend.text = element_text(size = sett_plot$legend$text$size)) +
  theme(text = element_text(#size = 7, 
    family = "Arial"))

plot(plot_dat_post)



# Save plot ---------------------------------------------------------------

ggsave("sorted_nodes.png", 
       plot = plot_dat_post,
       width = 12,
       height = 12,
       units = "cm",
       dpi = 600,
       path = "output")



# Check for unneeded nodes ------------------------------------------------

# For each way segment, check for unneded nodes
for (i in 1:(dat_ids_n-1)) {
  print(paste("i", i))
  
  way_id_finder <- which(dat_coords_corr$way_id == dat_ids[i])
  ref_ids_curr <- dat_coords_corr$ref_id[way_id_finder]
  
  way_id_finder <- which(dat_coords_corr$way_id == dat_ids[i+1])
  ref_ids_next <- dat_coords_corr$ref_id[way_id_finder]
  ref_ids_next_first <- ref_ids_next[1]
  
  ref_id_finder_curr <- which(ref_ids_curr == ref_ids_next_first)
  print(ref_id_finder_curr)
  
  ref_ids_curr_n <- length(ref_ids_curr)
  if (ref_id_finder_curr != ref_ids_curr_n) {
    indices_to_remove <- c((ref_id_finder_curr+1):ref_ids_curr_n)
    ref_ids_curr_to_remove <- ref_ids_curr[indices_to_remove]
    ref_id_finder <- which(dat_coords_corr$ref_id %in% ref_ids_curr_to_remove)
    print(ref_id_finder)
    #dat_coords_corr[ref_id_finder,] <- NULL
    dat_coords_corr <- dat_coords_corr[-c(ref_id_finder),]
  } 
}



# Visualize nodes ---------------------------------------------------------

plot_dat  <- 
  ggplot() +
  # geom_line(dat = dat_coords_corr,
  #           aes(x = gps_lon,
  #               y = gps_lat,
  #               color = as.factor(way_id)),
  #           alpha = 0.5) +
  geom_point(dat = dat_coords_corr,
             aes(x = gps_lon,
                 y = gps_lat,
                 size = node_no,
                 color = as.factor(way_id)),
             alpha = 0.5) + 
  coord_cartesian(xlim = c(11.647, 11.651),
                  ylim = c(48.080, 48.085))



# Post-process plot -------------------------------------------------------

plot_dat_post <- 
  plot_dat +
  ggtitle("Removal of uneeded nodes",
          subtitle = text_subtitle_nodes) + 
  labs(x = "Longitude",
       y = "Latitude") + 
  scale_color_manual(name = "Way IDs:", 
                     values = c("red2", "blue", "green3")) + 
  guides(size = "none") + 
  guides(size = "none") + 
  theme_bw() + 
  theme(legend.position="top",
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-15,-10,-8,0)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  theme(title = element_text(size = sett_plot$title$size, 
                             face = sett_plot$title$face),
        plot.subtitle = element_text(size = sett_plot$subtitle$size, 
                                     face = sett_plot$subtitle$face)) +
  theme(axis.title.x = element_text(size = sett_plot$axis$title$size), 
        axis.title.y = element_text(size = sett_plot$axis$title$size)) +
  theme(axis.text.x = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color),
        axis.text.y = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color)) +
  theme(legend.title = element_text(size = sett_plot$legend$title$size),
        legend.text = element_text(size = sett_plot$legend$text$size)) +
  theme(text = element_text(#size = 7, 
    family = "Arial"))

plot(plot_dat_post)



# Save plot ---------------------------------------------------------------

ggsave("removed_nodes.png", 
       plot = plot_dat_post,
       width = 12,
       height = 12,
       units = "cm",
       dpi = 600,
       path = "output")




# Compute distance between nodes ------------------------------------------

# In order to join map data and driving data the map data needs to be ...
# ... enriched with distance information

# Compute distance between GPS coordinates
dat_coords_dist <- compute_gps_dist_df(dat_coords_corr)



# Visualize distance between nodes ----------------------------------------

plot_dat <- 
  ggplot() +
  geom_point(dat = dat_coords_dist %>% 
               mutate(node_no_global = row_number()),
             aes(x = node_no_global,
                 y = gps_dist_m_cum,
                 color = as.factor(way_id)))



# Interpolate distance data -----------------------------------------------

# Define which columns should not be interpolated
col_names_exclude <- c("way_id", "element_no", "node_no")

# Interpolate values
dat_coords_intrpld <- 
  intrpldf(dat_coords_dist, 
           "gps_dist_m_cum", 
           stepsize = 0.1,
           colnames2excl = col_names_exclude, colname_intrpld = NULL)

# Fill values for columns that were excluded
for (c in col_names_exclude) {
  dat_coords_intrpld[, c] <- zoo::na.locf(dat_coords_intrpld[, c])
}



# Visualize interpolated data ---------------------------------------------

# Compare original and interpolated data
plot_dat <- 
  ggplot() +
  geom_point(dat = dat_coords_corr,
             aes(x = gps_lon,
                 y = gps_lat,
                 size = node_no,
                 color = as.factor(way_id)),
             alpha = 0.5) + 
  geom_point(dat = dat_coords_intrpld,
             aes(x = gps_lon,
                 y = gps_lat,
                 color = as.factor(way_id)),
             size = 0.1,
             alpha = 0.01) + 
  coord_cartesian(xlim = c(11.647, 11.651),
                  ylim = c(48.080, 48.085))



# Post-process plot -------------------------------------------------------

plot_dat_post <- 
  plot_dat +
  ggtitle("Interpolated GPS positions between nodes",
          subtitle = text_subtitle_nodes) + 
  labs(x = "Longitude",
       y = "Latitude") + 
  scale_color_manual(name = "Way IDs:", 
                     values = c("red2", "blue", "green3")) +
  guides(size = "none") + 
  theme_bw() + 
  theme(legend.position="top",
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-15,-10,-8,0)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  theme(title = element_text(size = sett_plot$title$size, 
                             face = sett_plot$title$face),
        plot.subtitle = element_text(size = sett_plot$subtitle$size, 
                                     face = sett_plot$subtitle$face)) +
  theme(axis.title.x = element_text(size = sett_plot$axis$title$size), 
        axis.title.y = element_text(size = sett_plot$axis$title$size)) +
  theme(axis.text.x = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color),
        axis.text.y = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color)) +
  theme(legend.title = element_text(size = sett_plot$legend$title$size),
        legend.text = element_text(size = sett_plot$legend$text$size)) +
  theme(text = element_text(#size = 7, 
    family = "Arial"))

plot(plot_dat_post)



# Save plot ---------------------------------------------------------------

ggsave("interpolated_nodes.png", 
       plot = plot_dat_post,
       width = 12,
       height = 12,
       units = "cm",
       dpi = 600,
       path = "output")



# Compute distance to reference point -------------------------------------

dat_pos <- 
  data.frame(
    gps_lon = 11.64886876174079500000, 
    gps_lat = 48.08353305109682000000
    )

# Compute distance to reference point
dat_coords_intrpld$pos_dist_m <- 
  compute_pos_dist(
    dat_coords_intrpld$gps_dist_m_cum,
    list(dat_coords_intrpld$gps_lon, dat_coords_intrpld$gps_lat),
    dat_pos
  )



# Trim map data -----------------------------------------------------------

# Map data needs to be limited to fit the required section of driving data
dat_coords_limit <- 
  dat_coords_intrpld %>% 
  filter(pos_dist_m >= -100 & pos_dist_m <= 50)



# Visualize limited data --------------------------------------------------

plot_dat <- 
  ggplot() + 
  geom_point(dat = dat_coords_intrpld,
             aes(x = gps_lon,
                 y = gps_lat,                 
                 #color = as.factor(way_id))
                 ),
             color = "black",
             size = 0.1,
             alpha = 0.05) + 
  geom_point(dat = dat_coords_limit,
             aes(x = gps_lon,
                 y = gps_lat),
             color = "red",
             alpha = 0.5) + 
  geom_point(data = dat_coords_intrpld %>% filter(pos_dist_m == 0),
             aes(x = gps_lon,
                 y = gps_lat),
             size = 5)  + 
  coord_cartesian(xlim = c(11.647, 11.651),
                  ylim = c(48.080, 48.085))



# Post-process plot -------------------------------------------------------

plot_dat_post <- 
  plot_dat +
  ggtitle("Trimmed map data & GPS reference position",
          subtitle = "Note: The red section represents the trimmed area") + 
  labs(x = "Longitude",
       y = "Latitude") + 
  # scale_color_manual(name = "Way segment IDs:", 
  #                    values = c("red2", "blue", "green3")) +
  guides(color = "none") + 
  theme_bw() + 
  theme(legend.position="top",
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-15,-10,-8,0)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  theme(title = element_text(size = sett_plot$title$size, 
                             face = sett_plot$title$face),
        plot.subtitle = element_text(size = sett_plot$subtitle$size, 
                                     face = sett_plot$subtitle$face)) +
  theme(axis.title.x = element_text(size = sett_plot$axis$title$size), 
        axis.title.y = element_text(size = sett_plot$axis$title$size)) +
  theme(axis.text.x = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color),
        axis.text.y = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color)) +
  theme(legend.title = element_text(size = sett_plot$legend$title$size),
        legend.text = element_text(size = sett_plot$legend$text$size)) +
  theme(text = element_text(#size = 7, 
    family = "Arial"))

plot(plot_dat_post)



# Save plot ---------------------------------------------------------------

ggsave("trimmed_map_data.png", 
       plot = plot_dat_post,
       width = 12,
       height = 12,
       units = "cm",
       dpi = 600,
       path = "output")



# Load driving data -------------------------------------------------------

dat_driv <- read.csv2("data/dat_driving.csv")



# Interpolate driving data ------------------------------------------------

dat_driv_intrpl <- 
  intrpldf(dat_driv, 
           "pos_dist_m", 
           stepsize = 0.1,
           colname_intrpld = NULL)



# Trim driving data -------------------------------------------------------

dat_driv_limit <- 
  dat_driv_intrpl %>% 
  filter(pos_dist_m >= -100 & pos_dist_m <= 50)



# Visualize trimmed map data and driving data -----------------------------

plot_dat <- 
  ggplot() + 
  geom_point(data = dat_coords_limit,
             aes(x = gps_lon,
                 y = gps_lat),
             col = "red") + 
  geom_point(data = dat_coords_limit %>% filter(pos_dist_m == 0),
             aes(x = gps_lon,
                 y = gps_lat),
             col = "red",
             size = 5) + 
  geom_point(data = dat_driv_limit,
             aes(x = gps_lon,
                 y = gps_lat),
             col = "black") + 
  geom_point(data = dat_driv_limit %>% filter(pos_dist_m == 0),
             aes(x = gps_lon,
                 y = gps_lat),
             col = "black",
             size = 5) +
  geom_point(data = dat_driv_limit[1,],
             aes(x = gps_lon,
                 y = gps_lat),
             col = "black",
             shape = 17,
             size = 5) 
  # geom_point(data = dat_driv_limit[nrow(dat_driv_limit),],
  #            aes(x = gps_lon,
  #                y = gps_lat),
  #            col = "blue",
  #            size = 5)



# Post-process plot -------------------------------------------------------

plot_dat_post <- 
  plot_dat +
  theme_bw() + 
  ggtitle("Joined driving data & map data",
          subtitle = "Notes:\nThe red section represents the map data.\nThe black line represents the driver GPS trajectory.\nThe triangle represents the direction the driving is coming from.\nThe dots represent the GPS reference position.") + 
  labs(x = "Longitude",
       y = "Latitude") + 
  guides(color = "none") + 
  theme_bw() + 
  theme(legend.position="top",
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-15,-10,-8,0)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  theme(title = element_text(size = sett_plot$title$size, 
                             face = sett_plot$title$face),
        plot.subtitle = element_text(size = sett_plot$subtitle$size, 
                                     face = sett_plot$subtitle$face)) +
  theme(axis.title.x = element_text(size = sett_plot$axis$title$size), 
        axis.title.y = element_text(size = sett_plot$axis$title$size)) +
  theme(axis.text.x = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color),
        axis.text.y = element_text(size = sett_plot$axis$text$size, 
                                   color = sett_plot$axis$text$color)) +
  theme(legend.title = element_text(size = sett_plot$legend$title$size),
        legend.text = element_text(size = sett_plot$legend$text$size)) +
  theme(text = element_text(#size = 7, 
    family = "Arial"))

plot(plot_dat_post)



# Save plot ---------------------------------------------------------------

ggsave("joined_data.png", 
       plot = plot_dat_post,
       width = 12,
       height = 12,
       units = "cm",
       dpi = 600,
       path = "output")
