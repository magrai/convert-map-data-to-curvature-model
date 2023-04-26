
# Load resources ----------------------------------------------------------

# Load libraries
library(puttytat4R)
library(dplyr)
library(ggplot2)
library(ggmap)
ggmap::register_google(key = "AIzaSyCrlm6qGuCtOD42OKxrch4ygo4qIrtbSz8")

# Load functions
file_path <- "R"
for (i in list.files(file_path)) { source(file.path(file_path, i)) }

dbInitSettings()
dbConnectOperator()



# Initialize database name ------------------------------------------------

db_conn_name <- dbFindConnObj("Kreuzungsfahrten-Lisa")



# Operator ----------------------------------------------------------------

# Load relevant positions
query <- "SELECT pos_id FROM v_data_neuperlach_gps_relevant"
pos_ids <- dbGetQuery(get(db_conn_name), query)[[1]]
dat_curv_coll <- c()
print(pos_ids)

for (pos_id in pos_ids) {
#for (pos_id in c(54)) {
  
  # Compute curvature
  # Compute new reference point
  dat_temp <- compute_curvature_and_ref_pos(pos_id)
  dat_curv <- dat_temp$dat_curv
  ref_pos <- dat_temp$ref_pos
  
  # Write reference point to database
  dat_ref_pos <- data.frame(
    pos_id = pos_id,
    gps_lat = ref_pos$gps_lat,
    gps_lon = ref_pos$gps_lon,
    created_on = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  dat_curv_coll <- rbind(dat_curv_coll, dat_curv)
  
  # # Write table to data base
  dbWriteTable(get(db_conn_name),
               name = "t_positions_curv_based",
               dat_ref_pos,
               append = T,
               row.names = F)
  
}





# Compute new distance to reference position ------------------------------

dat_curv_coll2 <- c()
for (pos_id_ in pos_ids) {
  
  print(pos_id_)
  
  # Load position data for reference point
  query <- "SELECT gps_lon, gps_lat FROM t_positions_curv_based WHERE pos_id ="
  query <- paste(query, pos_id_)
  ref_pos <- dbGetQuery(get(db_conn_name), query)
  
  
  dat_curv_temp <- 
    dat_curv_coll %>% 
    filter(pos_id == pos_id_)
  
  dat_curv_temp$pos_dist_m <- 
    compute_pos_dist(
      dat_curv_temp$gps_dist_m_cum,
      list(dat_curv_temp$gps_lon, dat_curv_temp$gps_lat),
      ref_pos
    )
  
  dat_curv_coll2 <- rbind(dat_curv_coll2, dat_curv_temp)
  
}



# Compare curvature profiles ----------------------------------------------

dat_curv_coll2_limited <- 
  dat_curv_coll2 %>% filter(pos_dist_m >= -150 & pos_dist_m <= 50)

dat_curv_coll2_limited$pos_dist_m <- round(dat_curv_coll2_limited$pos_dist_m, 1)


dat_curv_coll2_limited$curv_smoothed_scaled <- 
  normalize(dat_curv_coll2_limited$curv_smoothed)
dat_curv_coll2_limited$curv_smoothed_scaled2 <- 
  dat_curv_coll2_limited$curv_smoothed / max(dat_curv_coll2_limited$curv_smoothed)
dat_curv_coll2_limited$curv_smoothed_scaled2 <- 
  dat_curv_coll2_limited$curv_smoothed_scaled2/10

ggplot() + 
  geom_line(data = dat_curv_coll2_limited %>% filter(pos_dist_m >= -150 & pos_dist_m <= 50),
            aes(x = pos_dist_m,
                y = curv_smoothed_scaled2,
                color = as.factor(pos_id)))
