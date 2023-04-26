
# Load resources ----------------------------------------------------------

library(puttytat4R)
library(RPostgreSQL)



# Load data ---------------------------------------------------------------

# Load required way ids
#dbConnectOperator()
#db_conn <- dbFindConnObj("Kreuzungsfahrten-Lisa")
db_conn <- dbFindConnObj("lg-010_driving-data")
db_conn_name <- "db_conn_13"

# Get only way ids which are not already stored in database
query <- paste0("SELECT DISTINCT way_id FROM t_way_ids")
query <- paste0(
  "SELECT DISTINCT way_id 
  FROM t_osm_way_ids
  WHERE way_id NOT IN (SELECT DISTINCT way_id FROM t_osm_way_nodes)"
  )
message(query)
dat_ids <- dbGetQuery(get(db_conn_name), query)
dat_ids <- dat_ids$way_id



# Get OSM data ------------------------------------------------------------

# Load OSM data
dat_xml <- get_ways_data_via_overpass(dat_ids)
dat_xml <- get_ways_data_via_overpass(way_ids)

# Convert response to dataframe
dat_xml_list <- convert_osm_api_response_to_list(dat_xml)

# Extract GPS coordinates
# Correct order of nodes
dat_way_info <- extract_osm_way_information(dat_xml_list, dat_ids)
dat_way_nodes <- dat_way_info$dat_nodes
dat_way_tags <- dat_way_info$dat_tags



# Save data ---------------------------------------------------------------

# Write node information to database
db_table_name <- "t_osm_way_nodes"
dat_for_db <- dat_way_nodes
dat_for_db$created_on <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
dbWriteTable(get(db_conn_name), db_table_name, dat_for_db, 
             row.names = F, 
             append = T)

# Write tag information to database
db_table_name <- "t_osm_way_tags"
dat_for_db <- dat_way_tags
dat_for_db$created_on <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
dbWriteTable(get(db_conn_name), db_table_name, dat_for_db, 
             row.names = F, 
             append = T)
