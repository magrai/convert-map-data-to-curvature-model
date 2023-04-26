
# Load resources ----------------------------------------------------------

# Load libraries
library(puttytat4R)
library(dplyr)

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
print(pos_ids)



# Quality check: Order of nodes -------------------------------------------

pos_id_ <- 80
index_from <- which(pos_ids == pos_id_)
index_to <- length(pos_ids)

for (i in index_from:index_to) {
  
  text <- paste("i:", i, "\n") 
  cat(text)
  pos_id <- pos_ids[i]
  dat <- 
    preprocess_osm_data(pos_id, 
                        viz_order = T, 
                        viz_reduction = T,
                        viz_distance = T)
  
}
