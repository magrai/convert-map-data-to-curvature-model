
# Load way ids
way_ids <- load_way_ids_from_db(pos_id, db_conn_name)

# Load way nodes
dat_way_nodes <- load_ways_data_from_db(way_ids, db_conn_name)
