#' Get ways data via Overpass
#'
#' @description Get OSM data for way elements via Overpass API
#' (https://overpass-api.de/api/interpreter)
#' 
#' @param ids Way IDs to be downloaded
#'
#' @return XML response
#' 
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom xml2 read_xml
#' @export
#'
#' @examples
#' data_ids <- c(4061607, 142968329, 247441077, 247441078, 37743291, 247441070)
#' data_xml <- get_way_osm_data(data_ids)
get_ways_data_via_overpass <- function(ids) {
  
  url_overpass <- "https://overpass-api.de/api/interpreter"
  
  # Backup information in case of encoding problems
  # " " can be represented via "%20"
  # "(" can be represented via "%28"
  # ")" can be represented via "%29"
  # ";" can be represented via "%3B"
  url_data <- "?data="
  url_data_type <- "way"
  url_elements <- "(id:"
  url_out_geom <- ");out%20geom;"
  
  # Create string of data ids
  ids_str <- paste0(ids, collapse = ",")
  
  # Create URL
  url_temp <- paste0(
    url_overpass,
    url_data,
    url_data_type,
    url_elements,
    # IDs will be inserted here
    ids_str,
    url_out_geom
  )
  
  # Send and process request
  cat(url_temp)
  response <- httr::GET(url_temp)
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  xml <- xml2::read_xml(content)
  
  return(xml)
}


#' Convert OSM API response to dataframe
#'
#' @param xml 
#'
#' @return
#' @export
#'
#' @examples
convert_osm_api_response_to_list <- function(xml) {
  
  xml_list <- xml2::as_list(xml)
  dat <- xml_list$osm
  
  return(dat)
}


#' Extract OSM way information 
#' 
#' @description Extract GPS coordinates from Overpass API results
#'
#' @param xml XML response from Overpass ID 
#' @param way_ids_order 
#'
#' @return
#' 
#' @importFrom xml2 as_list
#' @import dplyr 
#' @export
#'
#' @examples
#' data_ids <-  c(4061607, 142968329, 247441077, 247441078, 37743291, 247441070)
#' data_xml <- get_way_osm_data(data_ids)
#' data_coords <- extract_way_gps_coords(data_xml, data_ids)
extract_osm_way_information <- function(dat_osm, way_ids_order, add_element_no = F) {
  
  dat_nodes <- c()
  dat_tags <- c()
  
  # Find way information
  ways <- names(dat_osm) == "way"
  way_ids <- seq(1, length(dat_osm))[ways]
  
  # Analyze each way element
  for (w in way_ids) {
    
    dat_way <- dat_osm[[w]]
    way_id <- attributes(dat_way)$id
    
    # Find index of original order
    element_no = which(way_ids_order == way_id)
    
    
    # Extract node information
    
    # Find node information
    nodes <- which(names(dat_way) == "nd")
    node_ids <- seq(1, length(dat_way))[nodes]
    
    # Extract GPS information from each node
    # Initialize node number
    node_no <- 0
    for (n in node_ids) {
      
      node_no <- node_no + 1
      dat_node <- attributes(dat_way[[n]])
      ref_id <- dat_node$ref
      gps_lon <- as.numeric(dat_node$lon)
      gps_lat <- as.numeric(dat_node$lat)
      
      # Collection information
      dat_nodes_temp <- data.frame(
        element_no = element_no,
        way_id = way_id,
        node_no = node_no,
        ref_id = ref_id,
        gps_lon = gps_lon,
        gps_lat = gps_lat
      )
      
      if (!add_element_no) {
        dat_nodes_temp$element_no <- NULL
      }
      
      dat_nodes <- rbind(dat_nodes, dat_nodes_temp)
      
    }
    dat_nodes <- dplyr::arrange(dat_nodes, element_no, node_no)
    
    # Extract tag information
    
    # Find tag information
    tags <- which(names(dat_way) == "tag")
    tag_ids <- seq(1, length(dat_way))[tags]
    
    tag_no <- 0
    for (t in tag_ids) {
      
      tag_no <- tag_no + 1
      dat_tag <- attributes(dat_way[[t]])
      attribute <- dat_tag[[1]]
      value <- dat_tag[[2]]
      
      dat_tags_temp <- data.frame(
        way_id = way_id,
        attribute = attribute, 
        value = value
      )
      
      dat_tags <- rbind(dat_tags, dat_tags_temp)
      
    }
  }
  
  dat_return <- list(dat_nodes = dat_nodes, dat_tags = dat_tags)
  
  return(dat_return)
  
}




#' Load way ids from database
#'
#' @description Load way ids for a position id from database
#'
#' @param id Position ID
#' @param db_conn_name Name of the database connection
#' @param source_name Name of the database source (default: t_way_ids)
#' @param query SQL query for retrieving way ids from the database source
#'
#' @return
#' @export
#'
#' @examples
load_way_ids_from_db <- function(pos_id, 
                                 db_conn_name, 
                                 source_name = NULL, 
                                 query = NULL,
                                 order_ids_to_exlude = c(0)) {
  
  # Set default source
  if (is.null(source_name)) {
    source_name = "t_osm_way_ids"
  }
  
  # Set default query
  if (is.null(query)) {
    query <- paste(
      "
      SELECT way_id 
      FROM _SOURCE_ 
      WHERE pos_id = _ID_ AND order_id_way NOT IN (_EXCLUDE_)
      ORDER BY order_id_way
      ")
  }
  
  # Replace source placeholder with source name
  query <- gsub("_SOURCE_", source_name, query)
  
  # Replace position id placeholder with position ids
  query <- gsub("_ID_", pos_id, query)
  
  # Replace order id placeholder with order ids that are to be excluded
  query <- gsub("_EXCLUDE_", order_ids_to_exlude, query)
  
  # Load data
  dat <- DBI::dbGetQuery(get(db_conn_name), query)
  
  # Convert to nameless vector
  dat <- unname(unlist(dat))
  
  return(dat)
  
}



#' Load ways data from database
#' 
#' @description Load ways from database (that has been previously retrieved 
#' e.g. via Overpass API, and processed)
#'
#' @param ids Way IDs
#' @param db_conn_name Name of the database connection
#' @param source_name Name of the database source (default: v_ways_nodes_latest)
#' @param query SQL query for retrieving ways data from the database source
#' @param enum Should the nodes be remunerated according to their order of 
#' appearance in way ids? (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
load_ways_data_from_db <- function(ids, 
                                   db_conn_name,
                                   source_name = NULL, 
                                   query = NULL,
                                   enum = T) {
  
  if (is.null(source_name)) {
    source_name <- "v_osm_way_nodes_latest"
  }
  
  if (is.null(query)) {
    query <- paste(
      "
      SELECT 
      way_id,
      node_no,
      ref_id,
      gps_lon,
      gps_lat
      FROM _SOURCE_ 
      WHERE 
      way_id IN (_IDS_)
      ")
  }
  # Replace source placeholder with source name
  query_temp <- gsub("_SOURCE_", source_name, query)
  
  # Load data separately for each way id in case of recurring way ids
  ids_n <- length(ids)
  dat <- c()
  for (i in 1:ids_n) {
    id <- ids[i]
    # Replace id placeholder with ids
    #ids_str <- paste(ids, collapse = ", ")
    query <- gsub("_IDS_", id, query_temp)
    dat_temp1 <- data.frame(element_no = i)
    dat_temp2 <- DBI::dbGetQuery(get(db_conn_name), query)
    dat_temp3 <- cbind(dat_temp1, dat_temp2)
    dat <- rbind(dat, dat_temp3)
  }
  
  if (enum) {
    
    # Assign element numbers and sort data according order of way_ids
    dat <- 
      dat %>% 
      #dplyr::mutate(element_no = match(way_id, ids)) %>% 
      #dplyr::relocate(element_no, .before = "way_id") %>% 
      dplyr::arrange(element_no, node_no) %>% 
      data.frame()
    
  }
  
  return(dat)
}



load_osm_way_speed_limits <- function(pos_id, way_ids, db_conn_name, default = 50) {
  
  query <- 
    paste(
      "
    SELECT DISTINCT ways.way_id, ways.order_id_way, ms.maxspeed 
    FROM t_osm_way_ids AS ways
    LEFT JOIN
    (
      SELECT way_id, value AS maxspeed 
      FROM t_osm_way_tags 
      WHERE way_id IN (_WAY_IDS_) AND attribute = 'maxspeed'
    ) AS ms ON
    ways.way_id = ms.way_id
    WHERE ways.pos_id = _POS_ID_ AND ways.way_id IN (_WAY_IDS_)
    ORDER BY ways.order_id_way
    "
    )
  way_ids_str <- paste(c(way_ids), collapse = ",")
  query <- gsub("_POS_ID_", pos_id, query)
  query <- gsub("_WAY_IDS_", way_ids_str, query)
  dat <- DBI::dbGetQuery(get(db_conn_name), query)
  dat$order_id_way <- NULL
  dat$maxspeed <- as.integer(dat$maxspeed)
  
  # In case of missing values, use previous value
  if (is.null(dat$maxspeed[1])) {
    dat$maxspeed <- zoo::na.locf(dat$maxspeed, fromLast = T)
  } else {
    dat$maxspeed <- zoo::na.locf(dat$maxspeed)
  }
  
  # Rename columns
  names(dat) <- c("way_id", "speed_kmh_max")
  
  # If there are still missing values, set default
  dat$source <- "osm"
  finder <- which(is.na(dat$maxspeed))
  if (length(finder) > 0) {
    dat$maxspeed[finder] <- default
    dat$source[finder] <- "default"
  }
  
  return(dat)
  
}
