#' Correct OSM data
#'
#' @param dat 
#' @param way_ids 
#' @param viz_order 
#' @param viz_reduction 
#' @param viz_distance 
#'
#' @return
#' @export
#'
#' @examples
correct_osm_data <- function(dat, 
                              way_ids, 
                              viz_order = F, 
                              viz_reduction = F) {
                              #viz_distance = F) {
  
  # Get direction information
  
  # Load direction information
  # query <- "SELECT direction FROM t_positions where pos_id ="
  # query <- paste(query, pos_id)
  # direction <- dbGetQuery(get(db_conn_name), query)[[1]]
  
  # # Filter for interfaces
  # # (if there is an interface between two way segments, both segments will 
  # # contain the identical reference id)
  # dat_way_nodes_interface <- 
  #   dat_way_nodes %>% 
  #   group_by(ref_id) %>% 
  #   filter(n() == 2) %>% 
  #   data.frame()
  # 
  # gps_lon_first <- dat_way_nodes_interface$gps_lon[1]
  # gps_lon_last <- tail(dat_way_nodes_interface$gps_lon, 1)
  # 
  # gps_lat_first <- dat_way_nodes$gps_lat[1]
  # gps_lat_last <- tail(dat_way_nodes$gps_lat, 1)
  # 
  # if (gps_lon_first < gps_lon_last) going_left = T else going_left = F
  # if (gps_lat_first < gps_lat_last) going_up = T else going_up = F
  # 
  # if (direction == "R" & going_left) mismatch = T else mismatch = F
  
  # Check of order nodes
  dat <- correct_order_of_way_nodes(dat, way_ids, viz = viz_order)
  # Check interfaces between way segments
  dat <- remove_unneeded_way_nodes(dat, way_ids, viz = viz_reduction)
  # Compute distance between nodes
  #dat_dist <- compute_gps_dist_df(dat, viz = viz_distance)
  
  #dat_return <- list(
  #  dat_way_nodes = dat,
  #  dat_way_nodes_dist = dat_dist
  #)
  
  #return(dat_return)
  return(dat)
}



#' Correct order of way nodes
#' 
#' @description Correct order of way nodes in case beginning, ending or
#' length of way segments are not matching
#'
#' @param dat Way nodes including references IDs
#' @param way_ids Way IDs sorted according to their order of appearance
#' @param viz Create visuzalization for plausabilisation (default = FALSE)
#'
#' @return
#' @export
#'
#' @examples
correct_order_of_way_nodes <- function(dat, way_ids, viz = F, debug = F) {

  element_nos <- unique(dat$element_no)
  elements_n <- length(element_nos)
  #if (way_ids_n > 1) {
  if (elements_n > 1) {
    
    if (viz) {
      title = "Uncorrected order of nodes"
      plot_osm_gps_coordinates(dat, title = title)
    }
    
    # Create template for flagging way ids to be inverted
    flags <- data.frame(
      element_no = element_nos,
      to_inv = F
    )
    
    # Check whether the last node of the first way segment matches
    # the first node of the second way segment
    # Nodes are referred to via ref_id
    
    # Get reference ids for first way segment
    finder <- which(dat$element_no == element_nos[1])
    ref_ids_curr <- dat$ref_id[finder]
    
    # Get reference ids for second way segment
    finder <- which(dat$element_no == element_nos[2])
    ref_ids_next <- dat$ref_id[finder]
    
    # Find matching elements
    finder <- which(ref_ids_curr %in% ref_ids_next)
    # If index is not referring to the last segment: 
    # Flag way segment for inversion
    if (finder != length(ref_ids_curr)) {
      flags$to_inv[1] <- T
      
    }
    
    # From the second to the last way segment, check whether the last node 
    # of the previous way is matching one of the nodes of the current way segment
    # Nodes are referred to via ref_id
    for (i in 2:(elements_n)) {
      
      if (debug) {
        print(paste("Current element no:", element_nos[i]))
      }
      
      # Get reference ids for previous way segment
      finder <- which(dat$element_no == element_nos[i - 1])
      ref_ids_prev <- dat$ref_id[finder]
      
      # Get reference ids for current way segment
      finder <- which(dat$element_no == element_nos[i])
      ref_ids_curr <- dat$ref_id[finder]
      ref_ids_curr_n = length(ref_ids_curr)
            

      # If the first reference id of the current way segment does not represent
      # the interface to the previous way segment flag the current way segment
      # (This is not sufficient and needs to be counter-checked again from the 
      # following way segment to the current way segment)
      # Reason: The can be cases, where the current way segment is longer than
      # required (e.g. begins earlier or ends later than the interfaces)
      finder <- which(ref_ids_curr %in% ref_ids_prev)
      
      # Check if the first reference point of the current way segment matches
      # the last reference point of the previous way segment
      if (finder == 1) {
        
        if (debug) {
          print("All good! (1)") 
        }
        
        } else {
          
          # Check if there are more elements coming
          if (i == elements_n) {
            
            # If there are no following elements apply correction
            flags$to_inv[i] <- T
            
            if (debug) {
              print("Corrected! (1)") 
            }
            
          } else {
            
            # If there are more elements following the current way segment,
            # check, if the last reference point of the current way segment 
            # represents the interface to the previous way segment
            if (finder == ref_ids_curr_n) {
              
              # If yes, apply correction
              flags$to_inv[i] <- T
              
              if (debug) {
                print("Corrected! (2)") 
              }

            } else {
              
              # If the last reference point of the current way segment does not
              # represents the interface to the previous way segment, check if
              # the order of reference points is already correct
              node_no_match_to_prev <- which(ref_ids_curr %in% ref_ids_prev)

              finder <- which(dat$element_no == element_nos[i + 1])
              ref_ids_next <- dat$ref_id[finder]
              node_no_match_to_next <- which(ref_ids_curr %in% ref_ids_next)
              
              # This is dirty and might lead to errors
              if (node_no_match_to_prev < node_no_match_to_next) {
                
                # If the order of reference ids is already correct, do nothing
                if (debug) {
                  print("All good! (3)") 
                }
                
              } else {

                # If the order of reference ids is not correct, apply cocrection
                flags$to_inv[i] <- T
                                
                if (debug) {
                  print("Corrected! (3)") 
                }

              }
            }
          }
        }
      } 
  }
  
  dat <- invert_order_of_nodes(dat, flags)
  if (viz) {
    title = "Corrected order of nodes"
    plot_osm_gps_coordinates(dat, title = title)
  }
  
  return(dat)
  
}



#' Invert order of nodes
#'
#' @param dat 
#' @param flags 
#'
#' @return
#' @export
#'
#' @examples
invert_order_of_nodes <- function(dat, flags) {
  
  # Correct data by inverting node enumeration
  dat <- 
    dplyr::left_join(dat, flags, by = "element_no") %>% 
    dplyr::group_by(element_no) %>% 
    mutate(node_no_backup = node_no) %>% 
    dplyr::mutate(node_no = ifelse(
      to_inv, 
      abs(node_no - max(node_no)) + 1, 
      node_no)
      ) %>% 
    dplyr::arrange(element_no, node_no) %>%
    dplyr::select(-to_inv) %>% 
    data.frame()
  
  return(dat)
  
}



#' Remove unneeded way nodes
#' 
#' @description Remove unneeded way nodes for interfacing way segments
#'
#' @param dat Way nodes including references IDs
#' @param way_ids Way IDs sorted according to their order of appearance
#' @param viz Create visuzalization for plausabilisation (default = FALSE)
#'
#' @return
#' @export
#'
#' @examples
remove_unneeded_way_nodes <- function(dat, way_ids, viz = F, debug = F) {
  
  element_nos <- unique(dat$element_no)
  elements_n <- length(element_nos)
  
  if (elements_n > 1) {
    
    # Initialise collector for reference ids that are to be removed
    ref_ids_to_remove <- c()
    
    #for (i in 1:(way_ids_n - 1)) {
    for (i in 1:(elements_n - 1)) {
      
      if (debug) {
        print(paste("Current element no:", element_nos[i]))
      }
      
      # Get reference ids for current way segment
      finder <- which(dat$element_no == element_nos[i])
      ref_ids_curr <- dat$ref_id[finder]
      ref_ids_curr_n <- length(ref_ids_curr)
      
      # Get reference ids for next way segment
      finder <- which(dat$element_no == element_nos[i + 1])
      ref_ids_next <- dat$ref_id[finder]
      
      # Check only for segments that have more than reference id
      if (ref_ids_curr_n > 1) {
        
        # Find reference point that has an interface with the next way segment
        finder <- which(ref_ids_curr %in% ref_ids_next)
        # If index of the found reference point is not the last reference point
        # of the current segment, remove all reference point that come after
        # this reference point
        if (finder < ref_ids_curr_n) {
          # Identify the reference points that are to be removed as indices
          indices_temp <- (finder + 1):ref_ids_curr_n
          # Find indices of these reference in the target table
          ref_ids_to_remove <- c(ref_ids_to_remove, ref_ids_curr[indices_temp])
          
          if (debug) {
            print("Identified reference ids in forward analysis") 
          }
          
        }
      }
    }
    
    for (i in 2:elements_n) {
      
      # Get reference ids for next way segment
      finder <- which(dat$element_no == element_nos[i - 1])
      ref_ids_prev <- dat$ref_id[finder]
      
      # Get reference ids for current way segment
      finder <- which(dat$element_no == element_nos[i])
      ref_ids_curr <- dat$ref_id[finder]
      ref_ids_curr_n <- length(ref_ids_curr)
      
      # Check only for segments that have more than one reference id
      if (ref_ids_curr_n > 1) {
        
        finder <- which(ref_ids_curr %in% ref_ids_prev)
        
        if (finder != 1) {
          # Identify reference ids until the identified reference point that
          # represents the interface
          indices_temp <- 1:(finder - 1)
          ref_ids_to_remove <- c(ref_ids_to_remove, ref_ids_curr[indices_temp])
          
          if (debug) {
            print("Identified reference ids in backward analysis") 
          }
          
        }
      }
    }
    
    # Check last way segment
    # Get reference ids for previous way segment
    # (simply use reference ids from previous section)
    finder <- which(ref_ids_curr %in% ref_ids_prev)
    if (finder != 1) {
      # Identify reference ids until the identified reference point that
      # represents the interface
      indices_temp <- 1:(finder - 1)
      ref_ids_to_remove <- c(ref_ids_to_remove, ref_ids_curr[indices_temp])
      
      if (debug) {
        print("Identified reference ids for last segment")
      }
      
    } 
    
    # Remove data for identified reference ids
    finder <- which(dat$ref_id %in% ref_ids_to_remove)
    if (length(finder) > 0) {
      dat <- dat[-c(finder),]
    }
  }
  
  if (viz) {
    title = "Plausibilisation of interfaces"
    plot_osm_gps_coordinates(dat, title = title)
  }
  
  return(dat)
}



#' Plot OSM GPS coordinates
#'
#' @param dat 
#' @param title 
#' @param labels 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
plot_osm_gps_coordinates <- function(dat, 
                                     title = NULL, 
                                     labels = T, 
                                     name_col = "element_no", 
                                     seed = 42) {
  
  set.seed(seed)
  
  # Visualize order of corrected nodes
  dat_plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_point(dat = dat,
                        aes(x = gps_lon,
                            y = gps_lat,
                            size = node_no,
                            color = as.factor(!!as.name(name_col))
                        ),
                        alpha = 0.5) + 
    ggrepel::geom_text_repel(dat = dat,
                             aes(x = gps_lon,
                                 y = gps_lat,
                                 color = as.factor(!!as.name(name_col)),
                                 label = ref_id),
                             size = 3,
                             max.overlaps = 100)
  # ggplot2::geom_text(dat = dat,
  #                    aes(x = gps_lon,
  #                        y = gps_lat,
  #                        color = as.factor(way_id),
  #                        label = ref_id),
  #                    hjust = -1,
  #                    size = 2) + 
  
  if (!is.null(title)) {
    dat_plot <- 
      dat_plot + 
      ggplot2::ggtitle(title)
  }
  
  plot(dat_plot)
}



#' Compute GPS distance
#' 
#' @description Compute GPS distance within a data frame
#'
#' @param df Data frame including the columns for lon./lat. GPS coordinates
#' @param name_lon Column name for lon. GPS values (default: "gps_lon")
#' @param name_lat Column name for lat. GPS values (default: "gps_lat")
#' @param viz Create visuzalization for plausabilisation (default = FALSE)
#'
#' @return The original data frame including two additional columns:
#' 1) Distance between two succeeding data points ("gps_dist_m")
#' 2) Cumulative sum of computed distances ("gps_dist_m_cum)
#' 
#' @import dplyr
#' @importFrom geosphere distHaversine
#' @export
#'
#' @examples
#' data_ids <-  c(4061607, 142968329, 247441077, 247441078, 37743291, 247441070)
#' data_xml <- get_way_data(data_ids)
#' data_coords <- extract_way_gps_coords(data_xml, data_ids)
#' data_coords_corr <- correct_order_of_nodes(data_coords)
#' data_coords_dist <- compute_gps_dist_df(data_coords_corr)
compute_gps_dist_df = function(dat, 
                               name_lon = "gps_lon",
                               name_lat = "gps_lat",
                               viz = FALSE) {
  
  name_lon_prev <- paste0(name_lon, "_prev")  
  name_lat_prev <- paste0(name_lat, "_prev")
  
  dat <- 
    dat %>%
    mutate(!!name_lon_prev := lag(!!as.name(name_lon)),
           !!name_lat_prev := lag(!!as.name(name_lat))) %>%
    rowwise() %>%
    mutate(gps_dist_m =
             geosphere::distHaversine(
               c(!!as.name(name_lon_prev), 
                 !!as.name(name_lat_prev)),
               c(!!as.name(name_lon), 
                 !!as.name(name_lat))
             )
    ) %>% 
    mutate(gps_dist_m = ifelse(is.na(gps_dist_m), 0, gps_dist_m)) %>% 
    ungroup() %>% 
    mutate(gps_dist_m_cum = cumsum(gps_dist_m)) %>% 
    select(-!!name_lon_prev, -!!name_lat_prev) %>% 
    data.frame()
  
  if (viz) {
    
    # Visualize distance between nodes
    dat_plot <- 
      ggplot2::ggplot() +
      ggplot2::geom_point(dat = dat,
                          aes(x = ref_id,
                              y = gps_dist_m,
                              color = as.factor(way_id))) + 
      ggplot2::geom_text(dat = dat,
                         aes(x = ref_id,
                             y = gps_dist_m,
                             label = as.character(ref_id),
                             color = as.factor(way_id)),
                         nudge_x = 1.75) + 
      ggplot2::ggtitle("Distance to previous node")
    
    plot(dat_plot)
    
  }
  
  
  return(dat)
  
}



#' Process OSM data
#'
#' @param pos_id 
#' @param viz_order 
#' @param viz_reduction 
#' @param viz_distance 
#'
#' @return
#' @export
#'
#' @examples
preprocess_osm_data <- function(pos_id, 
                                viz_order = F, 
                                viz_reduction = F,
                                viz_distance = F) {
  
  text_pos_id = paste("Position ID:", pos_id)
  text <- paste(text_pos_id, "\n")
  cat(text)
  
  #source("analyze/01_load-osm-data.R")
  #source("analyze/02_correct-osm-data.R")
  
  # Load way ids and way nodes data
  way_ids <- load_way_ids_from_db(pos_id, db_conn_name)
  dat <- load_ways_data_from_db(way_ids, db_conn_name)
  
  # Correct way nodes
  dat <- 
    correct_osm_data(dat, 
                     way_ids, 
                     viz_order = viz_order, 
                     viz_reduction = viz_reduction)
  
  # Compute GPS distance
  dat <- compute_gps_dist_df(dat, viz = viz_distance)
  
  if (viz_order | viz_reduction | viz_distance) {
    pauseAndContinue()
  }
  
  return(dat)
  
}