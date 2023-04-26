
# Interpolate driving data
dat_driv_intrpl <- 
  intrpldf(dat_driv, 
           "pos_dist_m", 
           stepsize = 0.1,
           colname_intrpld = NULL)


# Limit driving data
dat_driv_limit <- 
  dat_driv_intrpl %>% 
  filter(pos_dist_m >= -100 & pos_dist_m <= 50)