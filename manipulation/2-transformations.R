
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-sources ------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  
# Ideally, no real operations are performed in these sourced scripts. 
source("./scripts/functions-common.R") # used in multiple reports

# ---- load-packages ----------------------------------------------
# Attach packages so their functions don't need to be qualified when used
# See more : http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # Pipes
library(ggplot2) # Graphs
# Functions of these packages will need to be qualified when used
# See more: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tidyr") #  data manipulation
requireNamespace("dplyr") # f() names conflict with other packages (esp. base, stats, and plyr).
requireNamespace("testit") # for asserting conditions meet expected patterns.
# ---- load-globals ------------------------------------------------
path_dto_input <- "./data-unshared/derived/dto_raw.rds"
path_dto_output <- "./data-unshared/derived/dto.rds"

# ---- load-data ------------------------------------------------
dto_raw <- readRDS(path_dto_input)
ds <- dto_raw

# ---- inspect-data ------------------------------------------------
# ---- basic-table ------------------------------------------------
# ---- basic-graph ------------------------------------------------

# ----  ------------------------------------------------
# ---- save-to-disk ------------------------------------------------
saveRDS(dto, file = path_dto_output)


# DEVELOPMENTAL SCRIPT AFTER THIS LINE

# correct serial7r_tot to merge the 2014 var with the rand serial 7 data
ds_long$serial7r_tot <- ifelse(ds_long$year==2014, ds_long$serial7r_tot.y,ds_long$serial7r_tot.x)

# A list of the psychosocial variables to use to check for completion of the psychosocial variables.
ds_lbvars <- ds_long %>% 
  dplyr::select(score_loneliness_3, score_loneliness_11, snspouse, snchild, 
                snfamily, snfriends,support_spouse_total, support_child_total, support_fam_total, 
                support_friend_total, strain_spouse_total, strain_child_total, strain_family_total, 
                strain_friends_total, children_contact_mean, family_contact_mean, friend_contact_mean,
                activity_mean, activity_sum)

# an indicator variable of whether or not there are any psychosocial variables not NA for that wave.
ds_long$lbqs <- ifelse(rowSums(!is.na(ds_lbvars)) >1 , 1, 0)

ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn, lbqs) %>% 
  dplyr::mutate(
    lbwavecount =seq(n())) %>% 
  dplyr::ungroup()

# create an indicator only of waves  
ds_long$lbwave <- ifelse(ds_long$lbqs==1, ds_long$lbwavecount, 0)

# subset the data frame to include only those belonging to a cohort.
ds_long <- subset(ds_long, cohort!= 0)
