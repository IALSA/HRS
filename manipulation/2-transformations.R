
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
library(dplyr)
# Functions of these packages will need to be qualified when used
# See more: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tidyr") #  data manipulation
requireNamespace("dplyr") # f() names conflict with other packages (esp. base, stats, and plyr).
requireNamespace("testit") # for asserting conditions meet expected patterns.
requireNamespace("zoo")
# ---- load-globals ------------------------------------------------
path_dto_input <- "./data-unshared/derived/1-dto.rds"
path_dto_output <- "./data-unshared/derived/2-dto.rds"

# ---- load-data ------------------------------------------------
dto_raw <- readRDS(path_dto_input)


# ---- inspect-data ------------------------------------------------
dto_raw %>% glimpse()


# ---- tweak-data -------------------------------------------------
dto_new <- dto_raw %>% 
  dplyr::rename(
    id = hhidpn
  ) 

dto_new %>% glimpse()

# ---- correct-serial7 ------------------------------------------
# RAND pull does not contain data for 2014 at the time of grooming (April 2017)
# serial7r_tot_2014 is created to track the measure for serial7 in a separate column
# expect to be modified in the future
# implement correction
dto_new <- dto_new %>% 
  dplyr::mutate(
    serial7_total = ifelse(year==2014, serial7r_tot_2014, serial7r_tot)
  )
# Demonstration for a single case
dto_new %>% 
  dplyr::select(id, year, starts_with("serial7r")) %>% 
  dplyr::filter(id == "3020") 
# remove unnecessary columns
dto_new <- dto_new %>% 
  dplyr::select(-serial7r_tot, -serial7r_tot_2014)

# ---- create-leave-behind-indicator -----------------------------
leave_behind_measures <- c(
   "score_loneliness_3"    # label   
  ,"score_loneliness_11"   # label 
  ,"snspouse"              # label      
  ,"snchild"               # label      
  ,"snfamily"              # label  
  ,"snfriends"             # label     
  ,"support_spouse_total"  # label   
  ,"support_child_total"   # label
  ,"support_fam_total"     # label    
  ,"support_friend_total"  # label       
  ,"strain_spouse_total"   # label     
  ,"strain_child_total"    # label     
  ,"strain_family_total"   # label     
  ,"strain_friends_total"  # label       
  ,"children_contact_mean" # label      
  ,"family_contact_mean"   # label  
  ,"friend_contact_mean"   # label          
  ,"activity_mean"         # label    
  ,"activity_sum"          # label       
)
# 

# What needs to be done
# if a person-wave ( 1 row in dto_raw), on the variables `leave_behind_measures`
# does not have at least a single valid value (in other words all values are NA or NaN)
# then this person-wave row does not belong to the questionnaire 
# TODO design algorithmic method of removing observations that do not belong
# more specifically, create a tag variables, which would allow quick removal of irrelevant cases

# dto_new %>% dplyr::glimpse()
# a <- dto_new[dto_new$id==3020, c("id",leave_behind_measures) ]
# b <- as.data.frame(a)[3,]
# b %>% t()

# # Demonstrate for a single person
dto_new %>%
  dplyr::filter(id == "3020") %>%
  dplyr::select_(.dots = c("id","year",leave_behind_measures)) %>%
  dplyr::mutate(
    add_valid_values = rowSums(.[leave_behind_measures],na.rm = T),
    # add_valid_values = sum(is.na(.[leave_behind_measures])),  
    
    leave_behind_flag = ifelse(add_valid_values , TRUE, FALSE )
  ) %>%
  #as.data.frame()


  dplyr::group_by(id, leave_behind_flag) %>%
  dplyr::mutate(
    lb_wave_count = seq(n())
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    lb_wave = leave_behind_flag * lb_wave_count
  ) %>%
  as.data.frame()

# implement for the entire data set
dto <- dto_new %>% 
  # dplyr::filter(id == "1010") %>%
  #dplyr::select_(.dots = c("id","year",leave_behind_measures)) %>%
  dplyr::mutate(
    add_valid_values = rowSums(.[leave_behind_measures],na.rm = T),
    leave_behind_flag = ifelse(add_valid_values , TRUE, FALSE )
  ) %>% 
  dplyr::group_by(id, leave_behind_flag) %>% 
  dplyr::mutate(
    lb_wave_count = seq(n())
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    lb_wave = leave_behind_flag * lb_wave_count
  ) %>% 
  as.data.frame() 


# ---- basic-table ------------------------------------------------
# ---- basic-graph ------------------------------------------------

# ----  ------------------------------------------------
# ---- save-to-disk ------------------------------------------------
# saveRDS(dto_new, file = path_dto_output)


# DEVELOPMENTAL SCRIPT AFTER THIS LINE

ds_long <- dto_raw

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

# designate the first lbwave as baseline

ds_long <- ds_long %>%
  #dplyr::group_by(hhidpn)
  dplyr::mutate(
    interview_date = paste0(interview_mth,"/",interview_yr),
    interview_date = zoo::as.yearmon(interview_date, "%m/%Y"),
    hrs_tscore = interview_date-dplyr::lag(interview_date)
  )

ds_long %>% 
  dplyr::select(hhidpn, year, hrs_tscore, starts_with("interview_")) %>% 
  dplyr::filter(hhidpn == "3020")
  
test <- ds_long %>% 
  #dplyr::group_by(hhidpn) %>% 
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate (
    row = ds_long[,"lbwave"]==2
  ) %>% 
  dplyr::ungroup()


which(ds_long[,""])     
ds_long$lb_tscore = ifelse(ds_long[,lbwave]==1, 0,
                   ifelse(ds_long[,lbwave]>1, interview_date-dplyr::lag(interview_date),NA))

ds_long$lbtscore <- ifelse(ds_long$lbwave==1, 0, 
                   ifelse(ds_long$lbwave==2, ds_long$intdate_rand))
ds_long[ds_long[,"lbwave"]==2 & ds_long[,"hhidpn"]==

head(ds_long$intdate_rand)

ds_long %>% 
  dplyr::select(hhidpn, year, starts_with("interview_")) %>% 
  dplyr::filter(hhidpn == "3010") %>% 
  dplyr::mutate(
    lb_tscore = ifelse(ds_long[,lbwave]==1, 0,
                       ifelse(ds_long[,lbwave]>1, interview_date-dplyr::lag(interview_date),NA))
  )


# subset the data frame to include only those belonging to a cohort.
ds_long <- subset(ds_long, cohort!= 0)


# Two chunks below are taken from 
# https://github.com/IALSA/MAP/blob/master/manipulation/1-transformations.R#L85
# ---- temporal-traingulation --------------------
ds <- ds %>% 
  dplyr::mutate(
    age_in_days_visit = round(age_at_visit * 365, 0),
    age_in_days_bl    = round(age_at_bl * 365, 0),
    birth_date        = date_at_bl - age_in_days_bl, 
    birth_year        = lubridate::year(birth_date), 
    date_at_visit     = birth_date + age_in_days_visit 
  ) %>% 
  dplyr::select(-age_in_days_visit, - age_in_days_bl)

ds %>% 
  dplyr::select(id,birth_date,birth_year,date_at_bl,age_at_bl, fu_year, date_at_visit, age_at_visit) %>%
  dplyr::slice(1:10) %>% print() 


# names_labels(ds) %>% head()
# ---- compute-death-indicator --------------------
ds <- ds %>% 
  dplyr::group_by(id) %>%  
  dplyr::mutate(   
    died           = ifelse(any(!is.na(age_death)),1,0)
  ) %>% 
  dplyr::ungroup()
# names_labels(ds) %>% head()

# inspect transformation
set.seed(49)
ids <- sample(unique(ds$id), 3)
ids <- 33027
ds %>% 
  dplyr::filter(id %in% ids) %>% 
  dplyr::select(id, birth_date,age_at_bl,fu_year,age_at_visit,age_death, died  ) %>% 
  print()



