# The purpose of this script is to:
  # Select and rename variables for analysis
  # Calculate total scores and means to be used in analysis
  # 

############################
##    ##
############################

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
requireNamespace("zoo")

# ---- load-data ------------------------------------------------
ds_rand <- readRDS("./data-unshared/derived/rndhrs_p.rds")
ds_2004 <- readRDS("./data-unshared/derived/h04f1a.rds")
ds_2006 <- readRDS("./data-unshared/derived/h06f2b.rds")
ds_2008 <- readRDS("./data-unshared/derived/h08f2a.rds")
ds_2010 <- readRDS("./data-unshared/derived/hd10f5c.rds")
ds_2012 <- readRDS("./data-unshared/derived/h12f1a.rds")
ds_2014 <- readRDS("./data-unshared/derived/h14e1a.rds")

# make the variable names lowercase.
for(i in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
    cstring <- paste0("colnames(ds_",i,") <- tolower(colnames(ds_",i,"))")
    eval(parse(text=cstring)) # evaluates the content of the command string
} 

# load meta-data
(path_meta <- list.files("./data-phi-free/raw/meta/RAND-2017-02-08", full.names = T, pattern=".csv$"))
ls_meta <- list()
for(i in seq_along(path_meta)){
  section_name <- sub(".csv$","", basename(path_meta[i]) )
  ls_meta[[section_name]] <- readr::read_csv(path_meta[i]) %>% 
    dplyr::mutate(
      year = as.numeric(year),
      reversed = as.numeric(reversed)
    )
  attr(ls_meta[[section_name]],"spec") <- NULL
}
ls_meta %>%  names()

# ---- define-utility-functions -------------------
# these functions are used in all scale computations
# custom functions are placed in corresponding chunks

# renames and subsets the original data frame
subset_rename <- function(d,renaming_rules,year_){ 
  # broswer()
  items <- renaming_rules %>% 
    dplyr::filter(year==year_) %>% 
    dplyr::select(old_name,new_name) %>% 
    as.data.frame()
  # get the list of old and new names from the spreadsheet
  (old_names <- c("hhidpn",items[,"old_name"]))
  (new_names <- c("hhidpn",items[,"new_name"]))
  dnew <- d %>% 
    # dplyr::filter(hhidpn == 3010) %>% # filter a specific individual
    dplyr::select_(.dots = old_names) 
  colnames(dnew) <- new_names 
  return(dnew) 
}

# reverse the coding on selected items
# define function to reverse code a specified variable
reverse_coding <- function(d, variables){
  # d <- ds_lone
  for(v in variables){
    # v = "loneliness_1"
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    (original_values <- sort(p,decreasing = F))
    (reversed_values <- sort(p,decreasing = T))
    d[,v] <- plyr::mapvalues(d[,v], from=original_values, to=reversed_values) 
  }
  return(d)
}
# Usage:
# ds_lone <- ds_lone %>% 
# reverse_coding(reverse_these)

# standard computation of scale scores
compute_scale_score <- function(d){
  # d <- ds_long
  (col_names <- setdiff(names(d),c("year","hhidpn")))
  d[,'sum'] <-  apply(d[col_names],1,sum, na.rm = FALSE)
  d[,'mean'] <-  apply(d[col_names],1,mean, na.rm = FALSE)
  return(d)
}
# Usage:
# ds_long <- ds_long %>% compute_scale_score()


# ---- create-dto ---------------------
dto <- list()


# ----- RAND specific functions ---
#renames and subsets the RAND data frame
subset_rename_rand <- function(year_){ 
  # broswer()
  items <- rename_rand %>% 
    dplyr::filter(year==year_) %>% 
    dplyr::select(old_name,new_name) %>% 
    as.data.frame()
  # get the list of old and new names from the spreadsheet
  (old_names <- c("hhidpn",items[,"old_name"]))
  (new_names <- c("hhidpn",items[,"new_name"]))
  dnew <- ds_rand %>% 
    # dplyr::filter(hhidpn == 3010) %>% # filter a specific individual
    dplyr::select_(.dots = old_names) 
  colnames(dnew) <- new_names 
  dnew[,"year"]<- year_
  return(dnew) 
}

# For demographics variables that should not change over time use the available value to fill in NA values
# in cases where there is only one value change all to the available value.
replace_na <- function(x){
  #x <- c(NA,1,1)
  unique_values <- setdiff(unique(x),NA)
  testit::assert("Error: The value is absent or not unique", length(unique_values)==1L)
  if(length(unique_values)==1L){
    x[is.na(x)] <- unique_values
  }else{
    x[] <- NA
  }
}


# ------ RAND ----------------------------------------
# Processes the variables from the RAND longitudinal data file. 

#read in the renaming rules for the specific variables
rename_rand   <-  ls_meta[["rand"]]

# now select the rand ds 
ls_temp <- list()

ls_temp[[paste(2004)]] <- subset_rename_rand(2004)
ls_temp[[paste(2006)]] <- subset_rename_rand(2006)
ls_temp[[paste(2008)]] <- subset_rename_rand(2008)
ls_temp[[paste(2010)]] <- subset_rename_rand(2010)
ls_temp[[paste(2012)]] <- subset_rename_rand(2012)
ls_temp[[paste(2014)]] <- subset_rename_rand(2014)

# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)

ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

# Set birthyr_rand to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    birthyr_rand = replace_na(birthyr_rand)
  )

# Set birthmo_rand to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    birthmo_rand = replace_na(birthmo_rand)
    )

# Set cohort to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    cohort = replace_na(cohort)
  )

# Set reaedyrs to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    raedyrs = replace_na(raedyrs)
  )

# Set raedegrm to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    raedegrm = replace_na(raedegrm)
  )

# Set raeduc to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    raeduc = replace_na(raeduc)
  )

# Set race_rand to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    race_rand = replace_na(race_rand)
  )

# Set hispanic_rand to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    hispanic_rand = replace_na(hispanic_rand)
  )

# Set male to the available value for all years.
ds_long <- ds_long %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    male = replace_na(male)
  )

#convert year to factor for later join
ds_long$year <- as.factor(ds_long$year)


dto[["rand"]] <- ds_long

# ----- demographics ------------------
#read in the renaming rules for this specific variables
rename_demographics   <-  ls_meta[["demographics"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_demographics,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}

# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)

# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
tail(ds_long)

# convert marriage month and year to dates 
ds_long$mardate1st <- zoo::as.yearmon(as.character(paste0(ds_long$first_marriage_yr,"-",ds_long$first_marriage_mth)))
ds_long$mardate2nd <- zoo::as.yearmon(as.character(paste0(ds_long$second_marriage_yr,"-",ds_long$second_marriage_mth)))
ds_long$mardate3rd <- zoo::as.yearmon(as.character(paste0(ds_long$third_marriage_yr,"-",ds_long$third_marriage_mth)))
ds_long$mardate4th <- zoo::as.yearmon(as.character(paste0(ds_long$fourth_marriage_yr,"-",ds_long$forth_marriage_mth)))

# convert interview month and year to dates
ds_long$interviewdate <- zoo::as.yearmon(as.character(paste0(ds_long$interview_yr,"-",ds_long$interview_mth)))

# a list of variables to drop
drop_vars <- names(ds_long) %in% c("first_marriage_yr","first_marriage_mth","second_marriage_yr","second_marriage_mth","third_marriage_yr",
                                   "third_marriage_mth","fourth_marriage_yr","fourth_marriage_mth")
#drop vars
ds_long <- ds_long[!drop_vars]

dto[["demographics"]] <- ds_long


# ----- loneliness -------------
#read in the renaming rules for this specific variables
rename_loneliness   <-  ls_meta[["loneliness"]]
# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_loneliness,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)

# create a vector with names of items to be reverse scored
rename_meta <-rename_loneliness 
# use meta data to provide the rules
reverse_these <- unique( rename_meta[rename_meta$reversed==1,"new_name"] ) %>% as.data.frame()
reverse_these <- reverse_these[!is.na(reverse_these)]
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)
 
compute_loneliness_scale_score <- function(d){
  (col_names_11 <- setdiff(names(d),c("year","hhidpn")))
  (col_names_3 <- col_names_11[1:3])
  d[,"lonelysum_11"] <- apply(d[col_names_11],1,sum, na.rm = TRUE)
  d[,"lonelysum_3"] <- apply(d[col_names_3],1,sum, na.rm = TRUE)
  d[,"score_loneliness_3"] <- apply(d[col_names_3],1,mean, na.rm = TRUE)
  d$missing_count <- apply(d[col_names_11], 1, function(z) sum(is.na(z)))
  d <- d %>% 
    dplyr::mutate( 
      score_loneliness_11 = ifelse(missing_count<6, 
                    lonelysum_11/(11- missing_count),NA)
    )
  return(d)
}
# Usage:
ds_long <- compute_loneliness_scale_score(ds_long)
dto[["loneliness"]] <- ds_long


# ----- life_satisfaction -------------
#read in the renaming rules for this specific variables
rename_life_satisfaction  <-  ls_meta[["life-satisfaction"]] %>% as.data.frame()

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_life_satisfaction,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)

#compute sum and mean
ds_long <- ds_long %>% compute_scale_score()

# rename sum and mean
ds_long <- ds_long %>%
  dplyr::rename(
  life_sat_sum  = sum
  ,life_sat_mean = mean
)

dto[["life_satisfaction"]] <- ds_long

# ----- social-network -------------
#read in the renaming rules for this specific variables
rename_social_network  <-  ls_meta[["social-network"]] %>% as.data.frame()
# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_social_network,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}

# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

networkvars <- c("snspouse", "snchild", "snfamily","snfriends")
closevars <-c("closespouse", "closechild", "closefam", "closefri")

#The social network count items need to be reverse coded instead it was coded as 1 if yes 
# (e.g., yes they have children) and 5 in no (e.g., no children). I wanted to recode this so that it was 1 and 0
# this allows a social network total score to be calculated.

reverse_coding_socialnetwork <- function(d, variables){
  # d <- ds_lone
  for(v in variables){
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    d[,v] <- plyr::mapvalues(d[,v], from=5, to=0) 
    d[,v] <- plyr::mapvalues(d[,v], from=7, to=NA) 
  }
  return(d)
}

#recoding various codes to NA 
recoding_closesocialvars <- function(d, variables){
  for(v in variables){
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    d[,v] <- plyr::mapvalues(d[,v], from=c(66,98,99), to=c(NA,NA,NA))
  }
  return(d)
}

ds_long <- ds_long %>% recoding_closesocialvars(closevars)

ds_long <- ds_long %>% reverse_coding_socialnetwork(networkvars)


dto[["social_network"]] <- ds_long

# ----- social-support -------------
#read in the renaming rules for this specific variables
rename_social_support  <-  ls_meta[["social-support"]] %>% as.data.frame()
# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_social_support,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

#for perceived social support(or relationship quality) all positive items need to be reverse coded.
rename_meta <-rename_social_support
reverse_these <- unique( rename_meta[rename_meta$reversed==1,"new_name"] )
reverse_these <- reverse_these[!is.na(reverse_these)]
testit::assert("The scale does not contained reverse coded items",reverse_these==0L)
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)

# Create a function that computes positive and negative social support totals for each category.
# compute_social_support_scale_score <- function(d){
positive_spouse <- c("ssup1sp","ssup2sp","ssup3sp")
ds_long[,"support_spouse_total"] <- apply(ds_long[positive_spouse],1,sum, na.rm = FALSE)
ds_long[,"support_spouse_mean"] <- apply(ds_long[positive_spouse],1,mean, na.rm = FALSE)

negative_spouse <- c("ssup4sp","ssup5sp","ssup6sp","ssup7sp")
ds_long[,"strain_spouse_total"] <- apply(ds_long[negative_spouse],1,sum, na.rm = FALSE)
ds_long[,"strain_spouse_mean"] <- apply(ds_long[negative_spouse],1,mean, na.rm = FALSE)

positive_child <- c("ssup1ch","ssup2ch","ssup3ch")
ds_long[,"support_child_total"] <- apply(ds_long[positive_child],1,sum, na.rm = FALSE)
ds_long[,"support_child_mean"] <- apply(ds_long[positive_child],1,mean, na.rm = FALSE)

negative_child <- c("ssup4ch","ssup5ch","ssup6ch","ssup7ch")
ds_long[,"strain_child_total"] <- apply(ds_long[negative_child],1,sum, na.rm = FALSE)
ds_long[,"strain_child_mean"] <- apply(ds_long[negative_child],1,mean, na.rm = FALSE)

positive_family <- c("ssup1fam","ssup2fam","ssup3fam")
ds_long[,"support_fam_total"] <- apply(ds_long[positive_family],1,sum, na.rm = FALSE)
ds_long[,"support_fam_mean"] <- apply(ds_long[positive_family],1,mean, na.rm = FALSE)

negative_family <- c("ssup4fam","ssup5fam","ssup6fam","ssup7fam")
ds_long[,"strain_family_total"] <- apply(ds_long[negative_family],1,sum, na.rm = FALSE)
ds_long[,"strain_family_total"] <- apply(ds_long[negative_family],1,mean, na.rm = FALSE)

positive_friends <- c("ssup1fr","ssup2fr","ssup3fr")
ds_long[,"support_friend_total"] <- apply(ds_long[positive_friends],1,sum, na.rm = FALSE)
ds_long[,"support_friend_mean"] <- apply(ds_long[positive_friends],1,mean, na.rm = FALSE)

negative_friends <- c("ssup4fr","ssup5fr","ssup6fr","ssup7fr")
ds_long[,"strain_friends_total"] <- apply(ds_long[negative_friends],1,sum, na.rm = FALSE)
ds_long[,"strain_friends_mean"] <- apply(ds_long[negative_friends],1,mean, na.rm = FALSE)

dto[["social_support"]] <- ds_long

# ---------- social-contact --------
#read in the renaming rules for this specific variable
rename_social_contact  <-  ls_meta[["social-contact"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()

for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_social_contact,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)

# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)

# create a vector with names of items to be reverse scored 
# All social contact items are reversed scored 1 = Three or more times a week to 6 = less than once a year or never
rename_meta <-rename_social_contact
# use meta data to provide the rules
reverse_these <- unique( rename_meta[rename_meta$reversed==1,"new_name"] ) %>% as.data.frame()
reverse_these <- reverse_these[!is.na(reverse_these)]
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)

#Create three social contact scores one for each group, taking the average of the three contact types
child_contact_vars <- c("mtchild","spkchild","wrtchild")
ds_long[,"children_contact_mean"] <- apply(ds_long[child_contact_vars],1,mean, na.rm = FALSE)

family_contact_vars <- c("mtfam","spkfam","wrtfam")
ds_long[,"family_contact_mean"] <- apply(ds_long[family_contact_vars],1,mean, na.rm = FALSE)

friend_contact_vars <- c("mtfriend","spkfriend","wrtfriend")
ds_long[,"friend_contact_mean"] <- apply(ds_long[friend_contact_vars],1,mean, na.rm = FALSE)

dto[["social_contact"]] <- ds_long

#--------activity--------
#read in the renaming rules for this specific variables
rename_activity   <-  ls_meta[["activity"]]
# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_activity,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

# for years 2010 and later there is a 1 to 7 scale but in 2008 it was a 1 to 6 scale 
# recode the activity variable for years 2010 and later with the 6 and 7 response options binned together. 
for(i in 1:20){
  v = paste0("activity_",i)
  vold = paste0("activityorig_",i)
  ds_long[,vold] <- ds_long[,v]
  ds_long[,v] <- plyr::mapvalues(ds_long[,v], from=7, to=6) 
}

# create a vector with names of items to be reverse scored 
# All activity items are reversed scored 1 = Daily to 7 = Never/not relevant recode all so that higher numbers indicate more activity
rename_meta <-rename_activity
# use meta data to provide the rules
reverse_these <- unique( rename_meta[rename_meta$reversed==1,"new_name"] ) %>% as.data.frame()
reverse_these <- reverse_these[!is.na(reverse_these)]
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)


# select the years with the full activity scale
d_long <- ds_long %>% 
  dplyr::filter(year %in% c(2008, 2010, 2012, 2014)) %>%  # only these years
  dplyr::select(year, hhidpn, dplyr::matches("activity_")) # only these variables
# compute scale scores on subsetted ds
d_long <- compute_scale_score(d_long) %>% 
  dplyr::rename(activity_mean = mean, activity_sum = sum) %>% 
  dplyr::select(year, hhidpn, activity_mean, activity_sum)
# merge computed scales score to the general file
ds_long <- ds_long %>% dplyr::left_join(d_long)
head(ds_long)
dto[["activity"]] <- ds_long

#---------wellbeing------------
#read in the renaming rules for this specific variables
rename_wellbeing   <-  ls_meta[["wellbeing"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_wellbeing,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

# create a vector with names of items to be reverse scored 
rename_meta <-rename_wellbeing
# use meta data to provide the rules
reverse_these <- unique( rename_meta[rename_meta$reversed==1,"new_name"] ) %>% as.data.frame()
reverse_these <- reverse_these[!is.na(reverse_these)]
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)

compute_wellbeing_scale_score <- function(d){
  (col_names_7 <- setdiff(names(d),c("year","hhidpn")))
  (col_names_2 <- col_names_7[1:2])
  d[,"wellbeing_sum_7"] <- apply(d[col_names_7],1,sum, na.rm = TRUE)
  d[,"wellbeing_sum_2"] <- apply(d[col_names_2],1,sum, na.rm = TRUE)
  d[,"score_wellbeing_2"] <- apply(d[col_names_2],1,mean, na.rm = TRUE)
  d$missing_count <- apply(d[col_names_7], 1, function(z) sum(is.na(z)))
  d <- d %>% 
    dplyr::mutate( 
      score_wellbeing_7 = ifelse(missing_count<3, 
                                   wellbeing_sum_7/(7- missing_count),NA))
    d <- d %>% 
        dplyr::mutate( 
          wellbeing_sum_7 = ifelse(missing_count>0, NA, wellbeing_sum_7)
    )
  return(d)
}

ds_long <- ds_long %>% compute_wellbeing_scale_score()


dto[["wellbeing"]] <- ds_long

#------self-rated-memory------
#read in the renaming rules for this specific variables
rename_self_rated_memory   <-  ls_meta[["self-rated-memory"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_self_rated_memory,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

#srmemory needs to be recoded and 8 means Don't know or Not Ascertained and 9 means Refused
ds_long[,"srmemory"] <- plyr::mapvalues(ds_long[,"srmemory"], from=c(9,8,5,4,3,2,1), to=c(NA,NA,1,2,3,4,5)) 

#srmemoryp is recoded such that higher numbers mean memory has improved and 8 and 9 are recoded as NA
ds_long[,"srmemoryp"] <- plyr::mapvalues(ds_long[,"srmemoryp"], from=c(9,8,3,2,1), to=c(NA,NA,1,2,3)) 

dto[["srmemory"]] <- ds_long

#---word-list-recall-------
rename_wordlist   <-  ls_meta[["word-list"]] 

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_wordlist,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

dto[["wordlist"]] <- ds_long

#---mental-status-----------
rename_mental_status   <-  ls_meta[["mental-status"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_mental_status,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

mentalstatus_recode_vars <- c("msmonth", "msdate","msyear","msday","msnaming1","msnaming2","mspresident","msvp")
recoding_mentalstatus <- function(d, variables){
  for(v in variables){
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    d[,v] <- plyr::mapvalues(d[,v], from=c(5,8,9), to=c(0,NA,NA)) 
  }
  return(d)
}

# mental status items need to be recoded so that wrong answers are 0 (currently 5)
# and 8 means Don't know or Not Ascertained and 9 means Refused
ds_long <- ds_long %>% 
  recoding_mentalstatus(mentalstatus_recode_vars)

# recode count backwards variable so that 5 (incorrect) is 0 and 9 (refused or NA) is NA.
ds_long[,"countb"] <- plyr::mapvalues(ds_long[,"countb"], from = c(5,9), to =c(0,NA))
ds_long[,"countb2"] <- plyr::mapvalues(ds_long[,"countb2"], from = c(5,8,9), to =c(0,NA,NA))
# if requested participants could start over if they requested this this should be used instead 
# create a count variable to be used
ds_long[,"count"] <- ifelse(ds_long[,"countb"]==6, ds_long[,"countb2"], ds_long[,"countb"])

mentalstatus_vars <- c("msmonth", "msdate","msyear","msday","msnaming1","msnaming2","mspresident","msvp","count")
# Calculate a mental status total score by summing the items
ds_long[,"mentalstatus_tot"] <- apply(ds_long[mentalstatus_vars],1,sum, na.rm = TRUE)


dto[["mentalstatus"]] <- ds_long

#-----vocabulary--------
rename_vocabulary   <-  ls_meta[["vocabulary"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_vocabulary,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

vocab_recode_vars <- c("vocab1", "vocab2","vocab3","vocab4","vocab5")
recoding_vocab<- function(d, variables){
  for(v in variables){
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    d[,v] <- plyr::mapvalues(d[,v], from=9, to=NA) 
  }
  return(d)
}

# vocab items are coded with 9 as refused, recode this to NA scale is from 0 DK or incorrect to 2 perfectly correct.
ds_long <- ds_long %>% 
  recoding_vocab(vocab_recode_vars)

ds_long[,'vocab_total'] <-  apply(ds_long[vocab_recode_vars],1,sum, na.rm = FALSE)

dto[["vocabulary"]] <- ds_long

#-----depression--------------
rename_depression   <-  ls_meta[["depression"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_depression,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)
# ces-d items are coded with 9 as refused, 8 as dk, and 5 as no.
cesd_vars <- c("cesd1","cesd2","cesd3","cesd4","cesd5","cesd6","cesd7","cesd8")
recoding_depression<- function(d, variables){
  for(v in variables){
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    d[,v] <- plyr::mapvalues(d[,v], from=c(5, 8, 9), to=c(0, NA, NA)) 
  }
  return(d)
}

ds_long <- recoding_depression(ds_long, cesd_vars)

cesd_vars_to_reverse <- c("cesd4","cesd6")
ds_long <- reverse_coding(ds_long, cesd_vars_to_reverse)

head(ds_long)

ds_long[,'dep_total'] <-  apply(ds_long[cesd_vars],1,sum, na.rm = FALSE)

dto[["depression"]] <- ds_long

#-------health--------------------
rename_health   <-  ls_meta[["health"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_health,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

healthconditions <- c("hypertension", "diabetes", "cancer", "lungdisease", "heart", "stroke", "arthritis") 

recoding_health<- function(d, variables){
  for(v in variables){
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    d[,v] <- plyr::mapvalues(d[,v], from=c(3, 8, 9), to=c(0, NA, NA)) 
  }
  return(d)
}

ds_long[,"healthcond"] <- rowSums(ds_long[,healthconditions]==1)

exercise_vars <- c("vigorousactivity","moderateactivity","mildactivity")
recoding_exercise<- function(d, variables){
  for(v in variables){
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    d[,v] <- plyr::mapvalues(d[,v], from=c(1, 2, 3, 4, 7, 8, 9), to=c(4, 3, 2, 1, 5, NA, NA)) 
  }
  return(d)
}
ds_long <- recoding_exercise(ds_long, exercise_vars)
ds_long[,"exercise"] <- apply(ds_long[exercise_vars],1,sum, na.rm = FALSE)

dto[["health"]] <- ds_long

# ------- serial-7's ---------
# All years except 2014 will use the corrected serial 7's score from the RAND longitudinal file.
# In this section a serial 7's score for 2014 will be created.
rename_serial   <-  ls_meta[["serial7s"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_serial,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

# create a list of serial 7 count variables then recode to NA various error codes.
variables <- c("serial1","serial2","serial3","serial4","serial5")
for(v in variables){
  (p <- unique(ds_long[,v]) %>% as.numeric())
  (p <- p[!is.na(p)])
  ds_long[,v] <- plyr::mapvalues(ds_long[,v], from=c(999, 998, 66), to=c(NA, NA, 66)) 
}

#create a variable of the difference between serial responses
ds_long$serial1d <- as.numeric(7 + ds_long[,"serial1"])
ds_long$serial2d <- ds_long$serial1 - ds_long$serial2
ds_long$serial3d <- ds_long$serial2 - ds_long$serial3
ds_long$serial4d <- ds_long$serial3 - ds_long$serial4
ds_long$serial5d <- ds_long$serial4 - ds_long$serial5

# create an indicator score of whether the response was correct
ds_long[,"serial1s"] <- ifelse(ds_long[,"serial1d"] == 100, 1, ifelse(is.na(ds_long[,"serial1d"]), NA, 0))
ds_long[,"serial2s"] <- ifelse(ds_long[,"serial2d"] == 7, 1, ifelse(is.na(ds_long[,"serial2d"]), NA, 0))
ds_long[,"serial3s"] <- ifelse(ds_long[,"serial3d"] == 7, 1, ifelse(is.na(ds_long[,"serial3d"]), NA, 0))
ds_long[,"serial4s"] <- ifelse(ds_long[,"serial4d"] == 7, 1, ifelse(is.na(ds_long[,"serial4d"]), NA, 0))
ds_long[,"serial5s"] <- ifelse(ds_long[,"serial5d"] == 7, 1, ifelse(is.na(ds_long[,"serial5d"]), NA, 0))

serial7_scores <- c("serial1s","serial2s","serial3s","serial4s","serial5s")

# serial7r_tot_2014 is created because RAND does not yet contain 2014 it must be added manually.
# expect to modify in the future.
ds_long$serial7r_tot_2014 <- apply(ds_long[serial7_scores],1,sum, na.rm = FALSE)

# these hhidpn's were identified as meeting criteria for correction according to data cleaning rules.
# see (document when available) for details.
recode_hhidpn <- c(501750010, 524648010, 905808010, 500201010)
for (i in recode_hhidpn){
  row <- which(ds_long$hhidpn== 501750010)
  ds_long[row, "serial7r_tot"] <- 5 
}


dto[["serial7s"]] <- ds_long %>% dplyr::select(hhidpn, year, serial7r_tot_2014)

# ----- chronic-stressors -----------------------
rename_chronicstressors <- ls_meta[["chronic-stressors"]]

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  cstring <- paste0(
    "ls_temp[[paste(year)]] <- subset_rename(ds_",year,", rename_chronicstressors,",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# this creates a list in which each element is a dataset
# each dataset contains items from target construct for that year
lapply(ls_temp,names)
# now we combine datasets from all years into a single LONG dataset
ds_long <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_long)

chronicstress_vars <- c("healthprob","physicalprob","alcoholprob","workprob","financialprob","houseprob","relationshipprob","sickfam")

ds_long[,"stresstotal"] <- rowSums(ds_long[,chronicstress_vars])

dto[["chronic-stressors"]] <- ds_long

# Now that all scales are identified, variables renamed, and total/summary scores calculated merge dto a single long data frame.

# ---- merge-assembles-scales ----------------------------------
# merge multiple datasets that are stored as elements of a list
merge_multiple_files <- function(list, by_columns){
  Reduce(function( d_1, d_2 ) dplyr::full_join(d_1, d_2, by=by_columns), list)
}

ds_long <- merge_multiple_files(dto, by_columns = c("year","hhidpn"))

# # ---- save-to-disk ------------------------------------------------------------
names(ds_long)
saveRDS(dto, file = "./data-unshared/derived/1-dto-list.rds")
saveRDS(ds_long, file="./data-unshared/derived/1-dto.rds")

# ---- object-verification ------------------------------------------------
# the production of the 1-dto object is now complete
# we verify its structure and content:
ds <- readRDS("./data-unshared/derived/1-dto.rds")
names(ds)
# at this point the ds is a long data file


