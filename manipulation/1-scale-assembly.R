# The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/0-ellis-island.R",
#   output="./manipulation/stitched-output/0-ellis-island.md"
# )
# The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!

############################
##  Land on Ellis Island  ##
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

# ---- declare-globals ----------------------------------------------
path_renaming_rules <- "./data-phi-free/raw/renaming-rules/renaming-rules.xlsx"

# ---- load-data ------------------------------------------------
ds_2004 <- readRDS("./data-unshared/derived/h04f1a.rds")
ds_2006 <- readRDS("./data-unshared/derived/h06f2b.rds")
ds_2008 <- readRDS("./data-unshared/derived/h08f2a.rds")
ds_2010 <- readRDS("./data-unshared/derived/hd10f5c.rds")
ds_2012 <- readRDS("./data-unshared/derived/h12f1a.rds")
ds_2014 <- readRDS("./data-unshared/derived/h14e1a.rds")
#print(colnames(ds_2006))
# colnames(ds04) <- tolower(colnames(ds04))
# colnames(ds06) <- tolower(colnames(ds06))
# colnames(ds08) <- tolower(colnames(ds08))
# colnames(ds10) <- tolower(colnames(ds10))
# colnames(ds12) <- tolower(colnames(ds12))
# colnames(ds14) <- tolower(colnames(ds14))
 
for(i in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
    # create a string to be passed as command to the eval() function
    # i <- 2004
    cstring <- paste0("colnames(ds_",i,") <- tolower(colnames(ds_",i,"))")
    eval(parse(text=cstring)) # evaluates the content of the command string
} 

# ----- dummy -----------------------
# names_labels(ds04)
# dim(ds04)


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
colnames(ds_2012)
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

# ---- create-dto ---------------------
dto <- list()

# ----- demographics ------------------
# path_input_map <- "./data-shared/raw/mhsu-service-types/mhsu-service-type-mapping-2016-09-02.csv"
#read in the renaming rules for this specific variables
rename_demographics   <-  readxl::read_excel(path_renaming_rules, sheet = "demographics")
# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){
  # create a string to be passed as command to the eval() function
  # year <- 2006
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
head(ds_long)

dto[["demographics"]] <- ds_long


# ----- loneliness -------------
# path_input_map <- "./data-shared/raw/mhsu-service-types/mhsu-service-type-mapping-2016-09-02.csv"
#read in the renaming rules for this specific variables
rename_loneliness   <-  readxl::read_excel(path_renaming_rules, sheet = "loneliness")

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  # year <- 2006
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
head(ds_lone)

ds_long %>% dplyr::filter(hhidpn==10001010)

# create a vector with names of items to be reverse scored
rename_meta <-rename_loneliness 
# use meta data to provide the rules
reverse_these <- unique( rename_meta[rename_meta$reversed==TRUE,"new_name"] ) %>% as.data.frame()
reverse_these <- reverse_these[!is.na(reverse_these)]
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)
 
# d <- ds_long %>% dplyr::filter(hhidpn==10001010)
compute_loneliness_scale_score <- function(d){
  # d <- ds_long %>% dplyr::filter(hhidpn %in% c(3010,10281010))
  (col_names_11 <- setdiff(names(d),c("year","hhidpn")))
  (col_names_3 <- col_names_11[1:3])
  d[,"sum_11"] <- apply(d[col_names_11],1,sum, na.rm = TRUE)
  d[,"sum_3"] <- apply(d[col_names_3],1,sum, na.rm = TRUE)
  d[,"score_loneliness_3"] <- apply(d[col_names_3],1,mean, na.rm = TRUE)
  d$missing_count <- apply(d[col_names_11], 1, function(z) sum(is.na(z)))
  d <- d %>% 
    dplyr::mutate( 
      score_loneliness_11 = ifelse(missing_count<6, 
                    sum_11/(11- missing_count),NA)
    )
  return(d)
}
# Usage:
ds_long <- compute_loneliness_scale_score(ds_long)
dto[["loneliness"]] <- ds_long



# ----- life_satisfaction -------------
#path_input_map <- "./data-shared/raw/mhsu-service-types/mhsu-service-type-mapping-2016-09-02.csv"
#read in the renaming rules for this specific variables
rename_life_satisfaction  <-  readxl::read_excel(
  path_renaming_rules, 
  sheet = "lifesatisfaction"
) %>% as.data.frame()

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  # year <- 2006
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
head(ds_long)

rename_meta <-rename_life_satisfaction 
reverse_these <- unique( rename_meta[rename_meta$reversed==TRUE,"new_name"] )
reverse_these <- reverse_these[!is.na(reverse_these)]
testit::assert("The scale does not contained reverse coded items",reverse_these==0L)

# d <- ds_long %>% dplyr::filter(hhidpn==10001010)
ds_long <- ds_long %>% compute_scale_score()
head(ds_long)
dto[["life_satisfaction"]] <- ds_long

# ----- social-network -------------
#read in the renaming rules for this specific variables
rename_social_network  <-  readxl::read_excel(
  path_renaming_rules, 
  sheet = "socialnetwork"
) %>% as.data.frame()

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  # year <- 2006
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

#The social network count items do not need to be reverse coded instead it was coded as 1 if yes 
# (e.g., yes they have children) and 5 in no (e.g., no children). I wanted to recode this so that it was 1 and 0
# this allows a social network total score to be calculated.

reverse_coding_socialnetwork <- function(d, variables){
  # d <- ds_lone
  for(v in variables){
    # v = "loneliness_1"
    (p <- unique(d[,v]) %>% as.numeric())
    (p <- p[!is.na(p)])
    d[,v] <- plyr::mapvalues(d[,v], from=5, to=0) 
    d[,v] <- plyr::mapvalues(d[,v], from=7, to=NA) 
  }
  return(d)
}

ds_long <- ds_long %>% 
 reverse_coding_socialnetwork(networkvars)
#Computes two scores socialnetwork_total a count of whether or not network members exist in each of the 
# four possible categories and close_social_network a count of the total number of relationships the respondent
# considers close relationships across all relational categories.
compute_socialnetwork_scale_scores <- function(d){
  #d <- ds_long %>% dplyr::filter(hhidpn %in% c(3010,10281010))
  d[,"socialnetwork_total"] <- apply(d[networkvars],1,sum, na.rm = TRUE)
  d[,"close_social_network"] <- apply(d[closevars],1,sum, na.rm = TRUE)
  d$missing_count <- apply(d[networkvars], 1, function(z) sum(is.na(z)))
  d <- d %>% 
    dplyr::mutate( 
      socialnetwork_total = ifelse(missing_count<4, 
                                   socialnetwork_total,NA))
  d$missing_count <- apply(d[closevars], 1, function(z) sum(is.na(z)))   
   d <- d %>% 
        dplyr::mutate( 
          close_social_network = ifelse(missing_count<4, 
                                        close_social_network,NA)
    )
  return(d)
}

# d <- ds_long %>% dplyr::filter(hhidpn==10001010)
ds_long <- ds_long %>% compute_scale_score()
head(ds_long)
dto[["social_network"]] <- ds_long

# ----- social-support -------------
#read in the renaming rules for this specific variables
rename_social_support  <-  readxl::read_excel(
  path_renaming_rules, 
  sheet = "socialsupport"
) %>% as.data.frame()

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012, 2014)){ 
  # create a string to be passed as command to the eval() function
  # year <- 2006
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

#for perceived social support(or relationship quality) all items need to be reverse coded.
rename_meta <-rename_social_support
reverse_these <- unique( rename_meta[rename_meta$reversed==TRUE,"new_name"] )
reverse_these <- reverse_these[!is.na(reverse_these)]
testit::assert("The scale does not contained reverse coded items",reverse_these==0L)
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)

# d <- ds_long %>% dplyr::filter(hhidpn==10001010)
ds_long <- ds_long %>% compute_scale_score()
head(ds_long)
dto[["social_support"]] <- ds_long

#--------activity--------
#read in the renaming rules for this specific variables
rename_activity   <-  readxl::read_excel(path_renaming_rules, sheet = "activity")

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

ds_long %>% dplyr::filter(hhidpn==10001010)

# create a vector with names of items to be reverse scored 
# All activity items are reversed scored 1 = Daily to 7 = Never/not relevant recode all so that higher numbers indicate more activity
rename_meta <-rename_activity
# use meta data to provide the rules
reverse_these <- unique( rename_meta[rename_meta$reversed==TRUE,"new_name"] ) %>% as.data.frame()
reverse_these <- reverse_these[!is.na(reverse_these)]
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)

#Create activity summary scores
# d <- ds_long %>% dplyr::filter(hhidpn==10001010)

# take a subject
d_long <- ds_long %>% 
  dplyr::filter(year %in% c(2008, 2010, 2012, 2014)) %>%  # only these years
  dplyr::select(year, hhidpn, dplyr::matches("activity_")) # only these variables
# compute scale scores on subsetted ds
d_long <- compute_scale_score(d_long) %>% 
  dplyr::rename(activity_mean = mean, activity_sum = sum) %>% 
  dplyr::select(year, hhidpn, activity_mean, activity_sum)
# merge computed scales score to the general file
ds_long <- ds_long %>% dplyr::left_join(d_long)
dto[["activity"]] <- ds_long

#---------wellbeing------------
#read in the renaming rules for this specific variables
rename_wellbeing   <-  readxl::read_excel(path_renaming_rules, sheet = "wellbeing")

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

ds_long %>% dplyr::filter(hhidpn==10001010)

# create a vector with names of items to be reverse scored 
rename_meta <-rename_wellbeing
# use meta data to provide the rules
reverse_these <- unique( rename_meta[rename_meta$reversed==TRUE,"new_name"] ) %>% as.data.frame()
reverse_these <- reverse_these[!is.na(reverse_these)]
ds_long <- ds_long %>% 
  reverse_coding(reverse_these)

compute_wellbeing_scale_score <- function(d){
  # d <- ds_long %>% dplyr::filter(hhidpn %in% c(3010,10281010))
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
head(ds_long)
# merge computed scales score to the general file

dto[["wellbeing"]] <- ds_long

#------self-rated-memory------
#read in the renaming rules for this specific variables
rename_self_rated_memory   <-  readxl::read_excel(path_renaming_rules, sheet = "selfratedmemory")

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
rename_wordlist   <-  readxl::read_excel(path_renaming_rules, sheet = "wordlist")

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
rename_mental_status   <-  readxl::read_excel(path_renaming_rules, sheet = "mentalstatus")

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


# ---- save-to-disk ------------------------------------------------------------
names(dto)
lapply(dto, names)

# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data-unshared/derived/dto.rds", compress="xz")


# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data-unshared/derived/dto.rds")
names(dto)
# at this point the dto contains elements
# each of which is a dataset with a subset of variables
# united by type of items








# Developmental and reference code below -------------

#### Developmental script beyond this point down ####
#sources of functions used
source("./scripts/data/cassandra/selectionfunctions.R")

source(paste0(pathDir,"/scripts/data/cassandra/lb_scales.R"))
source(paste0(pathDir,"/scripts/data/cassandra/rename2004.R"))
source(paste0(pathDir,"/scripts/data/cassandra/demographics.R"))
source(paste0(pathDir,"/scripts/data/cassandra/physical_health.R"))
source(paste0(pathDir,"/scripts/data/cassandra/rename2010_2012.R"))
source(paste0(pathDir,"/scripts/data/cassandra/cognition.R"))

# remove the first character if it signifies the year of measurement
ds04 <- preparing_variable_labels(ds04, "J", "04")

ds04 <- rename2004(ds04)
ds04 <- loneliness_three_items_recode(ds04)
ds04 <- lifesatisfaction_summaryscores(ds04)
ds04 <- social_support_network_recode2004(ds04)
ds04 <- welling_scale_summarize2004(ds04)
ds04 <- physhealthrename2004_2008(ds04)
ds04 <- MentalStatus(ds04)
ds04 <- Vocabulary(ds04)

ds04$serial1[ds04$serial71==93] <- 1

psychosocial_04 <- subset(ds04, select=c('hhidpn','loneliness_1','loneliness_2','loneliness_3',"loneliness_4","loneliness_total",'loneliness_mean',                        
                                         'lifesatisfaction_mean','snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri',
                                         'close_relations','socnetwork',"mtchild", "spkchild", "wrtchild","mtfam", "spkfam", "wrtfam",
                                         "mtfriend", "spkfriend", "wrtfriend", 'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
                                         'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri',
                                         'wellbeing_total_two','wellbeing_mean_two',"memoryproblem","Activity_vigorous", "Activity_moderate", "Activity_mild"))

demo04 <- basicdemographics(ds04)

