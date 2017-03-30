# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R") # fonts, colors, themes 
source("./scripts/general-graphs.R") 
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("reshape2")
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS("./data-unshared/derived/dto.rds")
# each element this list is another list:
names(dto)

# ---- utility-functions ---------------------------------------------------------
# merge multiple datasets that are stored as elements of a list
merge_mulitple_files <- function(list, by_columns){
  Reduce(function( d_1, d_2 ) dplyr::full_join(d_1, d_2, by=by_columns), list)
}

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

# ---- functions-to-examime-temporal-patterns -------------------
view_temporal_pattern <- function(ds, measure, seed_value = 42){
  set.seed(seed_value)
  ds_long <- ds
  (ids <- sample(unique(ds_long$hhidpn),1))
  d <-ds_long %>%
    dplyr::filter(hhidpn %in% ids ) %>%
    dplyr::select_("hhidpn","year", measure)
  print(d)
}
# ds %>%  view_temporal_pattern("male", 2)
temporal_pattern <- function(ds, measure){
  # set.seed(seed_value)
  ds_long <- ds
  (ids <- sample(unique(ds_long$hhidpn),1))
  d <-ds_long %>%
    dplyr::filter(hhidpn %in% ids ) %>%
    dplyr::select_("hhidpn","year", measure)
  print(d)
}


# examine the descriptives over waves
over_waves <- function(ds, measure_name, exclude_values="") {
  ds <- as.data.frame(ds)
  testit::assert("No such measure in the dataset", measure_name %in% unique(names(ds)))
  # measure_name = "htval"; wave_name = "wave"; exclude_values = c(-99999, -1)
  cat("Measure : ", measure_name,"\n", sep="")
  t <- table( ds[,measure_name], ds[,"year"], useNA = "always"); t[t==0] <- ".";t
  print(t)
  cat("\n")
  ds[,measure_name] <- as.numeric(ds[,measure_name])
  
  d <- ds[!(ds[,measure_name] %in% exclude_values), ]
  a <- lazyeval::interp(~ round(mean(var),2) , var = as.name(measure_name))
  b <- lazyeval::interp(~ round(sd(var),3),   var = as.name(measure_name))
  c <- lazyeval::interp(~ n())
  dots <- list(a,b,c)
  t <- d %>%
    dplyr::select_("hhidpn","year", measure_name) %>%
    na.omit() %>%
    # dplyr::mutate_(measure_name = as.numeric(measure_name)) %>%
    dplyr::group_by_("year") %>%
    dplyr::summarize_(.dots = setNames(dots, c("mean","sd","count")))
  return(as.data.frame(t))
  
}

# ---- inspect-data -------------------------------------------------------------
dto$demographics %>% dplyr::glimpse()
dto$rand %>% dplyr::glimpse()

# ---- tweak-data --------------------------------------------------------------
dto_new <- list()

dto_new[["demographics"]] <- dto$demographics %>%
  dplyr::select(year, hhidpn, interview_yr, male, lbgiven, lbeligibility)

dto_new[["rand"]] <- dto$rand %>%
  dplyr::select(year, hhidpn, birthyr_rand, birthmo_rand, race_rand, hispanic_rand,intage_r,cohort, raedyrs,raedegrm,responded, 
                proxy,countb20r,shhidpnr, rmaritalst,rpartst,serial7r_tot)

dto_new[["life_satisfaction"]] <- dto$life_satisfaction %>% 
  dplyr::select(year, hhidpn, sum, mean) %>% 
  dplyr::rename(
    life_sat_sum  = sum
    ,life_sat_mean = mean
  )

dto_new[["loneliness"]] <- dto$loneliness %>% 
  dplyr::select(year,hhidpn,score_loneliness_3, score_loneliness_11)

dto_new[["social_network"]] <- dto$social_network %>% 
  dplyr::select(year, hhidpn, socialnetwork_total, snspouse, snchild, snfamily, snfriends)

dto_new[["social_support"]] <- dto$social_support %>% 
  dplyr::select(year, hhidpn, support_spouse_total, support_child_total, support_fam_total, support_friend_total,
                strain_spouse_total, strain_child_total, strain_family_total, strain_friends_total)

dto_new[["social_contact"]] <- dto$social_contact %>%
  dplyr::select(year, hhidpn, children_contact_mean, family_contact_mean, friend_contact_mean)
  
dto_new[["activity"]] <- dto$activity %>%
  dplyr::select(year, hhidpn, activity_mean, activity_sum)

dto_new[["srmemory"]] <- dto$srmemory %>%
  dplyr::select(year, hhidpn,srmemory, srmemoryp)

dto_new[["wordlist"]] <- dto$wordlist %>%
  dplyr::select(year, hhidpn, wrectoti, wrectotd)


dto_new[["mentalstatus"]] <- dto$mentalstatus %>%
  dplyr::select(year, hhidpn, mentalstatus_tot)

dto_new[["vocabulary"]] <- dto$vocabulary %>%
  dplyr::select(year, hhidpn, vocab_total)

dto_new[["depression"]] <- dto$depression %>%
  dplyr::select(year, hhidpn, dep_total)

dto_new[["health"]] <- dto$health %>%
  dplyr::select(year, hhidpn, healthcond, exercise)


# View(dto_new$demographics)
# View(dto_new$loneliness)
# View(dto_new$life_satisfaction)
#View(dto_new$social_support)

ds_long <- merge_mulitple_files(dto_new, by_columns = c("year","hhidpn"))

dplyr::filter(ds_long, hhidpn==3010)


# ---- force-to-static-sex ---------------------------
ds_long %>% view_temporal_pattern("male", 2) # sex
ds_long %>% over_waves("male") # 1, 2, 3, 4, 5, 6
# check that values are the same across waves
ds_long %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(male[!is.na(male)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

# grab the value for the first wave and forces it to all waves 
ds_long <- ds_long %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    male = dplyr::first(male) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()
# examine the difference
ds_long %>% over_waves("male")
ds_long %>% view_temporal_pattern("male", 2) # sex




ds <- readRDS("./data-unshared/derived/data-long.rds")

d_long <- ds %>% 
  dplyr::filter(hhidpn == 3020) %>%
  dplyr::select(hhidpn, year, male.x, race_rand, exercise, srmemory)

# ---- a function that checks for NA values in demographic variables that should be consistent -----
print(d_long)
patch_na <- function(
  d, # input dataset in long format with respect to time
  varname # name of the variable the values of which to patch
){
  # test whether variable has a single unique value or NA
  # replace all NA's values with that unique value
  d <- d_long
  d1 <- d %>%
    dplyr::group_by(hhidpn) %>%
    dplyr::mutate(
      unique_value = ifelse(unique(varname)
    )
    
}
# usage:
 d_long <- d_long %>% patch_na("male")
print(d_long)



ds_lbvars <- ds_long %>% 
  dplyr::select(score_loneliness_3, score_loneliness_11, socialnetwork_total, snspouse, snchild, 
              snfamily, snfriends,support_spouse_total, support_child_total, support_fam_total, 
              support_friend_total, strain_spouse_total, strain_child_total, strain_family_total, 
              strain_friends_total, children_contact_mean, family_contact_mean, friend_contact_mean,
              activity_mean, activity_sum)

ds_long$lbvars <- rowSums(!is.na(ds_lbvars))
head(ds_long)

# ----- Creates a variable lbwave that designates wave number based on eligibility for the leave-behind questionnaire
for(i in unique(ds_long$hhidpn)){
 #id <- 3020
  id <- i
  dsyear <- ds_long %>% dplyr::filter(hhidpn==id)
  wavecount <- 0
  wave_04 <- 0
  
  for(y in unique(dsyear$year)){
    #year <- 2014
    year <- y
    current_row <- which(ds_long$hhidpn==id & ds_long$year==year)
    cond_2004 <- !is.na(ds_long[current_row,"lbgiven"]) & ds_long[current_row,"lbgiven"] == "QUESTIONNAIRE LEFT WITH RESPONDENT"
    if(cond_2004==TRUE){
      wave_04 <- wave_04+1
      ds_long[current_row,"lbwave"] <- wave_04
      next}
    wave_cond <- !is.na(ds_long[current_row,"lbeligibility"]) & ds_long[current_row,"lbeligibility"] == "Eligible for leave behind"
    if(wave_cond==TRUE){
      wavecount <- wavecount+1
      ds_long[current_row,"lbwave"] <- wave_04 + wavecount
    }else{
      ds_long[current_row,"lbwave"] <- 0 
    }
    wave_2014 <- ds_long[current_row,"lbvars"] > 0 & year == "2014"
    if(wave_2014==TRUE){
      wavecount2014 <- 1
      ds_long[current_row,"lbwave"] <- wave_04 + wavecount + wavecount2014
    }
  }}
ds_long %>% dplyr::filter(hhidpn== "3020")

if(ds_long$lbvars>0 & ds_long$lbwave==0)

# ----- Creates a variable lbwave that designates wave number based on whether or not there are any
# non-missing leave-behind questionnaire items. 
for(i in unique(ds_long$hhidpn)){
  #id <- 3020
  id <- i
  dsyear <- ds_long %>% dplyr::filter(hhidpn==id)
  wavecount <- 0
  
  for(y in unique(dsyear$year)){
    #year <- 2012
    year <- y
    current_row <- which(ds_long$hhidpn==id & ds_long$year==year)
    
    wave_cond <- ds_long[current_row,"lbvars"] > 0
    if(wave_cond==TRUE){
      wavecount <- wavecount+1
      ds_long[current_row,"lbwave2"] <- wavecount
    }else{
      ds_long[current_row,"lbwave2"] <- 0 
    }
  }}


saveRDS(ds_long, file="./data-unshared/derived/data-long.rds")
# Examine distribution of lb waves
table(ds_long$lbwave2, ds_long$year)


ds_long[which(ds_long$lbwave2==5),]

examine <- ds_long %>% dplyr::filter(hhidpn==500172010)


# select only cases where the participant belongs to one of the HRS cohorts.
# examine the descriptives over waves
over_waves <- function(ds, measure_name, exclude_values="") {
  ds <- as.data.frame(ds)
  testit::assert("No such measure in the dataset", measure_name %in% unique(names(ds)))
  # measure_name = "htval"; wave_name = "wave"; exclude_values = c(-99999, -1)
  cat("Measure : ", measure_name,"\n", sep="")
  t <- table( ds[,measure_name], ds[,"year"], useNA = "always"); t[t==0] <- ".";t
  print(t)
  cat("\n")
  ds[,measure_name] <- as.numeric(ds[,measure_name])
  
  d <- ds[!(ds[,measure_name] %in% exclude_values), ]
  a <- lazyeval::interp(~ round(mean(var),2) , var = as.name(measure_name))
  b <- lazyeval::interp(~ round(sd(var),3),   var = as.name(measure_name))
  c <- lazyeval::interp(~ n())
  dots <- list(a,b,c)
  t <- d %>%
    dplyr::select_("hhidpn","year", measure_name) %>%
    na.omit() %>%
    # dplyr::mutate_(measure_name = as.numeric(measure_name)) %>%
    dplyr::group_by_("year") %>%
    dplyr::summarize_(.dots = setNames(dots, c("mean","sd","count")))
  return(as.data.frame(t))
  
}

# presubsetted cohort
ds_long %>% over_waves("cohort")

# subset the data frame to include only those belonging to a cohort.
ds_long <- subset(ds_long, cohort!= 0)



# check to see subsetting worked.
ds_long %>% over_waves("cohort")

# Select only hhidpn's that have at least one wave of LB completed.
ds_long2 <- ds_long %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::filter(any(lbwave2>0))
 



over_lbwaves <- function(ds, measure_name, exclude_values="") {
  ds <- as.data.frame(ds)
  testit::assert("No such measure in the dataset", measure_name %in% unique(names(ds)))
  # measure_name = "htval"; wave_name = "wave"; exclude_values = c(-99999, -1)
  cat("Measure : ", measure_name,"\n", sep="")
  t <- table( ds[,measure_name], ds[,"lbwave"], useNA = "always"); t[t==0] <- ".";t
  print(t)
  cat("\n")
  ds[,measure_name] <- as.numeric(ds[,measure_name])
  
  d <- ds[!(ds[,measure_name] %in% exclude_values), ]
  a <- lazyeval::interp(~ round(mean(var),2) , var = as.name(measure_name))
  b <- lazyeval::interp(~ round(sd(var),3),   var = as.name(measure_name))
  c <- lazyeval::interp(~ n())
  dots <- list(a,b,c)
  t <- d %>%
    dplyr::select_("hhidpn","lbwave", measure_name) %>%
    na.omit() %>%
    # dplyr::mutate_(measure_name = as.numeric(measure_name)) %>%
    dplyr::group_by_("lbwave") %>%
    dplyr::summarize_(.dots = setNames(dots, c("mean","sd","count")))
  return(as.data.frame(t))
  
}

names(ds_long)
ds_long %>% over_lbwaves("score_loneliness_11")

which(ds_long$lbwave==0 & !is.na(ds_long$score_loneliness_11))

print(ds_long[11,])

# ds <- dto_new %>% 
#   dplyr::bind_rows()

ds_long2%>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(raedegrm[!is.na(raedegrm)]))) %>%
  dplyr::arrange(desc(unique)) 

ds_long)




d_long <- ds_long %>% 
  dplyr::filter(hhidpn == 3010) %>%
  dplyr::select(hhidpn, year, male, exercise, srmemory)
d_long

# define variable properties for long-to-wide conversion
variables_static <- c("hhidpn", "male", "birthyr_rand", "birthmo_rand", "race_rand", "hispanic_rand", "cohort", "raedyrs","raedegrm")
variables_longitudinal <- setdiff(colnames(ds_long),variables_static)  # not static
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="year"]) # all except year

d_wide <- d_long %>%
  dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  dplyr::mutate(year=as.character(year)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(hhidpn) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",year)) %>%
  dplyr::select(-variable,-year) %>% 
  tidyr::spread(temp, value)
  
ds_wide <- ds_long %>%
  dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  # dplyr::mutate(wave = as.character(seq_along(unique(year)))) %>%
  # dplyr::mutate(wave = ifelse( wave %in% paste0(0:9), paste0("0",wave),wave)) %>%
  dplyr::mutate(year=as.character(year)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(hhidpn) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",year)) %>%
  dplyr::select(-variable,-year) %>% 
  tidyr::spread(temp, value)
ds_wide

# old
ds_wide <- ds_long2 %>%
  dplyr::select_(.dots = c(variables_static, "year", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  # dplyr::mutate(wave = as.character(seq_along(unique(year)))) %>%
  # dplyr::mutate(wave = ifelse( wave %in% paste0(0:9), paste0("0",wave),wave)) %>%
  dplyr::mutate(wave=as.character(year)) %>%
  dplyr::arrange(hhidpn) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",wave)) %>%
  dplyr::select(-variable,-year,-wave) %>% 
  tidyr::spread(temp, value)
ds_wide
# prepare data to be read by MPlus
ds_mplus <- sapply(ds_wide,as.numeric) %>% as.data.frame()
ds_mplus[is.na(ds_mplus)] <- -9999 # replace NA with a numerical code
ds_mplus %>% dplyr::glimpse()

# ---- save-r-data -------------------
# tranformed data with supplementary variables
saveRDS(ds,paste0(generic_path,"data-long-plus.rds"))
# only variables used in analysis
saveRDS(ds_long,paste0(generic_path,"data-long.rds"))
saveRDS(dto, file="./data-unshared/derived/data-wide.rds")
# prepared for Mplus
write.table(ds_mplus, "./data-unshared/derived/wide-dataset.dat", row.names=F, col.names=F)
write(names(ds_mplus), "./data-unshared/derived/wide-variable-names.txt", sep=" ")

data <- readRDS(paste0(generic_path,"data-wide.rds"))




