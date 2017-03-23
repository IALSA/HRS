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
dto <- readRDS("./data-unshared/derived/dto3.rds")
# each element this list is another list:
names(dto)

# ---- utility-functions ---------------------------------------------------------
# merge multiple datasets that are stored as elements of a list
merge_mulitple_files <- function(list, by_columns){
  Reduce(function( d_1, d_2 ) dplyr::full_join(d_1, d_2, by=by_columns), list)
}

# ---- inspect-data -------------------------------------------------------------
dto$demographics %>% dplyr::glimpse()
dto$rand %>% dplyr::glimpse()

# ---- tweak-data --------------------------------------------------------------
dto_new <- list()

dto_new[["demographics"]] <- dto$demographics %>%
  dplyr::select(year, hhidpn, interview_yr, male, race, hispanic,lbwave)

dto_new[["rand"]] <- dto$rand %>%
  dplyr::select(year, hhidpn, birthyr_rand, birthmo_rand, intage_r,cohort, raedyrs,raedegrm,responded, 
                proxy,countb20r,shhidpnr, rmaritalst,rpartst,serial7r_tot)

dto_new[["life_satisfaction"]] <- dto$life_satisfaction %>% 
  dplyr::select(year, hhidpn, sum, mean) %>% 
  dplyr::rename(
    life_sat_sum  = sum
    ,life_sat_mean = mean
  )

dto_new[["loneliness"]] <- dto$loneliness %>% 
  dplyr::select(year,hhidpn,score_loneliness_3, score_loneliness_11  )

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


# ds <- dto_new %>% 
#   dplyr::bind_rows()


# ---- prepare-for-mplus ---------------------
# define variable properties for long-to-wide conversion
variables_static <- c("hhidpn", "male", "birthyr_rand", "birthmo_rand",
                      "race", "hispanic", "cohort", "raedyrs", "raedegrm")

variables_longitudinal <- setdiff(colnames(ds),variables_static)  # not static
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="year"]) # all except year
# establish a wide format

ds_selected <- ds %>%
  dplyr::select_(.dots=c(variables_static, "year", variables_longitudinal))


# prepare data to be read by MPlus
ds_mplus <- sapply(ds_wide,as.numeric) %>% as.data.frame()
ds_mplus[is.na(ds_mplus)] <- -9999 # replace NA with a numerical code
ds_mplus %>% dplyr::glimpse()

ds_wide <- ds %>%
  # dplyr::select(id, wave, animals, word_recall_de ) %>%
  # gather(variable, value, -(id:wave)) %>%
  dplyr::select_(.dots=c(variables_static, "year", variables_longitudinal))
# ---- save-r-data -------------------
# tranformed data with supplementary variables
saveRDS(ds,paste0(generic_path,"data-long-plus.rds"))
# only variables used in analysis
saveRDS(ds_long,paste0(generic_path,"data-long.rds"))
saveRDS(ds_wide,paste0(generic_path,"data-wide.rds"))
# prepared for Mplus
write.table(ds_mplus, paste0(generic_path,"/wide-dataset.dat"), row.names=F, col.names=F)
write(names(ds_mplus), paste0(generic_path,"/wide-variable-names.txt"), sep=" ")

data <- readRDS(paste0(generic_path,"data-wide.rds"))




