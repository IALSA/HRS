# knitr::stitch_rmd(script="./manipulation/car-ellis.R", output="./manipulation/stitched-output/car-ellis.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # cleans console

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
base::source("./scripts/functions-common.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
# library(TabularManifest)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2")
requireNamespace("tidyr")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")
requireNamespace("reshape2") # data transformations

# ---- declare-globals ---------------------------------------------------------
# path_input  <- "./data/unshared/raw/map/ds0.rds"
path_input  <- ".../data-unshared/derived/dto_labels.rds"

# put test assert here to check the connection.
# generic_path <- "./sandbox/pipeline-demo-1/generic-data/"
generic_path <- "./data-unshared/derived/"

datalist <- readRDS("./data-unshared/derived/dto.rds")

#Create a dataframe of 
ds1<- datalist$demographics
ds2 <- datalist$loneliness
ds3 <- datalist$social_network
ds4 <- datalist$social_support
ds5 <- datalist$wordlist

#class(ds1$year)

dsmerge1 <- merge.data.frame(ds1,ds2, by = c("hhidpn","year"))
dsmerge2 <- merge.data.frame(dsmerge1,ds3, by = c("hhidpn","year"))
dsmerge3 <- merge.data.frame(dsmerge2, ds4, by = c("hhidpn","year"))
ds_long <- merge.data.frame(dsmerge3, ds5,by = c("hhidpn","year"))

# ---- functions-to-examine-temporal-patterns -------------------
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

ds %>%  view_temporal_pattern("married", 500)

temporal_pattern <- function(ds, measure){
  # set.seed(seed_value)
  ds_long <- ds
  (ids <- sample(unique(ds_long$hhidpn),1))
  d <-ds_long %>%
    dplyr::filter(hhidpn %in% ids ) %>%
    dplyr::select_("hhidpn","year", measure)
  print(d)
}

ds_long %>% temporal_pattern("wrectoti")

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
names_labels(ds0)


# ---- prepare-for-mplus ---------------------
# define variable properties for long-to-wide conversion
variables_static <- c(
  "hhidpn","year", "male","birthyr","birthmo", "race", "hispanic","degree"
)

variables_longitudinal <- setdiff(colnames(ds_long),variables_static)  # not static
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="year"]) # all except wave
# establish a wide format
ds_wide <- ds_long %>%
  # dplyr::select(id, wave, animals, word_recall_de ) %>%
  # gather(variable, value, -(id:wave)) %>%
  dplyr::select_(.dots=c(variables_static, "year", variables_longitudinal)) %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::mutate(year = ifelse( year %in% paste0(0:9), paste0("0",year),year)) %>%
  # dplyr::mutate(wave = paste0("t", wave)) %>%
  tidyr::unite(temp, variable, year) %>%
  tidyr::spread(temp, value)
ds_wide %>% dplyr::glimpse()
# prepare data to be read by MPlus
ds_mplus <- sapply(ds_wide,as.numeric) %>% as.data.frame()
ds_mplus[is.na(ds_mplus)] <- -9999 # replace NA with a numerical code
ds_mplus %>% dplyr::glimpse()


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
