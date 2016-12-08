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
pathFileBL       <- file.path("./data-unshared/raw/dataset_484_basic_2016-09-09.csv")
pathFileLong     <- file.path("./data-unshared/raw/dataset_484_long_2016-09-09.csv")
pathBaselineDate <- file.path("./data-unshared/raw/baseline-date-2015-11.csv") 
# check if the files exist
testit::assert("File does not exist", base::file.exists(pathFileBL))
testit::assert("File does not exist", base::file.exists(pathFileLong))
testit::assert("File does not exist", base::file.exists(pathBaselineDate))

# ---- load-data ------------------------------------------------
# load data objects

# Baseline data indicator
BL_date  <- readr::read_csv(pathBaselineDate) %>% 
  dplyr::mutate(projid = as.integer(projid)) %>% 
  dplyr::select(-visit) 
# Baseline measures
BL_raw   <- read.csv(pathFileBL, stringsAsFactors = FALSE) %>% # baseline measure
  # dplyr::rename(id = projid)%>% 
  dplyr::select(-dplyr::ends_with(".1")) # remove duplicated variables
# longitudinal observations
Long_raw <- read.csv(pathFileLong, stringsAsFactors = FALSE) %>%  # longitudinal observations
  # dplyr::rename(id = projid)%>% 
  dplyr::select(-dplyr::ends_with(".1")) %>% # remove duplicated variables
  dplyr::select(-alcohol_g) # this variable repeats in baseline

# combine the files
ds <- BL_raw %>% dplyr::left_join(BL_date, by = "projid")
ds <- ds %>% dplyr::left_join(Long_raw, by = c("projid","study"))
# metadata 
# meta <- readr::read_csv("./data-phi-free/meta/meta-data- map-2016-09-09.csv")
meta <- readr::read_csv("./data-phi-free/meta/meta-data-map-2016-11-27.csv")
# check the number of subjects
length(unique(BL_date$projid)) # MAP, ROS, MARS
length(unique(BL_raw$projid)) # MAP
length(unique(Long_raw$projid))# MAP


# ----- rename-variables -------------------------------------
# todo : optimize the following script

# get df with old (names to be replaced)
names_old <- as.data.frame(colnames(ds)) 
names(names_old) <- "name"; head(names_old); length(names_old$name)
names_old <- names_old %>% 
  dplyr::mutate(name = as.character(name))
# get df with new (names to replace the old ones)
names_new <- meta %>% 
  dplyr::select(name, name_new) %>% 
  dplyr::mutate(name = as.character(name)) %>%
  as.data.frame()
head(names_new); length(names_new$name)
# check for missing names
setdiff(names_old$name, names_new$name)
# pair up old and new
(replaced <- names_old %>% 
  dplyr::left_join(names_new, by = "name"))

colnames(ds) <- replaced[,"name_new"]

# ---- assing-variable-labels --------------------------------
for(name_ in names(ds)){
  attr(ds[,name_], "label") <- meta[meta$name_new == name_, "label"]
}

# ---- inspect-data ----------------------------------------------
# inspect loaded data objects (using basic demographic variables )
length(unique(ds$id)) # there are this many of subjects
# t <- table(ds[,"fu_year"], ds[,"died"]); t[t==0]<-".";t
t <- table(ds[,"msex"], ds[,"race"], useNA = "always"); t[t==0]<-".";t
t <- table(ds[,"educ"], ds[,"race"]); t[t==0]<-".";t

# ---- assemble-data-object-dto-1 --------------------------------------
dto <- list()
# the first element of data transfer object contains unit data
dto[["unitData"]] <- ds
# the second element of data transfer object contains meta data
dto[["metaData"]] <-  meta
# verify and glimpse
dto[["unitData"]] %>% dplyr::glimpse()
dto[["metaData"]] %>% dplyr::glimpse()
# names_labels(ds) %>% head()
# ---- save-to-disk ------------------------------------------------------------
# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data-unshared/derived/dto_raw.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data-unshared/derived/dto_raw.rds")
names(dto)
# 1st element - unit(person) level data
names(dto[["unitData"]])
# 2nd element - meta data, info about variables
names(dto[["metaData"]])
