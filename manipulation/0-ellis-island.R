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
path_folder <- "./data-unshared/raw/RAND/spss/"

filePaths <- list.files(path_folder , full.names=T, recursive=T, pattern="sav$")
fileNames <- basename(filePaths) # save only the last component

# ---- load-data ------------------------------------------------
# read SPSS files, convert to RDS, save in derived only run the first time on new computer
for(i in seq_along(filePaths)){
# for(i in 1){
filePath <- filePaths[[i]]
 fileName <- tail(strsplit(filePath, "/|.sav")[[1]], n=1)
oneFile <- Hmisc::spss.get(filePath, use.value.labels = TRUE)
saveRDS(oneFile, paste0("./data-unshared/derived/", fileName, ".rds")) # all raw data
}

# ---- load-data-rds --------------------------------
ds04 <- readRDS("./data-unshared/derived/h04f1a.rds")
ds06 <- readRDS("./data-unshared/derived/h06f2b.rds")
ds08 <- readRDS("./data-unshared/derived/h08f2a.rds")
ds10 <- readRDS("./data-unshared/derived/h10f4a.rds")
ds12 <- readRDS("./data-unshared/derived/h12e1a.rds")
ds14 <- readRDS("./data-unshared/derived/h14e1a.rds")



