#############################################################
# laundry room
# This script is to be run following 1-scale-assembly.R
# The purpose of this script is to take the long format data frame produced by that script and perform data cleaning operations.
# Data exploration and explanations of rational for data cleaning can be found (insert that here when available)

# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(TabularManifest)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
# source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graph-presets.R") # fonts, colors, themes 
# source("./scripts/general-graphs.R") 
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("psych") # For descriptive functions

# ---- load-data ---------------------------------------------------------------
# load the product of 1-scale-assembly.R a long data file
ds <- readRDS("./data-unshared/derived/data-long.rds")

# ---- inspect-data -------------------------------------------------------------
names(ds)

# --------------------------------------------------------------------------------------
# Number of close children (closechild) data correction 

# Impliment Rule 1:	
# If the number of close children listed was a double digit (e.g., 22, 33, 44) the number of 
# children was made equal to the single digit. 
# [This solves the problem for the majority of cases with greater than 20 close children from 239 to 86]

# create separate variables for each digit.
ds$digit1 <- substr(ds$closechild,1,1)
ds$digit2 <- substr(ds$closechild,2,3)


ds$digit1 <- plyr::mapvalues(ds$digit1, from=c("N"), to=c(NA))
ds$digit2 <- plyr::mapvalues(ds$digit2, from=c("aN"), to=c(NA))

# replace the double values with the single digit value.
ds$closechild <- as.numeric(ifelse(ds$digit1 == ds$digit2, ds$digit2, ds$closechild))

# Impliment Rule 2:
# Otherwise, recode closechild [number of children with whom one has a close relationship to NA if greater than]
ds$closechild <- ifelse(ds$closechild>20, NA, ds$closechild)

# ------ Number of close family members (closefam) data correction -------
# Rule 1:
# If the number of close family members is greater than 4 standard deviations above the mean (21) 
# and the change in number of close family members is greater than 4 standard deviations above the 
# mean change (21) then recode to NA. This is 112 cases. 

# compute_socialnetwork_scale_scores <- function(d){
#   #d <- ds_long %>% dplyr::filter(hhidpn %in% c(3010,10281010))
#   d[,"socialnetwork_total"] <- apply(d[networkvars],1,sum, na.rm = TRUE)
#   d[,"close_social_network"] <- apply(d[closevars],1,sum, na.rm = TRUE)
#   d$missing_count <- apply(d[networkvars], 1, function(z) sum(is.na(z)))
#   d <- d %>% 
#     dplyr::mutate( 
#       socialnetwork_total = ifelse(missing_count<4, 
#                                    socialnetwork_total,NA))
#   d$missing_count <- apply(d[closevars], 1, function(z) sum(is.na(z)))   
#   d <- d %>% 
#     dplyr::mutate( 
#       close_social_network = ifelse(missing_count<4, 
#                                     close_social_network,NA)
#     )
#   return(d)
# }
# 
# ds <- ds %>% compute_socialnetwork_scale_scores()

# - select only those who are older than 65 for the analysis

ds <- subset(ds, intage_r > 64)

#-Select only relevant demographic variables and total scores for analysis----------

# list variables to keep separated for long to wide conversion
variables_static <- c("hhidpn", "male", "birthyr_rand", "birthmo_rand", "race_rand", "hispanic_rand", "cohort", "raedyrs","raedegrm")

variables_longitudinal <- c("year","lbwave","responded","proxy","countb20r","shhidpnr","rmaritalst","intage_r","rpartst","score_loneliness_3", "score_loneliness_11",
                            "snspouse", "snchild", "snfamily", "snfriends",
                            "support_spouse_total", "support_child_total", "support_fam_total", "support_friend_total",
                            "strain_spouse_total", "strain_child_total", "strain_family_total", "strain_friends_total",
                            "children_contact_mean", "family_contact_mean", "friend_contact_mean",
                            "activity_mean", "activity_sum","srmemory", "srmemoryp","wrectoti", "wrectotd","mentalstatus_tot","vocab_total",
                            "dep_total","healthcond", "exercise")  # not static



# a year based wide data set
d_wide <- ds %>%
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

d_long <- ds %>%
  dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal)) 




ds_lb <- subset(d_long, lbwave>0 & lbwave!=5)
# define variable properties for long-to-wide conversion
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="lbwave"]) # all except year

# an lb wave based wide data set
dlb_wide <- ds_lb %>%
  dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  dplyr::mutate(lbwave=as.character(lbwave)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(hhidpn) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",lbwave)) %>%
  dplyr::select(-variable,-lbwave) %>% 
  tidyr::spread(temp, value)


# ---- save-r-data -------------------
# tranformed data with supplementary variables
#saveRDS(ds,"./data-unshared/derived/data-long-select.rds")

saveRDS(d_wide, file="./data-unshared/derived/data-wide.rds")
# lb wave based wide file
saveRDS(dlb_wide, file="./data-unshared/derived/lb-data-wide.rds")

# convert NA and NaN to 9999 for Mplus.
dlb_wide[is.na(dlb_wide)] <- 9999
d_long[is.nan(d_long)]<- 9999
dlb_wide <- rapply(dlb_wide, f=function(dlb_wide[,"activity_mean_1"]) ifelse(is.nan(dlb_wide[,"activity_mean_1"]),9999,dlb_wide[,"activity_mean_1"]), how="replace")
# prepared for Mplus
write.table(dlb_wide, "./data-unshared/derived/wide-dataset.dat", row.names=F, col.names=F)
write(names(dlb_wide), "./data-unshared/derived/wide-variable-names.txt", sep=" ")

# ---- publisher ---------------------------------------
path_report_1 <- "./reports/report.Rmd"
allReports <- c(
  path_report_1
  # ,path_report_2
  # , ...
)
pathFilesToBuild <- c(allReports) ##########
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}
