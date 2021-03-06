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


# ---- load-data ------------------------------------------------
ds_2004 <- readRDS("./data-unshared/derived/h04f1a.rds")
ds_2006 <- readRDS("./data-unshared/derived/h06f2b.rds")
ds_2008 <- readRDS("./data-unshared/derived/h08f2a.rds")
ds_2010 <- readRDS("./data-unshared/derived/h10f4a.rds")
ds_2012 <- readRDS("./data-unshared/derived/h12e1a.rds")
# ds14 <- readRDS("./data-unshared/derived/h14e1a.rds")

# colnames(ds04) <- tolower(colnames(ds04))
# colnames(ds06) <- tolower(colnames(ds06))
# colnames(ds08) <- tolower(colnames(ds08))
# colnames(ds10) <- tolower(colnames(ds10))
# colnames(ds12) <- tolower(colnames(ds12))
 
for(i in c(2004, 2006, 2008, 2010, 2012)){ 
    # create a string to be passed as command to the eval() function
    # i <- 2004
    cstring <- paste0("colnames(ds_",i,") <- tolower(colnames(ds_",i,"))")
    eval(parse(text=cstring)) # evaluates the content of the command string
} 

# ----- dummy -----------------------
# names_labels(ds04)
# dim(ds04)



# ----- loneliness -------------
# select leneliness items from each wave
items_2004 <- c(
   "jlb504a" = "loneliness_1"
  ,"jlb504b" = "loneliness_2"
  ,"jlb504c" = "loneliness_3"
  ,"jlb504d" = "loneliness_4"
  )

items_2006 <- c(
   "klb020a" = "loneliness_1"
  ,"klb020b" = "loneliness_2"
  ,"klb020c" = "loneliness_3"
) 
items_2008 <- c(
   "llb020a" = "loneliness_1"
  ,"llb020b" = "loneliness_2"
  ,"llb020c" = "loneliness_3"
  ,"llb020d" = "loneliness_4"
  ,"llb020e" = "loneliness_5"
  ,"llb020f" = "loneliness_6"
  ,"llb020g" = "loneliness_7"
  ,"llb020h" = "loneliness_8"
  ,"llb020i" = "loneliness_9"
  ,"llb020j" = "loneliness_10"
  ,"llb020k" = "loneliness_11" 
)
items_2010 <- c(
   "mlb020a"= "loneliness_1"
  ,"mlb020b"= "loneliness_2"
  ,"mlb020c"= "loneliness_3"
  ,"mlb020d"= "loneliness_4"
  ,"mlb020e"= "loneliness_5"
  ,"mlb020f"= "loneliness_6"
  ,"mlb020g"= "loneliness_7"
  ,"mlb020h"= "loneliness_8"
  ,"mlb020i"= "loneliness_9"
  ,"mlb020j"= "loneliness_10"
  ,"mlb020k"= "loneliness_11" 
)

items_2012 <- c(
   "nlb020a"= "loneliness_1"
  ,"nlb020b"= "loneliness_2"
  ,"nlb020c"= "loneliness_3"
  ,"nlb020d"= "loneliness_4"
  ,"nlb020e"= "loneliness_5"
  ,"nlb020f"= "loneliness_6"
  ,"nlb020g"= "loneliness_7"
  ,"nlb020h"= "loneliness_8"
  ,"nlb020i"= "loneliness_9"
  ,"nlb020j"= "loneliness_10"
  ,"nlb020k"= "loneliness_11" 
)

# subset_rename <- function(d, items, year){ 
subset_rename <- function(d, items){ 
  # d <- ds_2004
  # items <- items_2004
  # year <- 2004
  # 
  old_names <- c("hhidpn",names(items))
  new_names <- c("hhidpn",paste0(items))
  # new_names <- c("hhidpn",paste0("year",year,"_",items))
  # new_names <- c(paste0("year",year,"_",c("hhidpn",items)))
  dnew <- d %>% 
    dplyr::filter(hhidpn == 3010) %>%
    dplyr::select_(.dots = old_names) 
  colnames(dnew) <- new_names 
  return(dnew) 
}
# d04 <- subset_rename(ds_2004, items_2004, 2004 )
# head(d04)

ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012)){ 
  # create a string to be passed as command to the eval() function
  # year <- 2004
  cstring <- paste0("ls_temp[[paste(year)]] <- subset_rename(ds_",year,", items_",year,")")
  # cstring <- paste0("ls_temp[[paste(year)]] <- subset_rename(ds_",year,", items_",year,",",year,")")
  eval(parse(text=cstring)) # evaluates the content of the command string
}
# lapply(ls_temp,names)
# lapply(ls_temp,nrow)

ds_lone <- plyr::ldply(ls_temp, data.frame,.id = "year" )


ls_temp$`2004` 


ds_lone %>% dplyr::filter(hhidpn == 3010) %>% dplyr::count()

lapply(ls_temp, class)
names(ls_temp)
class(ls_temp)

str(ls_temp)
d04 <- subset_rename(ds04, items_04)
d06 <- subset_rename(ds06, items_06)
d08 <- subset_rename(ds08, items_08)
d10 <- subset_rename(ds10, items_10)
d12 <- subset_rename(ds12, items_12)



old_names <- c("hhidpn",names(items_04))
new_names <- c("id",items_04)
d04 <- ds04 %>% 
  dplyr::select_(.dots = old_names) 
colnames(d04) <- new_names
  

d06 <- ds06 %>% 
  dplyr::select_(.dots = c("hhidpn", items_06))



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

