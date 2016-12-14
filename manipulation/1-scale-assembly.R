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
# path_input_map <- "./data-shared/raw/mhsu-service-types/mhsu-service-type-mapping-2016-09-02.csv"
#read in the renaming rules for this specific variables
rename_loneliness   <-  readxl::read_excel(path_renaming_rules, sheet = "loneliness")

subset_rename <- function(d,renaming_rules,year_){ 
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

# now cycle through all ds for each year (must have ds_2004, ds_2006 objects)
ls_temp <- list()
for(year in c(2004, 2006, 2008, 2010, 2012)){ 
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
ds_lone <- plyr::ldply(ls_temp, data.frame,.id = "year" ) %>% 
  dplyr::arrange(hhidpn)
head(ds_lone)



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

ds_lone %>% dplyr::filter(hhidpn==10001010)
ds_lone <- ds_lone %>% 
  reverse_coding(
  c(
    "loneliness_1",
    "loneliness_2",
    "loneliness_3",
    "loneliness_5"
    )
)
d <- ds_lone %>% dplyr::filter(hhidpn==10001010)



#create a variable to indicate the number of missing loneliness scale items.
data$lonemiss <- (is.na(data$loneliness_1)+is.na(data$loneliness_2)+is.na(data$loneliness_3)+is.na(data$loneliness_4)+is.na(data$loneliness_5)+is.na(data$loneliness_6)+is.na(data$loneliness_7)+is.na(data$loneliness_8)+is.na(data$loneliness_9)+is.na(data$loneliness_10)+is.na(data$loneliness_11))

ds_lone$missing_count <- apply(ds_lone, 1, function(z) sum(is.na(z)))

ds_lone <- ds_lone %>% 
  dplyr::mutate(
    mean = ifelse(missing_count < 6, sum)
  )
head(ds_lone)
ifelse(data$lonemiss<6, data$loneliness_total/(11-data$lonemiss), NA)

#check
summary(data$lonemiss)

s <- which(colnames(data)=="loneliness_1")
f <- which(colnames(data)=="loneliness_11")

data$loneliness_total <- rowSums(data[s:f], na.rm=TRUE)

#Create the loneliness scale score only if there is less than 6 missing values
#this is as per codebook instructions.
data$loneliness_mean <- ifelse(data$lonemiss<6, data$loneliness_total/(11-data$lonemiss), NA)

summary(data$loneliness_mean)
return(data)



compute_longeliness_scores(d,short_years = c(2004,2006))



compute_loneliness_scale_score <- function(d){
  d <- ds_lone %>% dplyr::filter(hhidpn==10001010)
  target <- d %>% 
    dplyr::select(-year,-hhidpn) %>% 
    as.data.frame()
  target <- lapply(target,as.numeric) %>% as.data.frame()
  head(target)
  d[,"sum"] <- apply(target,1,sum, na.rm = TRUE)
  head(d)
  d$missing_count <- apply(d, 1, function(z) sum(is.na(z)))
  d <- d %>% 
    dplyr::mutate(
      total_count = ifelse(year==2004,4,
                                ifelse(year==2006,3,11))
    )
  d <- d %>% 
    dplyr::mutate( 
      mean = ifelse(total_count==11 | missing_count<6, 
                    sum/(total_count - missing_count),
                    ifelse(total_count %in% c(3,4),
                           sum/(total_count - missing_count),NA))
    )
     
    
  head(d)
  ifelse(data$lonemiss<6, data$loneliness_total/(11-data$lonemiss), NA)
  
  
  
  
  d[,"sum_3"] <- apply(target[,1:3],1,sum, na.rm = FALSE)
  d[,"mean_3"] <- apply(target[,1:3],1,mean, na.rm = TRUE)
  # head(d) 
  d[,"sum"] <- apply(target,1,sum, na.rm = TRUE)
  d[,"mean"] <- apply(target, 1,mean, na.rm = TRUE)
  head(d) 
}
  
target <- ds_lone %>% 
  dplyr::select(-year,-hhidpn) %>% 
  as.data.frame()
str(target)
head(target)
str(target)

str(target)
ds_lone[,"sum"] <- apply(target,1,sum, na.rm = TRUE)
head(ds_lone)  
  

















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

