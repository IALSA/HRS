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

ds %>% distinct(hhidpn)
# -----Examine lbwaves -------------------------------------------------------------

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

ds_test <- subset(ds, hhidpn==10372010 |hhidpn==16706020, select = c(hhidpn,closespouse,closechild,digit1,digit2))
ds_2 <- subset(ds, select = c(hhidpn,closespouse,closechild,digit1,digit2))
# Print the data for the hhidpn's of those with remaining outlier values.
check <- which(ds_2$closechild>20)
ids <- ds_2[check,"hhidpn"]
for (i in ids){
  print(ds %>% dplyr::filter(hhidpn==i))
}

# test
ds[178,"closechild"]

ds[check,"closechild"] <- NA

# test
ds[178,"closechild"]

check <- which(ds$closechild >10)
for (i in check){
  print(ds[i,])
}

ids <- ds[check,"hhidpn"]
for (i in ids){
  print(ds %>% dplyr::filter(hhidpn==i))
}

ds %>% dplyr::filter(hhidpn==919600010)

# ------ closefam ----------------------------------------------------
boxplot(closefam ~ year, ds)
describe(ds$closefam)
boxplot.stats(ds$closefam, coef = 2)$out

# Create a temporary data frame including only waves with psychosocial variables.
ds_temp <- subset(ds, lbwave>0, select=c(hhidpn, year, lbwave, closechild, closefam, closefri))

# calculate the difference scores between close network member values
ds_temp <- ds_temp %>% 
  dplyr::group_by(hhidpn) %>% 
  dplyr::mutate(
    lag_closefam = abs(closefam - lag(closefam)),
    lead_closefam = abs(lead(closefam)-closefam),
    lag_closefri = abs(closefri - lag(closefri)),
    lead_closefri = abs(lead(closefri)- closefri)
  )%>% 
  dplyr::ungroup()

criteria <- ds_temp$lag_closefam > (4*sd(ds_temp$lag_closefam,na.rm = TRUE))
criteria2 <- ds_temp$lead_closefam > (4*sd(ds_temp$lead_closefam,na.rm = TRUE))
fullcriteria <- criteria==TRUE | criteria2==TRUE

ds_temp$famflag <- ifelse(fullcriteria==TRUE, 1, 0)

# check values over 20 
check <- which(ds_temp$closefam >20 & ds_temp$famflag==1)

ids <- as.vector(ds_temp[check,"hhidpn"])
ids <- as.vector(ids[["hhidpn"]])

# a sub data frame to examine those with greater than 20 on closefam and a flag for change
ds_sub <- ds_temp[ds_temp$hhidpn %in% ids, ]

# Create a line chart to examine growth over time in those with more than 20 close family members and a flag for large discrepancy scores
ggplot(ds_sub, aes(x=lbwave, y=closefam, color=as.factor(hhidpn), group=hhidpn)) +geom_line() +
  scale_color_discrete(drop=TRUE, limits=levels(as.factor(ds_sub$hhidpn)))

# a matrix of line graphs for individual participants.
ids <- sample(unique(ds_sub$hhidpn), 20)
g1 <- ds_sub %>%
  filter(hhidpn %in% ids) %>%
  ggplot(aes(x=lbwave, y=closefam))+
  geom_line(aes(group=hhidpn)) +
  facet_wrap("hhidpn") +
  theme_minimal()
g1


#-------------------
# check values over 20 that do not have a discrepancy flag
check <- which(ds_temp$closefam >20 & ds_temp$famflag!=1)

ids <- as.vector(ds_temp[check,"hhidpn"])
ids <- as.vector(ids[["hhidpn"]])

ds_sub <- ds_temp[ds_temp$hhidpn %in% ids, ]

# a matrix of line graphs for individual participants.
ids <- sample(unique(ds_sub$hhidpn), 20)
g1 <- ds_sub %>%
  filter(hhidpn %in% ids) %>%
  ggplot(aes(x=lbwave, y=closefam))+
  geom_line(aes(group=hhidpn)) +
  facet_wrap("hhidpn") +
  theme_minimal()
g1

# There are only 14 people in this group. From examination of the graphs is looks like a few are likely errors. 
# hhidpn's 55720020,84298010, 205280010, 501588010

# Graph the participants who have a close family score greater than 20 but not a flag indicating large discrepancy. 
ggplot(ds_sub, aes(x=lbwave, y=closefam, color=as.factor(hhidpn), group=hhidpn)) +geom_line() +
  scale_color_discrete(drop=TRUE, limits=levels(as.factor(ds_sub$hhidpn)))


# create a section of script to implement data changes.
row_to_change <- which(ds_temp$closefam>20 & ds_temp$famflag==1)
row_to_change2 <- which((ds2$closefam >50 & is.na(ds2$diff_closefam))==TRUE)

for (i in row_to_change){
  ds2[row_to_change,"closefam"] <- NA
}

for (i in row_to_change2){
  ds2[row_to_change2,"closefam"] <- NA
}

ggplot(data=check_greaterthan20, aes(x=lbwave2, y=closefam, color=as.factor(hhidpn), group=hhidpn)) +geom_line() +
  scale_color_discrete(guide = guide_legend(ncol=2))

# --------------- close friends ----------------------------------

# --------------------- serial-7s ------------------------------------
# ------ create a serial 7 score for 2014 as this was not available in the HRS RAND longitudinal file

# describeBy(ds$serial1, list(year=ds$year))
# variables <- c("serial1","serial2","serial3","serial4","serial5")
# for(v in variables){
#   (p <- unique(ds[,v]) %>% as.numeric())
#   (p <- p[!is.na(p)])
#   ds[,v] <- plyr::mapvalues(ds[,v], from=c(999, 998, 993, 66), to=c(NA, NA, NA, 66)) 
# }
# 
# check <- which(ds$serial1 == 992)
# for (i in check){
#   print(ds[i,])
# }
ds_14<- ds %>%
  dplyr::filter(year==2014)

sum(ds_14$serial1==93, na.rm = TRUE)
# count instances of refused (refused coded as 999)
sum(ds_14$serial1==999, na.rm=TRUE)
# count instances of don't know or NA (coded as 998)
sum(ds_14$serial1==998, na.rm = TRUE)
sum(ds_14$serial1==66, na.rm = TRUE)
# Returns the number of Don't Know or Not Ascertained responses
sum(ds_14$serial2=="998")
# Returns the number of Refused responses
sum(ds_14$serial2=="999")

#Then recodes these so that they are NA
variables <- c("serial1","serial2","serial3","serial4","serial5")
for(v in variables){
  (p <- unique(ds_14[,v]) %>% as.numeric())
  (p <- p[!is.na(p)])
  ds_14[,v] <- plyr::mapvalues(ds_14[,v], from=c(999, 998, 66), to=c(NA, NA, 66)) 
}


#create a variable of the difference between serial 1 response and serial 2 response
ds_14$serial1d <- as.numeric(7 + ds_14[,"serial1"])
ds_14$serial2d <- ds_14$serial1 - ds_14$serial2
ds_14$serial3d <- ds_14$serial2 - ds_14$serial3
ds_14$serial4d <- ds_14$serial3 - ds_14$serial4
ds_14$serial5d <- ds_14$serial4 - ds_14$serial5


sum(ds_14$serial1d == 100, na.rm = TRUE)
sum(ds_14$serial1d != 100, na.rm = TRUE)
sum(is.na(ds_14$serial1d))
sum(ds_14$serial1d==200,na.rm = TRUE)
sum(ds_14$serial1d==1000,na.rm = TRUE)
sum(ds_14$serial1d > 100, na.rm = TRUE)


ds_14[,"serial1s"] <- ifelse(ds_14[,"serial1d"] == 100, 1, ifelse(is.na(ds_14[,"serial1d"]), NA, 0))
ds_14[,"serial2s"] <- ifelse(ds_14[,"serial2d"] == 7, 1, ifelse(is.na(ds_14[,"serial2d"]), NA, 0))
ds_14[,"serial3s"] <- ifelse(ds_14[,"serial3d"] == 7, 1, ifelse(is.na(ds_14[,"serial3d"]), NA, 0))
ds_14[,"serial4s"] <- ifelse(ds_14[,"serial4d"] == 7, 1, ifelse(is.na(ds_14[,"serial4d"]), NA, 0))
ds_14[,"serial5s"] <- ifelse(ds_14[,"serial5d"] == 7, 1, ifelse(is.na(ds_14[,"serial5d"]), NA, 0))

serial7_scores <- c("serial1s","serial2s","serial3s","serial4s","serial5s")
ds_14$serial7r_tot <- apply(ds_14[serial7_scores],1,sum, na.rm = FALSE)


psych::describe(ds_14[,"serial1s"])
psych::describe(ds_14$serial7r_tot)

# Checking for conditions to implement data cleaning rules

# Rule 1 Case has four correct codes and one code that is the transposition of a correct answer is recoded to correct
# Transposition of serial1 correct answer would be 39.
which(ds_14$serial1 == 39 & ds_14$serial7r_tot == 4)

# Transposition of serial 2 correct answer (86) would be 68. 
which(ds_14$serial2 == 68 & ds_14$serial7r_tot == 4)
ds_14[17558,]
#none need to be recoded
# Transposition of serial3 correcct answer (79) would be 97.
#check for cases
which(ds_14$serial3 == 97 & ds_14$serial7r_tot == 4)
# examine the data for this case
ds_14[7061,] # Do not change

# Transposition of serial4 correct answer (72) would be 27.
which(ds_14$serial4 == 27 & ds_14$serial7r_tot == 4)

# Transposition of serial5 correct answer (65) would be 56.
check <- which(ds_14$serial5 == 56 & ds_14$serial7r_tot == 4)
for (i in check){
  print(ds_14[i,])
}
# Recode the serial7r_tot for hhidpn 501750010, 524648010, & 905808010 to 5 as these meet the data cleaning criteria.


# Check last data cleaning rule 
# Case has four correct codes and one code that contains one of the single digits of a correct answer is recoded.
# Serial1.
which((ds_14$serial1 == 3 | ds_14$serial1 == 9) & ds_14$serial7r_tot == 4)
# Serial 2 
which((ds_14$serial2 == 8 | ds_14$serial2 == 6) & ds_14$serial7r_tot == 4)
# Serial 3
which((ds_14$serial3 == 7 | ds_14$serial3 == 9) & ds_14$serial7r_tot == 4)
# Serial 4 
which((ds_14$serial4 == 7 | ds_14$serial4 == 2) & ds_14$serial7r_tot == 4)
# Serial 5
which((ds_14$serial5 == 6 | ds_14$serial5 == 5)  & ds_14$serial7r_tot == 4)
check <- which((ds_14$serial5 == 6 | ds_14$serial5 == 5)  & ds_14$serial7r_tot == 4)
for (i in check){
  print(ds_14[i,])
}
# hhidpn 500201010 meets change criteria.

recode_hhidpn <- c(501750010, 524648010, 905808010, 500201010)
for (i in recode_hhidpn){
  row <- which(ds_14$hhidpn== 501750010)
  ds_14[row, "serial7r_tot"] <- 5 
}

# examine two cases to check for accuracy of change
ds_14 %>% dplyr::filter(hhidpn==501750010)
ds_14 %>% dplyr::filter(hhidpn==10050010)

describeBy(ds_join$serial7r_tot, list(year=ds_join$year))
boxplot(serial7r_tot  ~ year, dsR)

# ------exploration with subset of relevant variables -----------
ds_long <- readRDS("./data-unshared/derived/data-long.rds")

# Create a data frame including only waves with psychosocial variables.
ds_lb <- subset(ds_long, lbwave>0)

ds_slice <- ds_lb %>%
  dplyr::slice(100:1000)

# loneliness
ggplot(data=ds_slice, aes(x=lbwave, y=score_loneliness_11, color=as.factor(hhidpn), group=hhidpn,show.legend = FALSE)) +geom_line() +
  scale_color_discrete(guide = guide_legend(ncol=2)) +
  stat_summary(fun.y = mean, geom = "line", aes(group=1))

# spouse support
ggplot(data=ds_slice, aes(x=lbwave, y=support_spouse_total, color=as.factor(hhidpn), group=hhidpn,show.legend = FALSE)) +geom_line() +
  scale_color_discrete(guide = guide_legend(ncol=2)) +
  stat_summary(fun.y = mean, geom = "line", aes(group=1))

# friends support
ggplot(data=ds_slice, aes(x=lbwave, y=support_fam_total, color=as.factor(hhidpn), group=hhidpn,show.legend = FALSE)) +geom_line() +
  scale_color_discrete(guide = guide_legend(ncol=2)) +
  stat_summary(fun.y = mean, geom = "line", aes(group=1))

# word recall delayed memory 
ggplot(data=ds_slice, aes(x=lbwave, y=wrectotd, color=as.factor(hhidpn), group=hhidpn,show.legend = FALSE)) +geom_line() +
  scale_color_discrete(guide = guide_legend(ncol=2)) +
  stat_summary(fun.y = mean, geom = "line", aes(group=1))

psych::describeBy(ds_lb$wrectotd, list(year=ds_lb$lbwave))

ds_wide<- readRDS("./data-unshared/derived/lb-data-wide.rds")

ds_wide$male <- factor(ds_wide$male, levels= c("1","2"), labels = c("MALE", "FEMALE"))

table(ds_wide$male)

class(ds_wide$intage_r_1)
mean(as.numeric(ds_wide$intage_r_1), na.rm = TRUE)
range(ds_wide$intage_r_1, na.rm=TRUE)

psych::describe(as.numeric(ds_wide$intage_r_1), na.rm = TRUE)
psych::describe(as.numeric(ds_wide$intage_r_2), na.rm = TRUE)
vars <- c("int","activity_mean_2","activity_mean_3","activity_mean_4")
psych::describe(ds_wide[vars])

vars <- c("activity_mean_1","activity_mean_2","activity_mean_3","activity_mean_4")
psych::describe(ds_wide[vars])


vars <- c("score_loneliness_11_1","score_loneliness_11_2","score_loneliness_11_3","score_loneliness_11_4")
psych::describe(ds_wide[vars])


vars <- c("wrectotd_1","wrectotd_2","wrectotd_3","wrectotd_4")
psych::describe(ds_wide[vars])

table(ds_wide$year_1)
table(ds_wide$year_2)
# ------ depression -------------
ds <- dto$depression
describeBy(ds, list(year=ds$year))
ds %>% dplyr::filter(hhidpn==32181040)

# ----- health --------
ds <- dto$health
describeBy(ds, list(year=ds$year))

