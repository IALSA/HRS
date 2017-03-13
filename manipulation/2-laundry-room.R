# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(TabularManifest)
library(psych)
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
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS("./data-unshared/derived/dto_labels.rds")

# ---- inspect-data -------------------------------------------------------------
names(dto)
lapply(dto,names)

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


# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------
ds <- dto$demographics


ids <-unique(ds_demo$hhidpn)

table(ds_demo$year, ds_demo$male)
table(ds_demo$year, ds_demo$proxy_interview)
table(ds_demo$year, ds_demo$interview_language)
table(ds_demo$year, ds_demo$nursing_home)
table(ds_demo$year, ds_demo$religion)
table(ds_demo$year, ds_demo$number_marriages)
table(ds_demo$year, ds_demo$birthyr)
dto$demographics %>% histogram_discrete("male")
dto$demographics %>% histogram_continuous("age_at_visit",bin_width = 2)
boxplot(age_at_visit ~ year, ds_demo)

# ---- force-to-static-sex ---------------------------
ds %>% view_temporal_pattern("male", 2) # sex
ds %>% over_waves("male") # 1, 2, 3, 4, 5, 6
# check that values are the same across waves
ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(male[!is.na(male)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

# grab the value for the first wave and forces it to all waves 
ds <- ds %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    male   = dplyr::first(male) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()
# examine the difference
ds %>% over_waves("male")
ds %>% view_temporal_pattern("male", 2) # sex

# ---- force-to-static-birthyr ---------------------------
# Note that in the RAND files used here that birthyr is the saved birthyr from the tracker file for each year
# there are some instances where the birthdate in the tracker file does not correspond to what is called the core
# birthdate in the RAND corrections they sometimes used the tracker and sometimes the other as the correct birthdate
# for age calculations. This is flagged in birthyf. 
ds %>% view_temporal_pattern("birthyru", 9) # birthyr
ds %>% over_waves("birthyr") # 1, 2, 3, 4, 5, 6
# check that values are the same across waves
ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(birthyru[!is.na(birthyru)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

# Count the number of instances where there is more than one birthyr given. 
ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(birthyru[!is.na(birthyru)]))) %>%
  dplyr::count(unique>1) # unique > 1 indicates change over wave

ds2 <- ds %>% dplyr::filter(birthyf== "DIFFERENCE USED TRACKER")
ds3 <- ds %>% dplyr::filter(birthyf== "DIFFERENCE USED OTHER")
ds %>% dplyr::filter(hhidpn==14455011)
# Use first value for hhidpn 10565020
# use second value for hhidpn 1065031
# 

# grab the value for the first wave and forces it to all waves 
ds <- ds %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    male   = dplyr::first(male) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()



# examine the difference
ds %>% over_waves("birthyr")
ds %>% view_temporal_pattern("birthyr", 1) # sex

# --------- education -----------
ds %>% view_temporal_pattern("degree", 2) # sex
ds %>% over_waves("degree") # 1, 2, 3, 4, 5, 6

# Recode the don't know and RF responses so that they are not considered
ds[,"degree"] <- plyr::mapvalues(ds[,"degree"], from=c("DK (Don't Know); NA (Not Ascertained)","RF (Refused)"), to=c(NA, NA)) 


# check that values are the same across waves
ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(degree[!is.na(degree)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(degree[!is.na(degree)]))) %>%
  dplyr::count(unique>1) # unique > 1 indicates change over wave


discrepant_ed_ids <- c("33889020","501879011","501880011","534274010","534666020","910504010")
# examine the cases with descrepant degree values
for(i in 1:length(discrepant_ed_ids)){
  v <- discrepant_ed_ids[i]
  print(ds %>% dplyr::filter(hhidpn==v))
}
# hhidpn 33889020 had Law, Phd, Md, then other, set education to Law, Phd, MD (the first value)
# hhidpn 501879011 had a bachelors in 2006 then a masters/mba in 2008
# hhidpn 534274010 had a Masters/MBA in 2010 then is listed as bachelors in 2012 use Masters/MBA (First value)
# hhidpn 534666020 listed as Bachlors in 2010 then less than bachelors in 2012 use Bachelors (first value)
# hhidpn 910504010 listed as Bachelors in 2010 then less than bachelors in 2012 use Bachelors (first value)

# grab the value for the first wave and forces it to all waves 
ds <- ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    male   = dplyr::first(degree) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

# Trying to get degree to be the same across waves
#ds_grouped<- ds %>%
#   dplyr::group_by(hhidpn)
# 
# ds_grouped %>%
# dplyr::summarize(unique = length(unique(degree[!is.na(degree)]))) %>%
#   dplyr::count(unique>1) # unique > 1 indicates change over wave
# 
# NonNAindex <- which(!is.na(ds$degree))
# ds[NonNAindex, "degree"]
# head(NonNAindex)
# firstNonNA <- dplyr::first(ds, NonNAindex)
# firstNonNA
# dplyr::first(ds, degree)
# 
# # use the value for the first wave and force it to all waves for degree variable
# ds <- ds_grouped %>%
#   dplyr::mutate(
#     degree = dplyr::first(firstNonNA)# grabs the value for the first wave and forces it to all waves
#   ) %>%
#   dplyr::ungroup()
# 
# 
# 
# 
# print(ds %>% dplyr::filter(hhidpn==33889020))
# 

# check that values are the same across waves
ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(edyrs[!is.na(edyrs)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(edyrs[!is.na(edyrs)]))) %>%
  dplyr::count(unique>1) # unique > 1 indicates change over wave

ds[,"edyrs"] <- plyr::mapvalues(ds[,"edyrs"], from=c(97,98,99), to=c(NA, NA, NA))

ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(degree[!is.na(degree)]))) %>%
  dplyr::count(unique>1) # unique > 1 indicates change over wave
ds[,"degree"] <- plyr::mapvalues(ds[,"degree"], from=c("DK (Don't Know); NA (Not Ascertained)","RF (Refused)"), to=c(NA, NA)) 

# ----- loneliness --------
ds_lone <- dto$loneliness

describeBy(ds_lone, list(year=ds_lone$year))
boxplot(sum_11 ~ year, ds_lone)
boxplot.stats(ds_lone$sum_11, coef = 1.5)$out
boxplot(loneliness_1 ~ year, ds_lone)
# Would like to examine longitudinal trajectories to look for outliers

# ----- life_satisfaction -------------

ds_life <-dto$life_satisfaction
describeBy(ds_life, list(year=ds_life$year))

# ----- social-network ------
ds <- dto$social_network
describeBy(ds, list(year=ds$year))
boxplot(socialnetwork_total  ~ year, ds)

# ----- social-contact ------
ds <- dto$social_contact
describeBy(ds, list(year=ds$year))
boxplot(socialnetwork_total  ~ year, ds)

# -----closechild-------
ds<-dto$social_network
boxplot(closechild ~ year, ds)
boxplot.stats(ds$closechild, coef = 5)$out
which(ds$closechild==99)

boxplot(closefam ~ year, ds)

ds[178,]
ds[180,]
ds[176,]
# ------ closefam -------
boxplot(closefam ~ year, ds)
boxplot.stats(ds$closefam, coef = 2)$out

#There are some values that are suspiciously high. 
# hhidpn 53871010 has a value of 115 for close family members in 2008 but in 2012 when re-interviewed the number of close family members is 2.
# this same individual has some suspicious values for close children and close friends in 2008. 43 close children seems unlikely. 
which.max(ds$closefam)
ds[30631,]
ds[30633,]
ds[30635,]

boxplot(closefri ~ year, ds)

# ------- social-support -------

ds <- dto$social_support
describeBy(ds, list(year=ds$year))

# ------- activity -----
ds <- dto$activity
describeBy(ds, list(year=ds$year))

boxplot(activity_sum ~ year, ds)
boxplot.stats(ds$activity_sum, coef = 2)$out

which(ds$activity_sum == 123)
ds[3804,]

# investigate individuals with outliers
ds %>% dplyr::filter(hhidpn==3010)


# -------- wellbeing -----
ds <- dto$wellbeing
describeBy(ds, list(year=ds$year))
boxplot(wellbeing_sum_7  ~ year, ds)
boxplot.stats(ds$wellbeing_sum_7 , coef = 1.5)$out

which(ds$wellbeing_sum_7==7)
ds[13131,]
ds %>% dplyr::filter(hhidpn==32181040)

# ------ self-rated-memory -----
ds <- dto$srmemory
describeBy(ds, list(year=ds$year))

# ---------word-list-recall -----------
ds <- dto$wordlist
describeBy(ds, list(year=ds$year))
boxplot(wrectoti  ~ year, ds)
boxplot(wrectotd  ~ year, ds)

which(ds$wrecnoneim==1)

# -------- vocabulary -----------
ds <- dto$vocabulary
describeBy(ds, list(year=ds$year))

# ---------- mental-status -----
ds <- dto$mentalstatus
describeBy(ds, list(year=ds$year))

# ------ serial-7s ------
ds <- dto$serial7s
describeBy(ds, list(year=ds$year))
boxplot(serial1s  ~ year, ds)
sum(ds$serial1==93, na.rm = TRUE)
# count instances of refused (refused coded as 999)
sum(ds$serial1==999, na.rm=TRUE)
# count instances of don't know or NA (coded as 998)
sum(ds$serial1==998, na.rm = TRUE)

# Returns the number of Don't Know or Not Ascertained responses
sum(ds$serial2=="998")
# Returns the number of Refused responses
sum(ds$serial2=="999")

#Then recodes these so that they are NA
variables <- c("serial1","serial2","serial3","serial4","serial5")
for(v in variables){
  (p <- unique(ds[,v]) %>% as.numeric())
  (p <- p[!is.na(p)])
  ds[,v] <- plyr::mapvalues(ds[,v], from=c(999, 998), to=c(NA, NA)) 
}


#create a variable of the difference between serial 1 response and serial 2 response
sum(ds$serial1==66, na.rm = TRUE)
ds$serial1d <- as.numeric(7 + ds[,"serial1"])
ds$serial2d <- ds$serial1 - ds$serial2
ds$serial3d <- ds$serial2 - ds$serial3
ds$serial4d <- ds$serial3 - ds$serial4
ds$serial5d <- ds$serial4 - ds$serial5

head(ds$serial1d)
boxplot(serial1d  ~ year, ds)
sum(ds$serial1d == 100, na.rm = TRUE)
sum(ds$serial1d != 100, na.rm = TRUE)
sum(is.na(ds$serial1d))
sum(ds$serial1d==200,na.rm = TRUE)
sum(ds$serial1d==1000,na.rm = TRUE)
sum(ds$serial1d > 100, na.rm = TRUE)
print(d)

ds[,"serial1s"] <- ifelse(ds[,"serial1d"] == 100, 1, ifelse(ds[,"serial1d"] == 200, 1, ifelse(is.na(ds[,"serial1d"]), NA, ds[,"serial1d"])))
ds[,"serial2s"] <- ifelse(ds[,"serial2d"] == 7, 1, ifelse(is.na(ds[,"serial2d"]), NA, 0))
ds[,"serial3s"] <- ifelse(ds[,"serial3d"] == 7, 1, ifelse(is.na(ds[,"serial3d"]), NA, 0))
ds[,"serial4s"] <- ifelse(ds[,"serial4d"] == 7, 1, ifelse(is.na(ds[,"serial4d"]), NA, 0))
ds[,"serial5s"] <- ifelse(ds[,"serial5d"] == 7, 1, ifelse(is.na(ds[,"serial5d"]), NA, 0))


head(ds$serial3s)
describeBy(ds$serial2s, list(year=ds$year))
which(ds$serial1s>1)
ds[217,]

ds %>% dplyr::filter(hhidpn==32181040)

# ------ depression -------------
ds <- dto$depression
describeBy(ds, list(year=ds$year))
ds %>% dplyr::filter(hhidpn==32181040)

# ----- health --------
ds <- dto$health
describeBy(ds, list(year=ds$year))


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
