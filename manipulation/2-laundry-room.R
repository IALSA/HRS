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
ds<- dto$demographics

# examine basic descriptives
table(ds$year, ds$male)

# basic histogram
dto$demographics %>% histogram_discrete("male")


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
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    male = dplyr::first(male) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()
# examine the difference
ds %>% over_waves("male")
ds %>% view_temporal_pattern("male", 2) # sex

dto[["demographics"]] <- ds

# -------- force-to-static-race -----------
dsR <- dto$rand

dsR %>% view_temporal_pattern("race_rand", 2) # race

# check that values are the same across waves
dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(race_rand[!is.na(race_rand)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

examine <- dsR %>% dplyr::filter(hhidpn== "10210010")

dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(race_rand[!is.na(race_rand)]))) %>%
  dplyr::count(unique>1) # unique > 1 indicates change over wave

# grab the value for the first wave and forces it to all waves 
dsR <- dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    race_rand = dplyr::first(race_rand) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()
# examine the difference
dsR %>% over_waves("race_rand")
ds %>% view_temporal_pattern("race_rand", 2) # sex

# grab the value for the first wave and forces it to all waves 
dsR <- dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    hispanic_rand = dplyr::first(hispanic_rand) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

dto[["rand"]] <- dsR

# ---- force-to-static-birthyr ---------------------------
# Note that in the RAND files used here that birthyr is the saved birthyr from the tracker file for each year
# there are some instances where the birthdate in the tracker file does not correspond to what is called the core
# birthdate in the RAND corrections they sometimes used the tracker and sometimes the other as the correct birthdate
# for age calculations. This is flagged in birthyf. Therefore using the HRS Rand Longitidinal file version of birth year
# and birth month is recommended. 


# example of a case with different birth dates in the RAND fat files versus the RAND longitudinal files
dsR %>% dplyr::filter(hhidpn== "35258020")
ds %>% dplyr::filter(hhidpn== "35258020")

# check that values are the same across waves (they are for birthyr_rand)
dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(birthyr_rand[!is.na(birthyr_rand)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

# Count the number of instances where there is more than one birthyr given in HRS Rand Fat files demographics. 
ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(birthyru[!is.na(birthyru)]))) %>%
  dplyr::count(unique>1) # unique > 1 indicates change over wave

# grab the value for the first wave and forces it to all waves (birth year)
dsR <- dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    birthyr_rand = dplyr::first(birthyr_rand) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

# grab the value for the first wave and forces it to all waves (birth month)
dsR <- dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    birthmo_rand = dplyr::first(birthmo_rand) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

# ---- cohort from RAND --------

# check that it matched RAND stats and it does
dsR %>% over_waves("cohort")

# grab the value for the first wave and forces it to all waves (cohort)
dsR <- dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    cohort = dplyr::first(cohort) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

# --------- education -----------
ds %>% view_temporal_pattern("degree", 2) # degree
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


# grab the value for the first wave where there is a non-missing value and forces it to all waves 
ds <- ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::arrange(hhidpn, year) %>%
  dplyr::mutate(
    degree_fixed = setdiff(unique(degree),NA)[1]
  ) %>%
  dplyr::ungroup()

# change the newly created degree_fixed variable from a character vector to a factor.
ds$degree_fixed <- factor(ds$degree_fixed, order=TRUE, 
                          levels = c("LESS THAN BACHELORS"
                            ,"BACHELORS"
                            ,"MASTERS/MBA"
                            ,"LAW, PHD, MD"
                            ,"OTHER (SPECIFY)"
                          ))
# check that values are now the same across waves
ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(degree_fixed[!is.na(degree_fixed)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

# examine single case 
ds %>% dplyr::filter(hhidpn==19536010)
dsR %>% dplyr::filter(hhidpn== "19536010")
ds %>% dplyr::filter(hhidpn== "19536010")

# ------- years of education --------------
# # Recode 98 dk and 99 refused to missing NOTE that 97 is "Other"
ds[,"edyrs"] <- plyr::mapvalues(ds[,"edyrs"], from=c(98,99), to=c(NA, NA))

ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(edyrs[!is.na(edyrs)]))) %>%
  dplyr::arrange(desc(unique)) # unique > 1 indicates change over wave

ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(edyrs[!is.na(edyrs)]))) %>%
  dplyr::count(unique>1) # unique > 1 indicates change over wave

# grab the value for the first wave where there is a non-missing value and forces it to all waves
ds <- ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::arrange(hhidpn, year) %>%
  dplyr::mutate(
    edyrs_fixed = setdiff(unique(edyrs),NA)[1]
  ) %>%
  dplyr::ungroup()

#Check
ds %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::summarize(unique = length(unique(edyrs_fixed[!is.na(edyrs_fixed)]))) %>%
  dplyr::count(unique>1) # unique > 1 indicates change over wave

# ---- RAND education values -------

dsR %>% over_waves("raedyrs")

# grab the value for the first wave and forces it to all waves (raedyrs)
dsR <- dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    raedyrs = dplyr::first(raedyrs) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

# ----- RAND degree ----------

dsR %>% over_waves("raedegrm")

# grab the value for the first wave and forces it to all waves (raedyrs)
dsR <- dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    raedegrm = dplyr::first(raedegrm) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

# ------- RAND education ---------

dsR %>% over_waves("raeduc")

# grab the value for the first wave and forces it to all waves (raedyrs)
dsR <- dsR %>%
  dplyr::group_by(hhidpn) %>%
  dplyr::mutate(
    raeduc = dplyr::first(raeduc) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

dto[["rand"]] <- dsR

# ----- loneliness --------
ds_lone <- dto$loneliness

describeBy(ds_lone, list(year=ds_lone$year))
boxplot(sum_11 ~ year, ds_lone)
boxplot.stats(ds_lone$sum_11, coef = 1.5)$out
boxplot(loneliness_1 ~ year, ds_lone)

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
check <- which(ds$closechild >20)
for (i in check){
  print(ds[i,])
}

ds %>% dplyr::filter(hhidpn==12274010)

ds <- cbind(ds, digit1=substr(ds[, "closechild"],1,1), digit=substr(ds[, "closechild"],2,3))

ds$digit <- plyr::mapvalues(ds[,"digit"], from=c("aN"), to=c(NA))
ds$digit1 <- plyr::mapvalues(ds[,"digit1"], from=c("N"), to=c(NA))
ds$digit <-as.character(ds$digit)
ds$digit1 <-as.character(ds$digit1)
ds$closechildr <- as.numeric(ifelse(ds$digit1 == ds$digit, ds$digit1, ds$closechild))

check <- which(ds$closechildr>20)
for (i in check){
  print(ds[i,])
}

# ------ closefam -------
boxplot(closefam ~ year, ds)
boxplot.stats(ds$closefam, coef = 2)$out

check <- which(ds$closefam >50)
for (i in check){
  print(ds[i,])
}

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
# ------ create a serial 7 score for 2014 as this was not available in the HRS RAND longitudinal file
ds <- dto$serial7s
dsR <- dto$rand

dsR %>% dplyr::filter(hhidpn==32181040)

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

# Need to add this serial7 total for 2014 to the rest of the serial7 totals from HRS RAND.
#create dataset with only the needed variables
ds_tojoin <- subset(ds_14, select = c(year,hhidpn,serial7r_tot))
dsR$year <- as.factor(dsR$year)
ds_join <- dplyr::full_join(dsR, ds_tojoin, by = c("hhidpn", "year"))



ds_join %>% dplyr::filter(hhidpn==207281010)

ds_join$serial7r_tot <- ifelse(ds_join$year==2014, ds_join$serial7r_tot.y,ds_join$serial7r_tot.x)

# save to dto as rand
dto[["rand"]] <- ds_join

describeBy(ds_join$serial7r_tot, list(year=ds_join$year))
boxplot(serial7r_tot  ~ year, dsR)
# ------ depression -------------
ds <- dto$depression
describeBy(ds, list(year=ds$year))
ds %>% dplyr::filter(hhidpn==32181040)

# ----- health --------
ds <- dto$health
describeBy(ds, list(year=ds$year))





# ---- save-to-disk ------------------------------------------------------------
names(dto)
lapply(dto, names)
dto %>% object.size() %>% utils:::format.object_size("auto")
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data-unshared/derived/dto3.rds", compress="xz")

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
