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
source("./scripts/functions-common.R") # used in multiple reports
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

# ---- declare-factor-levels ------------------------------

proxy_interview_levels <- c(
  "1" = "SELF"
  ,"2" = "PROXY, SPOUSE IS REPORTER"
  ,"3" = "PROXY, NON-SPOUSE IS REPORTER"
  ,"4" = "PROXY, SPOUSE IS REPORTER - NOT LIVING WITH"
)
proxy_rating_cognitive_levels <- c(
  "1" =  "NO REASON TO THINK THE RESPONDENT HAS ANY COGNITIVE LIMITATIONS"
  ,"2" =  "THE RESPONDENT MAY HAVE SOME COGNITIVE LIMITATIONS BUT COULD  PROBABLY DO THE INTERVIEW"
  ,"3" =  "THE RESPONDENT HAS COGNITIVE LIMITATIONS THAT PREVENT  HIM/HER FROM BEING INTERVIEWED"
  ,"8" =  "DK (Dont Know); NA (Not Ascertained)"
  ,"9" =  "RF (Refused)"
)
interview_language_levels <- c(
  "1" = "ENGLISH"
  ,"2" = "SPANISH"
)
nursing_home_levels <- c(
  "1" = "YES"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
us_born_levels <- c(
  "1" = "YES"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
childhood_ses_levels <- c(
  "1" = "PRETTY WELL OFF FINANCIALLY"
  ,"3" = "ABOUT AVERAGE"
  ,"5" = "POOR"
  ,"6" = "[VOL] IT VARIED"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
religion_levels <- c(
  "1" = "PROTESTANT"
  ,"2" = "CATHOLIC"
  ,"3" = "JEWISH"
  ,"4" = "NO PREFERENCE"
  ,"7" = "OTHER (SPECIFY)"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
religious_service_levels <- c(
  "1" = "MORE THAN ONCE A WEEK"
  ,"2" = "ONCE A WEEK"
  ,"3" = "TWO OR THREE TIMES A MONTH"
  ,"4" = "ONE OR MORE TIMES A YEAR"
  ,"5" = "NOT AT ALL"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
english_household_levels <- c(
  "1" =  "YES"
  ,"5" =  "NO"
  ,"8" =  "DK (Don't Know); NA (Not Ascertained)"
  ,"9" =  "RF (Refused)" 
)
number_marriages_levels <- c(
  "0" =  "[VOL] NEVER BEEN MARRIED"
  ,"1" =  "ONCE"
  ,"2" =  "TWICE"
  ,"3" =  "THREE TIMES"
  ,"4" =  "FOUR OR MORE"
  ,"8" =  "DK (Don't Know); NA (Not Ascertained)"
  ,"9" =  "RF (Refused)"
)
married_levels <- c(
  "1" =  "[VOL] MARRIED"
  ,"2" =  "[VOL] ANNULLED"
  ,"3" =  "SEPARATED"
  ,"4" =  "DIVORCED"
  ,"5" =  "WIDOWED"
  ,"6" =  "NEVER MARRIED"
  ,"7" =  "OTHER (SPECIFY)"
  ,"8" =  "DK (Don't Know); NA (Not Ascertained)"
  ,"9" =  "RF (Refused)"
)
male_levels <- c(
  "1" = "MALE"
  ,"2" = "FEMALE"
)

race_levels <- c(
  "1" = " WHITE/CAUCASIAN"
  ,"2" = " BLACK/AFRICAN AMERICAN"
  ,"7" = "OTHER (SPECIFY) Masked version includes American Indian, Alaskan Native, Asian, and Pacific Islander"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

hispanic_levels <- c(
  "1" = "YES"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

degree_levels <- c(
  "1" = "LESS THAN BACHELORS"
  ,"2" = "BACHELORS"
  ,"3" = "MASTERS/MBA"
  ,"6" = "LAW, PHD, MD"
  ,"7" = "OTHER (SPECIFY)"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

# create a list object that collects all factor definitions
declared_levels <- ls(pattern = "_levels")
ls_levels <- list()
# for(i in declared_levels){
for(i in seq_along(declared_levels) ){
  # i <- 1
  variable_name <- sub("_levels","",declared_levels[i])
  ls_levels[[variable_name]] <- get(declared_levels[i])
}
names(ls_levels)


# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS("./data-unshared/derived/dto.rds")

# ---- inspect-data -------------------------------------------------------------
names(dto)
lapply(dto,names)



# ----- utility-functions ----------------------
get_freq <- function(
  d,
  varname
){
  d %>% 
    dplyr::group_by_(varname) %>% 
    dplyr::summarize(n=n())
}

make_factor <- function(d, variable_name){
  #d <- ds
  # variable_name <- "interview_language"
  
  variable_levels <- paste0(variable_name, "_levels")
  d[,variable_name]<- as.numeric(d[,variable_name])
  d[,variable_name] <- factor(
    d[,variable_name]
    , levels = as.numeric(names(get(variable_levels))) 
    , labels = get(variable_levels)
  )
  return(d)
}
#usage:
#ds <- make_factor(ds,"interview_language")
#NOTE: for proper use of this function, the object that stores
# factor level definition must be named "VARIABLE_levels",
# where VARIABLE is the name of the column

# ---- demographics- ----------------
ds <- dto$demographics

ds %>% dplyr::glimpse()
names(ls_levels)
for(i in seq_along(ls_levels) ){
  variable_name <- names(ls_levels[i])
  ds <- make_factor(ds, variable_name)
}

ds %>% dplyr::glimpse()
ds %>% 
  dplyr::group_by_(variable_name) %>% 
  dplyr::summarize(n=n())

dto[["demographics"]] <- ds

# ----- loneliness-apply-common-levels -----------------
ds <- dto$loneliness
loneliness_response <- c(
   "1" = "OFTEN"
  ,"2" = "SOME OF THE TIME"
  ,"3" = "HARDLY EVER OR NEVER"
)

varlist<- c("loneliness_1","loneliness_2","loneliness_3","loneliness_4","loneliness_5","loneliness_6","loneliness_7",
            "loneliness_8","loneliness_9","loneliness_10","loneliness_11")
for(i in varlist){
  ds[,paste0(i,"F")]<-ordered(ds[,i],
                              levels = as.numeric(names(loneliness_response)),
                              labels = loneliness_response)
}

# ------- social-contact ---------
ds <- dto$social_contact

#Note that this the is the response labels as applied after reverse coding
social_contact_response <- c(
  "6" = "THREE OR MORE TIMES A WEEK"
  ,"5" = "ONCE OR TWICE A WEEK"
  ,"4" = "ONCE OR TWICE A MONTH"
  ,"3" = "EVERY FEW MONTHS"
  ,"2" = "ONCE OR TWICE A YEAR"
  ,"1" = "LESS THAN ONCE A YEAR OR NEVER"
)

for(i in names(ds)){
  ds[,paste0(i,"F")]<-ordered(ds[,i],
                              levels = as.numeric(names(social_contact_response)),
                              labels = social_contact_response)
}
head(ds)

# ------- life-satsifaction ----------
ds_life <- subset( dto$life_satisfaction, year!=2006)
ds_life_2006 <- subset( dto$life_satisfaction, year==2006)

life_satisfaction_2006 <- c(
  "1" = "STRONGLY DISAGREE"
  ,"2" = "SOMEWHAT DISAGREE"
  ,"3" = "SLIGHTLY DISAGREE"
  ,"4" = "SLIGHTLY AGREE"
  ,"5" = "SOMEWHAT AGREE"
  ,"6" = "STRONGLY AGREE"
)

life_satisfaction_other <- c(
   "1" = "STRONGLY DISAGREE"
  ,"2" = "SOMEWHAT DISAGREE"
  ,"3" = "SLIGHTLY DISAGREE"
  ,"4" = "NEITHER AGREE OR DISAGREE"
  ,"5" = "SLIGHTLY AGREE"
  ,"6" = "SOMEWHAT AGREE"
  ,"7" = "STRONGLY AGREE"
)
varlist<- c("lifesatisfaction_1","lifesatisfaction_2","lifesatisfaction_3","lifesatisfaction_4","lifesatisfaction_5")

for(i in varlist){
  ds_life_2006[,paste0(i,"F")]<-ordered(ds_life_2006[,i],
                              levels = as.numeric(names(life_satisfaction_2006)),
                              labels = life_satisfaction_2006)
}


for(i in varlist){
ds_life[,paste0(i,"F")]<-ordered(ds_life[,i],
                                levels = as.numeric(names(life_satisfaction_other)),
                                labels = life_satisfaction_other)  
}

# Need to merge life satisfaction years and combine them with the other data.

# -------- wellbeing -------------

ds_wellbeing <- dto$wellbeing

wellbeing_common <- c(
  "1" = "STRONGLY DISAGREE"
  ,"2" = "SOMEWHAT DISAGREE"
  ,"3" = "SLIGHTLY DISAGREE"
  ,"4" = "SLIGHTLY AGREE"
  ,"5" = "SOMEWHAT AGREE"
  ,"6" = "STRONGLY AGREE"
)

varlist <- c("wellbeing_1", "wellbeing_2","wellbeing_3","wellbeing_4","wellbeing_5","wellbeing_6","wellbeing_7")

for(i in varlist){
  ds_wellbeing[,paste0(i,"F")]<-ordered(ds_wellbeing[,i],
                                   levels = as.numeric(names(wellbeing_common)),
                                   labels = wellbeing_common)  
}


# ----- health-apply-common-levels -----------------
ds <- dto$health
health_common_response <- c(
   "1" = "YES"
  ,"3" = "DISPUTES PREVIOUS WAVE RECORD, BUT NOW HAS CONDITION"
  ,"4" = "DISPUTES PREVIOUS WAVE RECORD, DOES NOT HAVE CONDITION"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

exercise_levels <- c(
   "1" = "MORE THAN ONCE A WEEK"
  ,"2" = "ONCE A WEEK"
  ,"3" = "ONE TO THREE TIMES A MONTH"
  ,"4" = "HARDLY EVER OR NEVER"
  ,"7" = "(VOL) EVERY DAY"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
varlist<- c("hypertension","diabetes","cancer","lungdisease","heart","psychiatric","arthritis")
for(i in varlist){
  ds[,paste0(i,"F")]<-ordered(ds[,i],
                                levels = as.numeric(names(health_common_response)),
                                labels = health_common_response)
}

exercise_varlist<- c("vigorousactivity","moderateactivity","mildactivity")
for(i in exercise_varlist){
  ds[,paste0(i,"F")]<-ordered(ds[,i],
                              levels = as.numeric(names(exercise_levels)),
                              labels = exercise_levels)
}

# ------- health-apply-individual-levels ----------------

stroke_levels <- c(
  "1" = "YES"
  ,"2" = "[VOL] POSSIBLE STROKE OR TIA (TRANSIENT ISCHEMIC ATTACK)"
  ,"3" = "DISPUTES PREVIOUS WAVE RECORD, BUT NOW HAS CONDITION"
  ,"4" = "DISPUTES PREVIOUS WAVE RECORD, DOES NOT HAVE CONDITION"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
memorydisease_levels <- c(
  "1" = "YES"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

Alzheimers_levels <- c(
  "1" = "YES"
  ,"3" = "DISPUTES PREVIOUS WAVE RECORD, BUT NOW HAS CONDITION"
  ,"4" = "DISPUTES PREVIOUS WAVE RECORD, DOES NOT HAVE CONDITION"
  ,"5" = "NO"
  ,"7" = "[VOL] NOT ALZHEIMER'S DISEASE"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

dementia_levels <- c(
   "1" = "YES"
  ,"3" = "DISPUTES PREVIOUS WAVE RECORD, BUT NOW HAS CONDITION"
  ,"4" = "DISPUTES PREVIOUS WAVE RECORD, DOES NOT HAVE CONDITION"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

# create a list object that collects individual health variable definitions
individual_health_levels <- c("stroke_levels", "memorydisease_levels", "Alzheimers_levels", "dementia_levels")
healthls_levels <- list()
# for(i in declared_levels){
for(i in seq_along(individual_health_levels) ){
   #i <- 1
  variable_name <- sub("_levels","",individual_health_levels[i])
  healthls_levels[[variable_name]] <- get(individual_health_levels[i])
}

# Apply the make_factor function to health variables that have their own labels.
for(i in seq_along(healthls_levels) ){
  # i <- 1
  variable_name <- names(healthls_levels[i])
  ds <- make_factor(ds, variable_name)
}
ds %>% dplyr::glimpse()

dto[["health"]] <- ds

dto %>% object.size() %>% utils:::format.object_size("auto")
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data-unshared/derived/dto_labels.rds", compress="xz")
# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------
ds_demo <- dto$demographics

ids <-unique(ds_demo$hhidpn)


dto$demographics %>% histogram_discrete("male")
dto$demographics %>% histogram_continuous("age_at_visit",bin_width = 2)
dto$demographics %>% histogram_discrete("ses", levels_to_exclude = NA)





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
