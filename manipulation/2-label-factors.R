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

make_factor <- function(x){
  # x = "interview_language"
  # cstring <- paste0( 
  #   "a <- factor(",x,",
  #   levels = as.numeric(names(",x,"_levels",")),
  #   labels = ",x,"_levels)")
  cstring <- paste0( 
    "factor(",x,",
    levels = as.numeric(names(",x,"_levels",")),
    labels = ",x,"_levels)")
  eval(parse(text=cstring)) # evaluates the content of the command string
  # return(a)
}

# ---- demographics -----------------------------------------------------------
ds <- dto$demographics %>% 
  dplyr::mutate(
    year = year %>% as.character() %>% as.integer(),
    pc   = as.character(proxy_ratiing_cognitive)
  )
ds %>% dplyr::glimpse()

ds %>% get_freq("proxy_interview")

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
nursing_home <- c(
   "1" = "YES"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
us_born <- c(
  "1" = "YES"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
childhood_ses <- c(
  "1" = "PRETTY WELL OFF FINANCIALLY"
  ,"3" = "ABOUT AVERAGE"
  ,"5" = "POOR"
  ,"6" = "[VOL] IT VARIED"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
religion <- c(
   "1" = "PROTESTANT"
  ,"2" = "CATHOLIC"
  ,"3" = "JEWISH"
  ,"4" = "NO PREFERENCE"
  ,"7" = "OTHER (SPECIFY)"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
religious_service <- c(
   "1" = "MORE THAN ONCE A WEEK"
  ,"2" = "ONCE A WEEK"
  ,"3" = "TWO OR THREE TIMES A MONTH"
  ,"4" = "ONE OR MORE TIMES A YEAR"
  ,"5" = "NOT AT ALL"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)
english_household <- c(
   "1" =  "YES"
  ,"5" =  "NO"
  ,"8" =  "DK (Don't Know); NA (Not Ascertained)"
  ,"9" =  "RF (Refused)" 
)
number_marriages <- c(
   "0" =  "[VOL] NEVER BEEN MARRIED"
  ,"1" =  "ONCE"
  ,"2" =  "TWICE"
  ,"3" =  "THREE TIMES"
  ,"4" =  "FOUR OR MORE"
  ,"8" =  "DK (Don't Know); NA (Not Ascertained)"
  ,"9" =  "RF (Refused)"
)
married <- c(
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
male <- c(
   "1" = "MALE"
  ,"2" = "FEMALE"
)

race <- c(
   "1" = " WHITE/CAUCASIAN"
  ,"2" = " BLACK/AFRICAN AMERICAN"
  ,"7" = "OTHER (SPECIFY) Masked version includes American Indian, Alaskan Native, Asian, and Pacific Islander"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

hispanic <- c(
   "1" = "YES"
  ,"5" = "NO"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

degree <- c(
  "1" = "LESS THAN BACHELORS"
  ,"2" = "BACHELORS"
  ,"3" = "MASTERS/MBA"
  ,"6" = "LAW, PHD, MD"
  ,"7" = "OTHER (SPECIFY)"
  ,"8" = "DK (Don't Know); NA (Not Ascertained)"
  ,"9" = "RF (Refused)"
)

ds <- ds %>% 
  dplyr::mutate(
    # interview_language = make_factor("interview_language")
    pF = factor(proxy_interview,
                levels = as.numeric(names(proxy_interview_levels)) ,
                labels = proxy_interview_levels
                ),
    # pcF = factor(proxy_ratiing_cognitive,
    #              levels = as.numeric(names))
  )

 ds %>% 
  # dplyr::filter(year == 2010) %>%
  get_freq("interview_language")

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
