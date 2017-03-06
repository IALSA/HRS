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


# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------
ds_demo <- dto$demographics

ids <-unique(ds_demo$hhidpn)

table(ds_demo$year, ds_demo$male)
table(ds_demo$year, ds_demo$proxy_interview)
table(ds_demo$year, ds_demo$interview_language)
table(ds_demo$year, ds_demo$nursing_home)
table(ds_demo$year, ds_demo$religion)
table(ds_demo$year, ds_demo$number_marriages)

dto$demographics %>% histogram_discrete("male")
dto$demographics %>% histogram_continuous("age_at_visit",bin_width = 2)
boxplot(age_at_visit ~ year, ds_demo)

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
boxplot(closechild ~ year, ds)
boxplot.stats(ds$closechild, coef = 5)$out
which(ds$closechild==99)

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
ds %>% dplyr::filter(hhidpn==21431040)


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
