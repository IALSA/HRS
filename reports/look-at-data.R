# knitr::stitch_rmd(script="./manipulation/car-ellis.R", output="./manipulation/stitched-output/car-ellis.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # cleans console

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/functions-common.R") # used in multiple reports
source("./scripts/graphs/graphs-presets.R") # fonts, colors, themes 
source("./scripts/graphs/graphs-general.R")
source("./scripts/graphs/graphs-specific.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")
# requireNamespace("magrittr") # pipes

# ---- declare-globals ---------------------------------------------------------
# path_input  <- "./data-unshared/derived/ds0.rds"
# figure_path <- 'manipulation/stitched-output/te/'

# ---- load-data ---------------------------------------------------------------
dto <- readRDS("./data-unshared/derived/dto.rds")
names(dto)
# 1st element - unit(person) level data
names(dto[["unitData"]])
# 2nd element - meta data, info about variables
names(dto[["metaData"]])

ds <- dto$unitData

# ---- inspect-data ---------------------------------------------------------
dto$unitData %>% names_labels() %>% head()

# ----- select_subset ------------------------------------
# select variables you will need for modeling
selected_items <- c( 
   "id" # personal identifier
  # ,"htm" # Height(meters)
  # ,"msex" # Gender
  # ,"race" # Participant's race
  # ,"educ" # Years of education
  
  # ,"dementia" # Dementia diagnosis
  
  
  ,"birth_date" # perturbed data of birth
  ,"birth_year" # the year of birth
  ,"date_at_bl" # date at baseline 
  ,"age_at_bl" # age at baseline 
# time-invariant above
  ,"fu_year" # Follow-up year ------------------------------------------------
# time-variant below
  ,"date_at_visit" # perturbed date of visit
  ,"age_at_visit" #Age at cycle - fractional  
  
  # ,"mmse" # Mini Mental State Exam
  , "cogn_global" # Global cognitive score
  # ,"cts_bname" # Boston naming - 2014
  # ,"catfluency" # Category fluency - 2014
  # ,"cts_nccrtd" #  Number comparison - 2014
  # 
  # ,"fev" # forced expiratory volume   
  # ,"gait_speed" # Gait Speed - MAP
  # ,"gripavg" # Extremity strength
)

d <- ds[ , selected_items]
head(d)
table(d$fu_year)

library(ggplot2)
d %>% 
  ggplot(aes_string(x="date_at_visit",y="cogn_global")) +
  geom_line(
    aes(
       group=id
      ,color=birth_year
    )
  )
  


# ----- ---------------
new_basic_line <- function(
  d_observed,
  variable_name,
  time_metric, 
  color_name="black",
  line_alpha=1,
  line_size =.5, 
  smoothed = FALSE,
  main_title     = variable_name,
  x_title        = paste0("Time metric: ", time_metric),
  y_title        = variable_name,
  rounded_digits = 0L
) {
  
  d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
  d_observed <- d_observed[!base::is.na(d_observed[, variable_name]), ]
  
  g <- ggplot(d_observed, aes_string(x=time_metric, y = variable_name)) 
  if(!smoothed){
    g <- g + geom_line(aes_string(group="id"), size=line_size, color=scales::alpha(color_name,line_alpha), na.rm=T)   
  } else{
    g <- g + geom_smooth(aes_string(group="id"),size=line_size,  method="lm",color=scales::alpha(color_name,line_alpha), na.rm=T, se=F )
    g <- g + geom_smooth(method="loess", color="blue", size=1, fill="gray80", alpha=.3, na.rm=T)
    
  }  
  
  g <- g + 
    # scale_x_continuous(labels=scales::comma_format()) +
    scale_y_continuous(labels=scales::comma_format()) +
    # labs(title=main_title, x=x_title, y=y_title) +
    theme_light() +
    theme(axis.ticks.length = grid::unit(0, "cm"))
  return( g )
}

new_basic_line(d, "cogn_global","fu_year")
# g <- basic_line(d, "cogn_global", "fu_year", "salmon", .9, .1, F)
# g

# ---- export_data -------------------------------------
# At this point we would like to export the data in .dat format
# to be fed to Mplus for any subsequent modeling
write.csv(d,"./sandbox/report-name/data/unshared/long_dataset.csv", row.names=F)
write.table(d,"./sandbox/report-name/data/unshared/long_dataset.dat", row.names=F, col.names=F)
write(names(d), "./sandbox/syntax-creator/data/unshared/long_dataset_varnames.txt", sep=" ")


str(ds$agreeableness)
sum(ds$conscientiousness)
