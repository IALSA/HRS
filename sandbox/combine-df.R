# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R") # fonts, colors, themes 
source("./scripts/general-graphs.R") 
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
# each element this list is another list:
names(dto)

# ---- utility-functions ---------------------------------------------------------
# merge multiple datasets that are stored as elements of a list
merge_mulitple_files <- function(list, by_columns){
  Reduce(function( d_1, d_2 ) merge(d_1, d_2, by=by_columns), list)
}

# ---- inspect-data -------------------------------------------------------------
dto$demographics %>% dplyr::glimpse()

# ---- tweak-data --------------------------------------------------------------
dto_new <- list()

dto_new[["demographics"]] <- dto$demographics %>% 
  dplyr::select(year, hhidpn, birthyr, interview_yr,male, race )

dto_new[["loneliness"]] <- dto$loneliness %>% 
  dplyr::select(year,hhidpn,score_loneliness_3, score_loneliness_11  )

dto_new[["life_satisfaction"]] <- dto$life_satisfaction %>% 
  dplyr::select(year, hhidpn, sum, mean) %>% 
  dplyr::rename(
     life_sat_sum = sum
    ,life_sat_mean = mean
  )

View(dto_new$demographics)
View(dto_new$loneliness)
View(dto_new$life_satisfaction)



ds <- merge_mulitple_files(dto_new, by_columns = c("year","hhidpn"))


# ds <- dto_new %>% 
#   dplyr::bind_rows()


# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------

