rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
#####################################
## @knitr load_sources



#####################################
## @knitr load_packages

library(knitr)
library(scales) #For formating values in graphs
library(RColorBrewer)
# library(reshape2) #For converting wide to long
library(grid) #For graphing
library(ggplot2) #For graphing
library(testit, quietly=TRUE) #For asserts
library(dplyr)


## @knitr load_data
ds <- readRDS("./data/derived/unshared/RandHRS_AK.rds")
ds <- as.data.frame(ds)
(available_variables <- names(ds))

## @knitr basic_view_1
length(allvars) # number of variables present

## @knitr basic_view_2
head(ds[1:10], 10)

head(ds[11:20], 10)

head(ds[21:30], 10)

head(ds[31:40], 10)

head(ds[41:54], 10)


## @knitr run

pathFilesToBuild <- base::file.path("./reports/basic_eda_sean/HRS-EDA.Rmd")
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the report
for( pathFile in pathFilesToBuild ) {
  #   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
  rmarkdown::render(input = pathFile, output_format=c("html_document"), clean=TRUE)
}

