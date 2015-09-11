
source("./scripts/0_collect_studies.R")
# results in individual .csv for each study with raw model results

source("./scripts/0c_combine_model_outputs.R")
# combines individual .csvs into one dataset containing results of all models

source("./reports/rename_collapse/Track_renaming.R ")
# corrects typos, renames, reclassifies filename elements

source("./reports/extend/standardize_ISR.R")
# converts covariances into correlations and computes confidence intervals



## list all the reports
pathReports <- "./reports"
(rmd_list <- list.files(pathReports, full.names=T, recursive=T, pattern="Rmd$"))



## Rename and Collapse
(pathFilesToBuild <- base::file.path("./reports/rename_collapse/Track_renaming.Rmd"))
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the report
for( pathFile in pathFilesToBuild ) {
    rmarkdown::render(input = pathFile, output_format=c("html_document"), clean=TRUE)
}
# takes in ds0, produces ds1

## Extend, Standardize, Adjust
(pathFilesToBuild <- base::file.path("./reports/extend/standardize_ISR.Rmd"))
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the report
for( pathFile in pathFilesToBuild ) {
    rmarkdown::render(input = pathFile, output_format=c("html_document"), clean=TRUE)
}


# renames model elements and collapses categories


## Basic
# Counts - basic model counts
# Essentials - basic model parameters
pathReports <- "./reports/basic"
(pathFilesToBuild <- list.files(pathReports, full.names=T, recursive=T, pattern="Rmd$"))
for( pathFile in pathFilesToBuild ) {
    rmarkdown::render(input = pathFile, output_format=c("html_document"), clean=TRUE)
}

## Individual quality check
# Each study gets an individualized report narrating its progress
pathReports <- "./reports/individual"
(pathFilesToBuild <- list.files(pathReports, full.names=T, recursive=T, pattern="Rmd$"))
for( pathFile in pathFilesToBuild ) {
    rmarkdown::render(input = pathFile, output_format=c("html_document"), clean=TRUE)
}


## Model Space
# Tile graphs and domain map.
pathReports <- "./reports/model_space"
(pathFilesToBuild <- list.files(pathReports, full.names=T, recursive=T, pattern="Rmd$"))
for( pathFile in pathFilesToBuild ) {
    rmarkdown::render(input = pathFile, output_format=c("html_document"), clean=TRUE)
}





#' Build the reports
# for( pathRmd in pathsReports ) {
#   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
#   pathHtml <- base::gsub(pattern=".Rmd$", replacement=".html", x=pathRmd)
#   knitr::knit(input=pathRmd, output=pathMd)
#   markdown::markdownToHTML(file=pathMd, output=pathHtml)
# }
