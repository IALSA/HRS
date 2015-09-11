rm(list=ls(all=TRUE))
########## Production of reports from .Rmd files ###

pathReport1 <- base::file.path("./reports/counts.Rmd")
pathReport2 <- base::file.path("./reports/essentials.Rmd")
pathReport3 <- base::file.path("./reports/individual/eas.Rmd")
pathReport4 <- base::file.path("./reports/individual/habc.Rmd")
pathReport5 <- base::file.path("./reports/individual/ilse.Rmd")
pathReport6 <- base::file.path("./reports/individual/nas.Rmd")
pathReport7 <- base::file.path("./reports/individual/nuage.Rmd")
pathReport8 <- base::file.path("./reports/individual/obas.Rmd")
pathReport9 <- base::file.path("./reports/individual/octo.Rmd")
pathReport10 <- base::file.path("./reports/individual/radc.Rmd")
pathReport11 <- base::file.path("./reports/individual/satsa.Rmd")

#  Define groups of reports
allReports<- c(pathReport1,pathReport2, pathReport3, pathReport4, pathReport5, pathReport6, pathReport7, pathReport8, pathReport9, pathReport10, pathReport11)
# Place report paths HERE ###########
pathFilesToBuild <- c(allReports) ##########


testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  #   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      #                        "pdf_document"
                      #                       ,"md_document"
                      "html_document"
                    ),
                    clean=TRUE)
}

# To generate the static website from the htmls that have been printed in the code above
# 1) Select the "gh-pages" branch of your project's repository in GitHub client
# 2) Open command line terminal and change directory to the root folder of your repository that you've cloned onto your hard drive using GitHub client (type "cmd" in the address line of the File Explorer opened on root folder of your repository's clone)
# 3) type "bundle install" to install Bundler if you're creating the website for the first time
# 4) type "bundle exec jekyll serve" to build site and serve to localhost:4000
