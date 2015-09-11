
# remove all elements for a clean start
rm(list=ls(all=TRUE))
cat("\014")
library(foreign)
library(plyr)
library(dplyr)
# Importing raw RAND files version A.
pathDir <- getwd()
pathFolder <- file.path(pathDir,"Data/Extract/RAND_vA")
pathFolderSPSS <- file.path(pathFolder,"spss")
pathFolderRDS <- file.path(pathDir,"Data/Derived/Unshared")

pathFile1 <- file.path(pathFolderSPSS, "h04f1a.sav")
pathFile1RDS <- file.path(pathFolderRDS, "h04f1a.Rds")

pathFile2 <- file.path(pathFolderSPSS, "h06f2b.sav")
pathFile2RDS <- file.path(pathFolderRDS, "h06f2b.Rds")

pathFile3 <- file.path(pathFolderSPSS, "h08f2a.sav")
pathFile3RDS <- file.path(pathFolderRDS, "h08f2a.Rds")

pathFile4 <- file.path(pathFolderSPSS, "h10f4a.sav")
pathFile4RDS <- file.path(pathFolderRDS, "h10f4a.Rds")

pathFile5 <- file.path(pathFolderSPSS, "h12e1a.sav")
pathFile5RDS <- file.path(pathFolderRDS, "h12e1a.Rds")

# run this script only once, comment out in later use
source("./Scripts/Data/import_spss.R")

require(dplyr)

# rename variables for 2004 wave subset
ds04full <- readRDS(pathFile1RDS) # import the raw data from an Rds object
ds04 <- ds04full[1:100,] # select a few obs when developing, for speed
source("./Scripts/Data/rename2004.R") # see "rename_scheme.xlsx" for renaming key. The code in "rename2***.R" was compiled using excel
ds04 <- ds04[,keepvars04] # keep only the variables of interest


# rename variables for 2006 wave subset
ds06full <- readRDS(pathFile2RDS)# import the raw data from an Rds object
ds06 <- ds06full[1:100,]  # select a few obs when developing, for speed
source("./Scripts/Data/rename2006.R") # see "rename_scheme.xlsx" for renameing key
ds06 <- ds06[,keepvars06]# keep only the variables of interest


# rename variables for 2008 wave subset
ds08full <- readRDS(pathFile3RDS)# import the raw data from an Rds object
ds08 <- ds08full[1:100,]  # select a few obs when developing, for speed
source("./Scripts/Data/rename2008.R") # see "rename_scheme.xlsx" for renameing key
ds08 <- ds08[,keepvars08]# keep only the variables of interest


# rename variables for 2012 wave subset
ds12full <- readRDS(pathFile5RDS)# import the raw data from an Rds object
ds12 <- ds12full[1:100,]   # select a few obs when developing, for speed
source("./Scripts/Data/rename2012.R") # see "rename_scheme.xlsx" for renameing key
ds12 <- ds12[,keepvars12]# keep only the variables of interest



