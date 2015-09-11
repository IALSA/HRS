
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

pathFile1 <- file.path(pathFolderSPSS, "h04f1a.sav")
pathFile1RDS <- file.path(pathFolder, "h04f1a.Rds")

pathFile2 <- file.path(pathFolderSPSS, "h06f2b.sav")
pathFile2RDS <- file.path(pathFolder, "h06f2b.Rds")

pathFile3 <- file.path(pathFolderSPSS, "h08f2a.sav")
pathFile3RDS <- file.path(pathFolder, "h08f2a.Rds")

pathFile4 <- file.path(pathFolderSPSS, "h10f4a.sav")
pathFile4RDS <- file.path(pathFolder, "h10f4a.Rds")

pathFile5 <- file.path(pathFolderSPSS, "h12e1a.sav")
pathFile5RDS <- file.path(pathFolder, "h12e1a.Rds")

require(foreign)
#Next three lines are run the first time on machine
 #ds0_1 <- read.spss(file = pathFile1, use.value.labels=TRUE)
 #ds0_1 <- data.frame(ds0_1)
 #saveRDS(object = ds0_1, file=pathFile1RDS, compress="xz")

#Next three lines are run the first time on machine
 #ds0_2 <- read.spss(file=pathFile2, use.value.labels=TRUE)
 #ds0_2 <- data.frame(ds0_2)
 #saveRDS(object = ds0_2, file=pathFile2RDS, compress="xz")


#Next three lines are run the first time on machine
 #ds0_3 <- read.spss(file=pathFile3, use.value.labels=TRUE)
 #ds0_3 <- data.frame(ds0_3)
 #saveRDS(object = ds0_3, file=pathFile3RDS, compress="xz")


#Next three lines are run the first time on machine
 #ds0_4 <- read.spss(file = pathFile4, use.value.labels=TRUE)
 #ds0_4 <- data.frame(ds0_4)
 #saveRDS(object = ds0_4, file=pathFile4RDS, compress="xz")

#Next three lines are run the first time on machine
 #ds0_5 <- read.spss(file=pathFile5, use.value.labels=TRUE)
 #ds0_5 <- data.frame(ds0_5)
 #saveRDS(object = ds0_5, file=pathFile5RDS, compress="xz")


require(dplyr)

# rename variables for 2004 wave subset
ds04full <- readRDS(pathFile1RDS)
ds04 <- ds04full[1:100,]
source("./Scripts/Data/rename2004.R")
ds04 <- ds04[,keepvars04]


# rename variables for 2006 wave subset
ds06full <- readRDS(pathFile2RDS)
ds06 <- ds06full[1:100,]
source("./Scripts/Data/rename2006.R")
ds06 <- ds06[,keepvars06]


# rename variables for 2008 wave subset
ds08full <- readRDS(pathFile3RDS)
ds08 <- ds08full[1:100,]
source("./Scripts/Data/rename2008.R")
ds08 <- ds08[,keepvars08]


# rename variables for 2012 wave subset
ds12full <- readRDS(pathFile5RDS)
ds12 <- ds12full[1:100,]
source("./Scripts/Data/rename2012.R")
ds12 <- ds12[,keepvars12]



