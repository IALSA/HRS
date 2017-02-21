##################




options(width=160)
rm(list=ls())
cat("\f")
library(data.table)
library(plyr)

pathDir <- getwd() # establish home directory
pathFiles <- file.path(pathDir,"Data/Derived/unshared/RAND//")
ds <- readRDS(paste0(pathFiles,"h06f2b.RDS"))
year_letterid <- "K"

#create a for loop that takes the first letter off 
varnames <- colnames(ds)

for (i in 1:length(varnames)){
  # for (i in seq_along(varnames)){  
  if(substring(varnames[i],1,1)==year_letterid){
    varnames[i] <- substring(varnames[i],2)
  } else {
    varnames[i] <- varnames[i]
  }
}

#changes all the variable names to lower case
names(ds) <- tolower(varnames)
varnames2 <- names(ds)

#create a list of variables to include
id <- c("hhidpn")
(condition <- substring(varnames2,1,2) == "lb") #logical columns with "lb" in position 2,3
(sectionvars <- varnames2[which(condition)]) # names
section <- c(id,sectionvars) # selection
ds1 <- ds[section] 


ds1 <- plyr::rename(x=ds1, replace = c(
  "lb001a" = "newspaper_read",
  "lb001b" = "hobby",
  "lb001c" = "vacation_US",
  "lb001d" = "vacation_abroad",
  "lb001e" = "daytrip",
  "lb001f" = "use_internet",
  "lb001g" = "own_cellphone",
  "lb001h" = "none_of_above",
  "lb002" = "group_participation"
))
  


  
  