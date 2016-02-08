options(width=160)
rm(list=ls())
cat("\f")

library(Hmisc)

## @knitr setPaths

pathDir <- getwd() # establish home directory
pathFiles <- file.path(pathDir,"data/extract/RAND/spss")
list.files(pathFiles) # inspect participating studies

## @knitr import_raw_files
filePaths <- list.files(pathFiles, full.names=T, recursive=T, pattern="sav$")
fileNames <- basename(filePaths) # save only the last component


# read SPSS files, convert to RDS, save in derived only run the first time on new computer
for(i in 1:length(filePaths)){
for(i in 1){
 filePath <- filePaths[[i]]
  fileName <- tail(strsplit(filePath, "/|.sav")[[1]], n=1)
  oneFile <- Hmisc::spss.get(filePath, use.value.labels = TRUE)
  saveRDS(oneFile, paste0("./data/derived/unshared/", fileName, ".rds")) # all raw data
 }}

#collect all RDS in a list object
pathFilesRDS <- pathFiles <- file.path(pathDir,"data/derived/unshared//")
filePathsRDS <- list.files(pathFiles, full.names=T, recursive=T, pattern="rds$")
fileNamesRDSbase <- basename(filePathsRDS) # save only the last component
fileNamesRDS <- gsub(".rds","",fileNamesRDSbase) # remove extention from the filename
lsRAND <- list() # create empty list to population

psychosocial <- c("h04f1a","h06f2b","h08f2a","h10f4a","h12e1a")
#### Development ####
ds04 <- readRDS("./data/derived/unshared/RAND/h04f1a.rds")
ds06 <- readRDS("./data/derived/unshared/RAND/h06f2b.rds")
ds08 <- readRDS("./data/derived/unshared/RAND/h08f2a.rds")
ds10 <- readRDS("./data/derived/unshared/RAND/h10f4a.rds")
ds12 <- readRDS("./data/derived/unshared/RAND/h12e1a.rds")

# str(ds04)
# dim(ds04)
# 
# (ds <- ds04[,c("HHIDPN","JA028")])
# str(ds)
# head(ds)

## @knitr define_lookup_function
names_labels <- function(ds0){
    nl <- data.frame(matrix(NA, nrow=ncol(ds0), ncol=2))
    names(nl) <- c("name","label")
      for (i in seq_along(names(ds0))){
        # i = 2
        nl[i,"name"] <- attr(ds0[i], "names")
          if(is.null(attr(ds0[[i]], "label")) ){
          nl[i,"label"] <- NA}else{
          nl[i,"label"] <- attr(ds0[,i], "label")  
          }
      }
    return(nl)
}


nl04 <- names_labels(ds0=ds04)
nl06 <- names_labels(ds0=ds06)
nl08 <- names_labels(ds0=ds08)
nl10 <- names_labels(ds0=ds10)
nl12 <- names_labels(ds0=ds12)


write.csv(nl04,"./data/derived/items/nl04.csv")
write.csv(nl06,"./data/derived/items/nl06.csv")
write.csv(nl08,"./data/derived/items/nl08.csv")
write.csv(nl10,"./data/derived/items/nl10.csv")
write.csv(nl12,"./data/derived/items/nl12.csv")


#### Developmental script beyond this point down ####

#1. Complete list of variables that have monolythic names ("gender")

#for(i in 1:length(filePathsRDS)) {
 # lsRAND[[i]] <- readRDS(filePathsRDS[i])
#}
#names(lsRAND) <- fileNamesRDS

#str(lsRAND)

# lsRAND is too big. We need a filter to screen what files should go it. 
# See Issue 1:  https://github.com/IALSA/HRS/issues/1

#Identify RAND file years with psychosocial variables
psychosocial <- c("h04f1a","h06f2b","h08f2a","h10f4a","h12e1a")
lsPsychosocial <- list()

for(i in 1:length(psychosocial)){
  lsPsychosocial[[i]]<- readRDS(paste0(pathFilesRDS, psychosocial[i],".rds"))
}

files <- c("h04f1a","h06f2b","h08f2a","h10f4a","h12e1a")
id <- rep("HHIDPN", length(files))
sex <- c("GENDER", "GENDER", "gender", "gender", "gender")
age <- c("JA019", "KA019", "LA019", "MA019", "NA019") # J - wave indication, A - section name





