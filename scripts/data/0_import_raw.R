options(width=160)
rm(list=ls())
cat("\f")

## @knitr setPaths

pathDir <- getwd() # establish home directory
pathFiles <- file.path(pathDir,"data/extract/RAND/spss")
list.files(pathFiles) # inspect participating studies

## @knitr import_raw_files
filePaths <- list.files(pathFiles, full.names=T, recursive=T, pattern="sav$")
fileNames <- basename(filePaths) # save only the last component


# read SPSS files, convert to RDS, save in derived only run the first time on new computer
#for(i in 1:length(filePaths)){
#for(i in 1){
  #filePath <- filePaths[[i]]
  #fileName <- tail(strsplit(filePath, "/|.sav")[[1]], n=1)
  #oneFile <- Hmisc::spss.get(filePath, use.value.labels = TRUE)
  #saveRDS(oneFile, paste0("./data/derived/unshared/", fileName, ".rds")) # all raw data
}

#collect all RDS in a list object
pathFilesRDS <- pathFiles <- file.path(pathDir,"data/derived/unshared//")
filePathsRDS <- list.files(pathFiles, full.names=T, recursive=T, pattern="rds$")
fileNamesRDSbase <- basename(filePathsRDS) # save only the last component
fileNamesRDS <- gsub(".rds","",fileNamesRDSbase) # remove extention from the filename
lsRAND <- list() # create empty list to population

#for(i in 1:length(filePathsRDS)){
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


#### Developmental script beyond this point down ####

## @knitr names_labels
names_labels <- function(file){
  if(is.null(file)){cat("You don't have this file")}else{
    ds0 <- file
    nl <- data.frame(matrix(NA, nrow=ncol(ds0), ncol=2))
    names(nl) <- c("name","label")
      for (i in seq_along(names(ds0))){
        nl[i,"name"] <- attr(ds0[i], "names")
          if(is.null(attr(ds0[[i]], "label")) ){
          nl[i,"label"] <- NA}else{
          nl[i,"label"] <- attr(ds0[[i]], "label")  
          }
      }
    return(nl)
  }
}

## @knitr export_names_labels
explort_names_labels <- function(file){
  ds0 <- file
  nl <- data.frame(matrix(NA, nrow=ncol(ds0), ncol=2))
  names(nl) <- c("name","label")
  for (i in seq_along(names(ds0))){
    nl[i,"name"] <- attr(ds0[i], "names")
      if(is.null(attr(ds0[[i]], "label")) ){
      nl[i,"label"] <- NA}else{
      nl[i,"label"] <- attr(ds0[[i]], "label")  
      }
  }
  write.table(nl,"./data/derived/rename/test.csv", sep=",")
}
explort_names_labels(ds)
  




  
names(datas_raw) <- fileNames

class(datas_raw)
names(datas_raw)
saveRDS(datas_raw,"./data/derived/unshared/datas_raw.rds" ) # all raw data
datas_raw <- readRDS("./data/derived/unshared/datas_raw.rds") 
names(datas_raw)
filesToCombine <- c(
  "h92f1b.sav"  
 ,"h94f1a.sav"  
 ,"h96f4a.sav"  
 ,"h98f2c.sav" 
 ,"h00f1c.sav" 
 ,"h02f2c.sav"  
 ,"h04f1a.sav"  
 ,"h06f2b.sav"  
 ,"h08f2a.sav"  
 ,"h10f4a.sav" 
)

ls_raw <- datas_raw[filesToCombine] 


names(ls_raw[[1]])

