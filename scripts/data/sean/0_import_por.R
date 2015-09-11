options(width=160)
rm(list=ls())
cat("\f")

## @knitr setPaths

pathDir <- getwd() # establish home directory
pathFiles <- file.path(pathDir,"data/extract/sean")
list.files(pathFiles) # inspect participating studies

## @knitr import_raw_files


filePaths <- list.files(pathFiles, full.names=T, recursive=T, pattern="por$")
fileNames <- basename(filePaths) # save only the last component

# extract from SPSS file and place into a list object datas_raw
# datas_raw <- list() 
# for(i in 2:length(filePaths)){
# for(i in 1){
  filePath <- filePaths[[1]]
  fileName <- tail(strsplit(filePath, "/|.por")[[1]], n=1)
# oneFile <- Hmisc::spss.get(filePath, use.value.labels = TRUE)
# oneFile <- foreign::read.spss(filePath, use.value.labels = FALSE)
  
# http://stackoverflow.com/questions/3136293/read-spss-file-into-r
  require(memisc)
  oneFile <- memisc::as.data.set(spss.portable.file(filePath))
  

  saveRDS(oneFile, paste0("./data/derived/unshared/", fileName, ".rds")) # all raw data
# }


ds <- readRDS("./data/derived/unshared/RandHRS_AK.rds")
dim(ds)
names(ds)
str(ds)


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

