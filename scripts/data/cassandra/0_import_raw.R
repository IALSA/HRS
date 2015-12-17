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
# }

#collect all RDS in a list object
pathFilesRDS <- pathFiles <- file.path(pathDir,"data/derived/unshared//")
filePathsRDS <- list.files(pathFiles, full.names=T, recursive=T, pattern="rds$")
fileNamesRDSbase <- basename(filePathsRDS) # save only the last component
fileNamesRDS <- gsub(".rds","",fileNamesRDSbase) # remove extention from the filename
lsRAND <- list() # create empty list to population

psychosocial <- c("h04f1a","h06f2b","h08f2a","h10f4a","h12e1a")
#### Development ####
ds04 <- readRDS("./Data/Derived/unshared/RAND/h04f1a.rds")
ds06 <- readRDS("./Data/Derived/unshared/RAND/h06f2b.rds")
ds08 <- readRDS("./Data/Derived/unshared/RAND/h08f2a.rds")
ds10 <- readRDS("./Data/Derived/unshared/RAND/h10f4a.rds")
ds12 <- readRDS("./Data/Derived/unshared/RAND/h12e1a.rds")

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

source(paste0(pathDir,"/scripts/data/cassandra/selectionfunctions.R"))

demo04<-demographics(ds04,"J",".04")
help04<-funlimitshelp(ds04,"J",".04")
disability04<-disability(ds04,"J",".04")
psych04<-psychosocial(ds04,"J",".04")
physhlth04<-physicalhealth(ds04,"J",".04")

hrs04<-Reduce(function(x, y) merge(x, y, all=TRUE), list(demo04,help04,disability04,psych04,physhlth04))


