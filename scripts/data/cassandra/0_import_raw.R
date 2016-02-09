options(width=160)
rm(list=ls())
cat("\f")

## @knitr setPaths
library(data.table)

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
ds04 <- readRDS("./Data/Derived/unshared/h04f1a.rds")
ds06 <- readRDS("./Data/Derived/unshared/h06f2b.rds")
ds08 <- readRDS("./Data/Derived/unshared/h08f2a.rds")
ds10 <- readRDS("./Data/Derived/unshared/h10f4a.rds")
ds12 <- readRDS("./Data/Derived/unshared/h12e1a.rds")

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

demo04<-data.table(demographics(ds04,"J","04"))
setkey(demo04,hhidpn.04)
head(demo04)

help04<-data.table(funlimitshelp(ds04,"J",".04"))
setkey(help04,hhidpn.04)
head(help04)
tables()

disability04<-data.table(disability(ds04,"J",".04"))
setkey(disability04,hhidpn.04)
head(disability04)

psych04<-data.table(psychosocial(ds04,"J",".04"))
setkey(psych04,hhidpn.04)
head(psych04)

physhlth04 <- data.table(physicalhealth(ds04,"J",".04"))
setkey(physhlth04,hhidpn.04)
head(physhlth04)

hrs04<- demo04[help04]
head(hrs04)
hrs04<-hrs04[disability04]
hrs04<-hrs04[psych04]
hrs04<-hrs04[physhlth04]


head(hrs04)

#2006

demo06<-data.table(demographics(ds06,"K","06"))
setkey(demo06,hhidpn.06)
head(demo06)

help06<-data.table(funlimitshelp(ds06,"K",".06"))
setkey(help06,hhidpn.06)
head(help06)
tables()

disability06<-data.table(disability(ds06,"K",".06"))
setkey(disability06,hhidpn.06)
head(disability06)

psych06<-data.table(psychosocial(ds06,"K",".06"))
setkey(psych06,hhidpn.06)
head(psych06)

physhlth06 <- data.table(physicalhealth(ds06,"K",".06"))
setkey(physhlth06,hhidpn.06)
head(physhlth06)

hrs06<- demo06[help06]
hrs06<-hrs06[disability06]
hrs06<-hrs06[psych06]
hrs06<-hrs06[physhlth06]

head(hrs06)
hrs <- hrs04[hrs06]

###########2008
demo08<-data.table(demographics(ds08,"l","08"))
setkey(demo08,hhidpn.08)
head(demo08)

help08<-data.table(funlimitshelp(ds08,"l",".08"))
setkey(help08,hhidpn.08)
head(help08)
tables()

disability08<-data.table(disability(ds08,"l",".08"))
setkey(disability08,hhidpn.08)
head(disability08)

psych08<-data.table(psychosocial(ds08,"l",".08"))
setkey(psych08,hhidpn.08)
head(psych08)

physhlth08 <- data.table(physicalhealth(ds08,"l",".08"))
setkey(physhlth08,hhidpn.08)
head(physhlth08)

hrs08<- demo08[help08]
head(hrs08)
hrs08<-hrs08[disability08]
hrs08<-hrs08[psych08]
hrs08<-hrs08[physhlth08]

head(hrs08)

#2010
demo10<-data.table(demographics(ds10,"m","10"))
setkey(demo10,hhidpn.10)
head(demo04)

help10<-data.table(funlimitshelp(ds10,"m",".10"))
setkey(help10,hhidpn.10)
head(help10)
tables()

disability10<-data.table(disability(ds10,"m",".10"))
setkey(disability10,hhidpn.10)
head(disability10)

psych10<-data.table(psychosocial(ds10,"m",".10"))
setkey(psych10,hhidpn.10)
head(psych10)

physhlth10 <- data.table(physicalhealth(ds10,"m",".10"))
setkey(physhlth10,hhidpn.10)
head(physhlth10)

hrs10<- demo10[help10]
head(hrs10)
hrs10<-hrs10[disability10]
hrs10<-hrs10[psych10]
hrs10<-hrs10[physhlth10]





