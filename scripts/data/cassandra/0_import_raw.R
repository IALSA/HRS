options(width=160)
rm(list=ls())
cat("\f")

library(dplyr)
## @knitr setPaths
library(data.table)
library(Hmisc)

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
ds04 <- readRDS("./data/derived/unshared/RAND/h04f1a.rds")
ds06 <- readRDS("./data/derived/unshared/RAND/h06f2b.rds")
ds08 <- readRDS("./data/derived/unshared/RAND/h08f2a.rds")
ds10 <- readRDS("./data/derived/unshared/RAND/h10f4a.rds")
ds12 <- readRDS("./data/derived/unshared/RAND/h12e1a.rds")
ds14 <- readRDS("./data/derived/unshared/H14LB_R.rds")

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
#sources of functions used
source(paste0(pathDir,"/scripts/data/cassandra/selectionfunctions.R"))
source(paste0(pathDir,"/scripts/data/cassandra/lb_scales.R"))
source(paste0(pathDir,"/scripts/data/cassandra/rename2004.R"))


source(paste0(pathDir,"/scripts/data/cassandra/rename2010_2012.R"))

ds04 <- preparing_variable_labels(ds04, "J", "04")
ds04 <- rename2004(ds04)
ds04 <- loneliness_three_items_recode(ds04)
ds04 <- lifesatisfaction_summaryscores(ds04)
ds04 <- social_support_network_recode2004(ds04)
ds04 <- welling_scale_summarize2004(ds04)

psychosocial_04 <- subset(ds04, select=c('hhidpn','loneliness_1','loneliness_2','loneliness_3',"loneliness_4","loneliness_total",'loneliness_mean',                        
                  'lifesatisfaction_mean','snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri',
                  'close_relations','socnetwork',"mtchild", "spkchild", "wrtchild","mtfam", "spkfam", "wrtfam",
                  "mtfriend", "spkfriend", "wrtfriend", 'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
                  'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri',
                  'wellbeing_total_two','wellbeing_mean_two'))
psychosocial_04<-add_year_to_variable(data = psychosocial_04, year_label = "_04")

colnames(psychosocial_04)

summary(psychosocial_04$loneliness_mean_04)

#############
#2006
source(paste0(pathDir,"/scripts/data/cassandra/rename2006.R"))
ds06 <- preparing_variable_labels(ds06,"K", "06")
ds06 <- rename2006

ds10 <- preparing_variable_labels(ds10, "m", "10")
ds10 <- rename2010_2012(ds10)
demo04<-data.table(demographics(ds04R,"04"))

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
# head(physhlth04)
(nl <- names_labels(ds0=as.data.frame(physhlth04))) 

hrs04<- merge(demo04, help04, by.demo04 = "hhidpn.04", by.help04 = "hhidpn.04")
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
(nl <- names_labels(ds0=as.data.frame(physhlth06)))

hrs06<- merge(demo06, help06, by.demo06 = "hhidpn.06", by.help06 = "hhidpn.06")
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

psych08 <- data.table(psychosocial(ds08,"l",".08"))
setkey(psych08,hhidpn.08)
head(psych08)

physhlth08 <- data.table(physicalhealth(ds08,"l",".08"))
data.table::setkey(physhlth08,hhidpn.08)
(nl <- names_labels(ds0=as.data.frame(physhlth08)))

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
data.table::setkey(physhlth10,hhidpn.10)
(nl <- names_labels(ds0=as.data.frame(physhlth10)))

hrs10<- demo10[help10]
head(hrs10)
hrs10<-hrs10[disability10]
hrs10<-hrs10[psych10]
hrs10<-hrs10[physhlth10]

#2012
demo12<-data.table(demographics(ds12,"n","12"))
setkey(demo12,hhidpn.12)
head(demo12)

help12<-data.table(funlimitshelp(ds12,"n",".12"))
setkey(help12,hhidpn.12)
head(help12)
tables()

disability12<-data.table(disability(ds12,"n",".12"))
setkey(disability12,hhidpn.12)
head(disability12)

psych12<-data.table(psychosocial(ds12,"n",".12"))
setkey(psych12,id)
head(psych12)

physhlth12 <- data.table(physicalhealth(ds12,"n",".12"))
data.table::setkey(physhlth12,hhidpn.12)
(nl <- names_labels(ds0=as.data.frame(physhlth12)))

hrs12<- demo12[help12]
head(hrs12)
hrs12<-hrs12[disability12]
hrs12<-hrs12[psych12]
hrs12<-hrs12[physhlth12]

#Data from the 2014 wave has to be handled a bit differently because
#it is not a RAND file therefore all sections are separate and HHID and PN are not combined.

#Create ds files for 2014 data 
pathDir <- getwd() # establish home directory
pathFiles <- file.path(pathDir,"Data/Extract/RAND/csv14")
list.files(pathFiles) # inspect participating studies

## @knitr import_raw_files
filePaths <- list.files(pathFiles, full.names=T, recursive=T, pattern="csv$")
fileNames <- basename(filePaths) # save only the last component
list.files(pathFiles) # inspect participating studies

# read csv files, convert to RDS, save in derived only run the first time on new computer
for(i in 1:length(filePaths)){
filePath <- filePaths[[i]]
fileName <- tail(strsplit(filePath, "/|.csv")[[1]], n=1)
oneFile <- read.csv(filePath)
saveRDS(oneFile, paste0("./data/derived/unshared/", fileName, ".rds")) # all raw data
}

ds14a <- readRDS("./data/derived/unshared/H14A_R.rds")
ds14b <- readRDS("./data/derived/unshared/H14B_R.rds")
ds14c <- readRDS("./data/derived/unshared/H14C_R.rds")
ds14d <- readRDS("./data/derived/unshared/H14D_R.rds")
ds14g <- readRDS("./data/derived/unshared/H14G_R.rds")
ds14lb <- readRDS("./data/derived/unshared/H14LB_R.rds")
ds14RC <- readRDS("./data/derived/unshared/H14RC_R.rds")


ds14a$hhidpn <- paste0(ds14a$HHID,"0",ds14a$PN)
ds14b$hhidpn <- paste0(ds14b$HHID,"0",ds14b$PN)
ds14c$hhidpn <- paste0(ds14c$HHID,"0",ds14c$PN)
ds14d$hhidpn <- paste0(ds14d$HHID,"0",ds14d$PN)
ds14g$hhidpn <- paste0(ds14g$HHID,"0",ds14g$PN)
ds14lb$hhidpn <- paste0(ds14lb$HHID,"0",ds14lb$PN)

psych14<-data.table(psychosocial(ds14lb,"O",".14"))
setkey(psych14,hhidpn.14)
head(psych14)

setnames(hrs04, "hhidpn.04","hhidpn")
setnames(hrs06, "hhidpn.06", "hhidpn")
setnames(hrs08, "hhidpn.08", "hhidpn")
setnames(hrs10, "hhidpn.10", "hhidpn")
setnames(hrs12, "hhidpn.12", "hhidpn")

hrs <- merge(hrs04, hrs06, by = "hhidpn")
hrs <- merge(hrs, hrs08, by = "hhidpn")
hrs <- merge(hrs, hrs10, by = "hhidpn")
hrs <- merge(hrs, hrs12, by = "hhidpn")

saveRDS(hrs, paste0("./data/derived/unshared/", "hrs", ".rds"))
write.csv(hrs,"./data/derived/unshared/hrs.csv")
