options(width=160)
rm(list=ls())
cat("\f")

library(dplyr)
## @knitr setPaths
library(data.table)
library(Hmisc)

pathDir <- getwd() # establish home directory
pathFiles <- file.path(pathDir,"Data/Extract/RAND")
list.files(pathFiles) # inspect participating studies

## @knitr import_raw_files
filePaths <- list.files(pathFiles, full.names=T, recursive=T, pattern="sav$")
fileNames <- basename(filePaths) # save only the last component


# read SPSS files, convert to RDS, save in derived only run the first time on new computer
#for(i in 1:length(filePaths)){
#for(i in 1){
# filePath <- filePaths[[i]]
#  fileName <- tail(strsplit(filePath, "/|.sav")[[1]], n=1)
 # oneFile <- Hmisc::spss.get(filePath, use.value.labels = TRUE)
 # saveRDS(oneFile, paste0("./Data/Derived/unshared/", fileName, ".rds")) # all raw data
#}

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
ds14 <- readRDS("./Data/Derived/unshared/h14e1a.rds")

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
nl14 <- names_labels(ds0=ds14)

write.csv(nl04,"./data/derived/items/nl04.csv")
write.csv(nl06,"./data/derived/items/nl06.csv")
write.csv(nl08,"./data/derived/items/nl08.csv")
write.csv(nl10,"./data/derived/items/nl10.csv")
write.csv(nl12,"./data/derived/items/nl12.csv")
write.csv(nl14,"./data/derived/items/nl14.csv")

#### Developmental script beyond this point down ####
#sources of functions used
source(paste0(pathDir,"/scripts/data/cassandra/selectionfunctions.R"))
source(paste0(pathDir,"/scripts/data/cassandra/lb_scales.R"))
source(paste0(pathDir,"/scripts/data/cassandra/rename2004.R"))
source(paste0(pathDir,"/scripts/data/cassandra/demographics.R"))
source(paste0(pathDir,"/scripts/data/cassandra/physical_health.R"))
source(paste0(pathDir,"/scripts/data/cassandra/rename2010_2012.R"))
source(paste0(pathDir,"/scripts/data/cassandra/cognition.R"))

ds04 <- preparing_variable_labels(ds04, "J", "04")
ds04 <- rename2004(ds04)
ds04 <- loneliness_three_items_recode(ds04)
ds04 <- lifesatisfaction_summaryscores(ds04)
ds04 <- social_support_network_recode2004(ds04)
ds04 <- welling_scale_summarize2004(ds04)
ds04 <- physhealthrename2004_2008(ds04)
ds04 <- MentalStatus(ds04)
ds04 <- Vocabulary(ds04)

ds04$serial1[ds04$serial71==93] <- 1

psychosocial_04 <- subset(ds04, select=c('hhidpn','loneliness_1','loneliness_2','loneliness_3',"loneliness_4","loneliness_total",'loneliness_mean',                        
                  'lifesatisfaction_mean','snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri',
                  'close_relations','socnetwork',"mtchild", "spkchild", "wrtchild","mtfam", "spkfam", "wrtfam",
                  "mtfriend", "spkfriend", "wrtfriend", 'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
                  'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri',
                  'wellbeing_total_two','wellbeing_mean_two',"memoryproblem","Activity_vigorous", "Activity_moderate", "Activity_mild"))

demo04 <- basicdemographics(ds04)

ds2004<-merge(demo04, psychosocial_04, by = "hhidpn")
ds2004<-add_year_to_variable(data = ds2004, year_label = "_04")

#############
#2006
source(paste0(pathDir,"/scripts/data/cassandra/rename2006.R"))
ds06 <- preparing_variable_labels(ds06,"K", "06")
ds06 <- rename2006(ds06)
ds06 <- loneliness_three_items_recode(ds06)
ds06 <- lifesatisfaction_summaryscores(ds06)
ds06 <- social_support_network_recode2004(ds06)
ds06 <- welling_scale_summarize(ds06)
ds06 <- physhealthrename2004_2008(ds06)
demo06 <- basicdemographics(ds06)
psychosocial_06 <- subset(ds06, select=c('hhidpn','loneliness_1','loneliness_2','loneliness_3',"loneliness_total",'loneliness_mean',"lifesatisfaction_mean",
                                         'snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri','close_relations','socnetwork',"mtchild", "spkchild", "wrtchild","mtfam", "spkfam", "wrtfam",
                                         "mtfriend", "spkfriend", "wrtfriend", 'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
                                         'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri','wellbeing_total','wellbeing_mean',"memoryproblem","Activity_vigorous", "Activity_moderate", "Activity_mild"))

ds2006<-merge(demo06, psychosocial_06, by = "hhidpn")

ds2006<-add_year_to_variable(data = ds2006, year_label = "_06")


##################################
source(paste0(pathDir,"/scripts/data/cassandra/rename2008.R"))

ds08 <- preparing_variable_labels(ds08,"l", "08")
colnames(ds08)
ds08 <- rename2008(ds08)
ds08<- loneliness_three_items_recode(ds08)
ds08 <- lifesatisfaction_summaryscores(ds08)
ds08 <- social_support_network_recode(ds08)
ds08 <- welling_scale_summarize(ds08)
ds08 <- physhealthrename2004_2008(ds08)
demo08 <- basicdemographics(ds08)
psychosocial_08 <- subset(ds08, select=c('hhidpn','loneliness_1','loneliness_2','loneliness_3',"loneliness_total",'loneliness_mean',"lifesatisfaction_mean",
                                         'snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri','close_relations','socnetwork',"mtchild", "spkchild", "wrtchild","mtfam", "spkfam", "wrtfam",
                                         "mtfriend", "spkfriend", "wrtfriend", 'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
                                         'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri','wellbeing_total','wellbeing_mean',"memoryproblem","Activity_vigorous", "Activity_moderate", "Activity_mild"))

ds2008<-merge(demo08, psychosocial_08, by.demo08 = "hhidpn")

ds2008 <- add_year_to_variable(ds2008, "_08")

#####################

ds10 <- preparing_variable_labels(ds10 ,"m", "10")
colnames(ds10)
ds10 <- rename2010_2012(ds10)
ds10<- loneliness_three_items_recode(ds10)
ds10 <- lifesatisfaction_summaryscores(ds10)
ds10 <- social_support_network_recode(ds10)
ds10 <- welling_scale_summarize(ds10)
ds10 <- physhealthrename2010_2014(ds10)
demo10 <- basicdemographics(ds10)
psychosocial_10 <- subset(ds10, select=c('hhidpn','loneliness_1','loneliness_2','loneliness_3',"loneliness_total",'loneliness_mean',"lifesatisfaction_mean",
                                         'snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri','close_relations','socnetwork',"mtchild", "spkchild", "wrtchild","mtfam", "spkfam", "wrtfam",
                                         "mtfriend", "spkfriend", "wrtfriend", 'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
                                         'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri','wellbeing_total','wellbeing_mean',"AD","dementia","Activity_vigorous", "Activity_moderate", "Activity_mild"))

ds2010<-merge(demo10, psychosocial_10, by = "hhidpn")

ds2010 <- add_year_to_variable(ds2010, "_10")

#####2012###########
ds12 <- preparing_variable_labels(ds12 ,"n", "12")
colnames(ds12)
ds12 <- rename2010_2012(ds12)
ds12<- loneliness_three_items_recode(ds12)
ds12 <- lifesatisfaction_summaryscores(ds12)
ds12 <- social_support_network_recode(ds12)
ds12 <- welling_scale_summarize(ds12)
ds12 <- physhealthrename2010_2014(ds12)
demo12 <- basicdemographics(ds12)
colnames(ds12)
psychosocial_12 <- subset(ds12, select=c('hhidpn','loneliness_1','loneliness_2','loneliness_3',"loneliness_total",'loneliness_mean',"lifesatisfaction_mean",
                                         'snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri','close_relations','socnetwork',"mtchild", "spkchild", "wrtchild","mtfam", "spkfam", "wrtfam",
                                         "mtfriend", "spkfriend", "wrtfriend", 'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
                                         'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri','wellbeing_total','wellbeing_mean',"AD","dementia","Activity_vigorous", "Activity_moderate", "Activity_mild"))

ds2012<-merge(demo12, psychosocial_12, by = "hhidpn")

ds2012 <- add_year_to_variable(ds2012, "_12")

##########2014##################
source(paste0(pathDir,"/scripts/data/cassandra/rename2014.R"))
dsLB14 <- readRDS("./data/derived/unshared/H14LB_R.rds")
dsC14 <- readRDS("./data/derived/unshared/H14C_R.rds")
dsB14 <- readRDS("./data/derived/unshared/H14B_R.rds")
dsA14 <- readRDS("./data/derived/unshared/H14A_R.rds")

dsLB14$hhidpn <- paste0(dsLB14$HHID,0,dsLB14$PN)
dsB14$hhidpn <- paste0(dsB14$HHID, 0, dsB14$PN)
dsC14$hhidpn <- paste0(dsC14$HHID, 0, dsC14$PN)
dsA14$hhidpn <- paste0(dsA14$HHID, 0, dsA14$PN)
dsA14$phhidpn <- ifelse(is.na(dsA14$OPN_SP)==FALSE,paste0(dsA14$HHID, 0, dsA14$OPN_SP),NA)

demo14 <- merge(dsA14, dsB14, by = "hhidpn")

ds14 <- preparing_variable_labels(ds14 ,"o", "14")
ds14 <- rename2014(ds14)
ds14<- loneliness_three_items_recode(ds14)
ds14 <- lifesatisfaction_summaryscores(ds14)
ds14 <- social_support_network_recode(ds14)
ds14 <- welling_scale_summarize(ds14)

#dsphys14 <- preparing_variable_labels(dsC14 ,"O", "14")

ds14 <- physhealthrename2010_2014(ds14)
#ds2014<-merge(ds14, dsphys14, by = "hhidpn")

#demo14 <- preparing_variable_labels(demo14 ,"O", "14")

demo14 <- basicdemographics2014(ds14)
psychosocial_14 <- subset(ds14, select=c('hhidpn','loneliness_1','loneliness_2','loneliness_3',"loneliness_total",'loneliness_mean',"lifesatisfaction_mean",
                                         'snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri','close_relations','socnetwork',"mtchild", "spkchild", "wrtchild","mtfam", "spkfam", "wrtfam",
                                         "mtfriend", "spkfriend", "wrtfriend", 'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
                                         'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri','wellbeing_total','wellbeing_mean',"AD","dementia","Activity_vigorous", "Activity_moderate", "Activity_mild"))



ds2014<-merge(demo14, psychosocial_14, by = "hhidpn")

ds2014<-add_year_to_variable(data = ds2014, year_label = "_14")
colnames(ds2014)



###########Merge all years######################################
ds2004$ID <- ds2004$hhidpn_04
ds2006$ID <- ds2006$hhidpn_06
ds2008$ID <- ds2008$hhidpn_08
ds2010$ID <- ds2010$hhidpn_10
ds2012$ID <- ds2012$hhidpn_12
ds2014$ID <- ds2014$hhidpn_14

hrs1<-merge(ds2004, ds2006, by = "ID", all = TRUE)
hrs2<-merge(hrs1, ds2008, by = "ID", all = TRUE)
hrs3<-merge(hrs2, ds2010, by = "ID", all = TRUE)
hrs4<-merge(hrs3, ds2012, by = "ID", all = TRUE)
hrs_all<-merge(hrs4, ds2014, by = "ID", all = TRUE)

saveRDS(hrs_all, paste0("./data/derived/unshared/", "hrs", ".rds"))
write.csv(hrs_all,"./data/derived/unshared/hrs.csv")

