options(width=160)
rm(list=ls())
cat("\f")

library(dplyr)
## @knitr setPaths

pathDir <- getwd() # establish home directory
pathFiles <- file.path(pathDir,"data/extract/RAND_vA/")
pathFile04 <- file.path(pathFiles, "h04f1a.Rds")
hrs04<-readRDS(pathFile04)

hrs04[1:10,1]
demographics <- function(ds, year_letterid, year_label){
  
  varnames<-colnames(ds)
  
  #create a for loop that remove the first character from variable/column name  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #changes all the variable names to lower case
  colnames(ds) <- tolower(varnames)
  varnames2 <- colnames(ds)
  
  #create a list of demographic variables
  prescreenvars<-c("hhidpn","birthyr","birthmo","degree","gender","hispanic","race","study", "phhidpn")
  condition <- substring(varnames2,1,1)=="a"
  coverscreen<-varnames2[which(condition)]
  condition <- substring(varnames2,1,1)=="b"
  demographic<-varnames2[which(condition)]
  demovars<-c(prescreenvars,coverscreen,demographic)
  # browser()
  data<-ds[demovars]
  data[1:10,]
  
  #add the year to the prescreen variables
  prescvars<-colnames(data)
  for (i in 1:length(data)){
    prescvars[i]<-paste0(prescvars[i], ".", year_label)
  }
  
  colnames(data)<-prescvars
  return(data)
}

selectdemographics<-function(datayr,yrletter,yr){

#create a for loop that takes the "J" for 04 off the front 
#of the variable name and adds .04.
varnames<-colnames(datayr)



#variables identifiable by consistent names
colnames(datayr)<-varnames2
id<-c("hhidpn")
demographics<-varnames2[which(substring(varnames2,1,1)=="B")]
vars<-c(id,demographics)

data<-datayr[vars]
data[1:10,]
data <- plyr::rename(data, 
  replace = c(
    "B002"="usborn",
    "B006"="arriveyr",
    "B014A"="educ",
    "B017M"="degree",
    "B020"="ses",
    "B026"="fathEd",
    "B027"="momEd",
    "B028A"="hispanic2",
    "B031A"="race2",
    "B033"="childev",
    "B034"="childliv",
    "B035"="military",
    "B038"="militarydis",
    "B045"="yrslivearea",
    "B050"="religion",
    "B082"="relServ",
    "B053"="relImport",
    "B054"="englishH",
    "B055"="marrynew",
    "B056"="marryyr",
    "B057"="marryyr",
    "B058"="divwidPW",
    "B059"="divwidmth",
    "B060"="divwidyr",
    "B061"="unmarried",
    "B065"="nummarry",
    "B066_1"="marry1yr",
    "B067_1"="marry1mth",
    "B068_1"="marry1end",
    "B070_1"="marry1yrs",
    "B066_2"="marry2yr",
    "B067_2"="marry2mth",
    "B068_2"="marry2end",
    "B070_2"="marry2yrs",
    "B066_3"="marry3yr",
    "B067_3"="marry3mth",
    "B068_3"="marry3end",
    "B070_3"="marry3yrs",
    "B066_4"="marry4yr",
    "B067_4"="marry4mth",
    "B068_4"="marry4end",
    "B070_4"="marry4yrs",
    "B063"="married2",
    "B076"="demhelp"))


#add the year to the prescreen variables
demovars<-colnames(data)
for (i in 1:length(data)){
  prescvars[i]<-paste0(demovars[i],yr)
}

colnames(data)<-demovars
return(data)
}
demo04<-demographics(hrs04,"J",".04")

demo04[1:10,]
