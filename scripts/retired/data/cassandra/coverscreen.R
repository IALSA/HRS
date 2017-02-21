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

coverscreen<-function(datayr,yrletter,yr){

#create a for loop that takes the "J" for 04 off the front 
#of the variable name and adds .04.
varnames<-colnames(datayr)

for (i in 1:length(varnames)){
  if(substring(varnames[i],1,1)==yrletter){
    varnames[i]<-substring(varnames[i],2)
  }else{
  varnames[i]<-varnames[i]
  }
}
varnames[1:100]
#rename essential variables with names for consistency
varnames2<-ifelse(varnames=="HHIDPN","hhidpn",
            ifelse(varnames=="BIRTHYR","birthyr",
            ifelse(varnames=="BIRTHMO","birthmo",
            ifelse(varnames=="DEGREE","degree",
            ifelse(varnames=="GENDER","gender",
            ifelse(varnames=="HISPANIC","hispanic",
            ifelse(varnames=="RACE","race",
            ifelse(varnames=="STUDY","study",varnames))))))))

#variables identifiable by consistent names
colnames(datayr)<-varnames2
prescreenvars<-c("hhidpn","birthyr","birthmo","degree","gender","hispanic","race","study")
coverscreen<-varnames2[which(substring(varnames2,1,1)=="A")]
demovars<-c(prescreenvars,coverscreen)

data<-datayr[demovars]
data[1:10,]
data <- plyr::rename(data, 
  replace = c(
  "A500"="intMonth",
  "A501"="intYr",
  "A002"="agreeInt",
  "A009"="proxy",
  "A010"="sameproxy",
  "A103"="proxyrel",
  "A011"="proxyCog",
  "A012"="language",
  "A019"="age",
  "A028"="nursH",
  "A065"="nursmth",
  "A066"="nursyr",
  "A022"="samesp",
  "A023"="spalive",
  "A024"="mpdie",
  "A025"="ypdie",
  "A026"="married",
  "A027"="livingwp",
  "A030"="livingwsp",
  "A031"="stoplivtogm",
  "A032"="stoplivtogy",
  "A033"="spnursh",
  "A034"="separated",
  "A035"="livpart",
  "A036"="newptm",
  "A037"="newptyr",
  "A045"="newsp",
  "A044"="spage",
  "A050"="financialk",
  "A055"="famfinres",
  "A056"="respchange",
  "A070"="home",
  "A071"="sameres1",
  "A072"="samecity1",
  "A073"="sameres2",
  "A074"="samecity2",
  "A076M"="state",
  "A078"="yrscity",
  "A079"="otherhome",
  "A081M"="otherhomestate",
  "A083"="yrs2city",
  "A084"="numresidences",
  "A085"="mainres",
  "A098"="hhmcount",
  "A099"="numreskid",
  "A100"="numnoreskid",
  "A101"="kidsnotsp",
  "A102"="kidmove",
  "A106"="kidcontact",
  "A113"="extendedfam",
  "A117"="hhmem"))


#add the year to the prescreen variables
prescvars<-colnames(data)
for (i in 1:length(data)){
  prescvars[i]<-paste0(prescvars[i],yr)
}

colnames(data)<-prescvars
return(data)
}
cover04<-coverscreen(hrs04,"J",".04")

demo04[1:10,]
