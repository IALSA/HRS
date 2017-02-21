library(dplyr)
library(zoo)
## @knitr setPaths

basicdemographics<-function(data){

#recode as missing missing year variables
data$b066.2[data$b066.2==9999|data$b066.2==9998] <- NA


#Creates single date variables 

data[,'birthdate'] <- paste(data[,"birthyr"],data[,'birthmo'], sep = "-")
data[,'birthdate'] <- as.yearmon(data$birthdate, "%Y-%m")
data[,'interviewdate'] <- paste(data[,'a500'], data[,'a501'], sep = "-")
data[,'interviewdate'] <- as.yearmon(data$interviewdate, "%m-%Y")
data[,'nursinghomedate'] <- as.yearmon(paste(data[,'a065'], data[,'a066'], sep = "-"))
data[,'firstmarriagedate'] <- as.yearmon(paste(data[,'b066.1'], data[,'b067.1'], sep = "-"))
data[,'secondmarriagedate'] <- as.yearmon(paste(data[,'b066.2'], data[,'b067.2'], sep = "-"))
data[,'thirdmarriagedate'] <- as.yearmon(paste(data[,'b066.3'], data[,'b067.3'], sep = "-"))
data[,'fourthmarriagedate'] <- as.yearmon(paste(data[,'b066.4'], data[,'b067.4'], sep = "-"))
data[,'btw_wavemarriagedate'] <- as.yearmon(paste(data[,'b056'], data[,'b057'], sep = "-"))
data[,'btw_wave_divorcewidow_date'] <- as.yearmon(paste(data[,'b059'], data[,'b060'], sep = "-"))                                               

#proxy_self_interview a value of 2 means spouse is the reporter, 4 means spouse that does not live with respondent
data <- plyr::rename(data, 
  replace = c(
  "a009"="proxy_self_interview",
  "a011"="proxyratecogfunction",
  "a012"="interview_language",
  "a019"="currentage",
  "a028"="nursinghome",
  "b002"="usborn2",
  "b006"="arriveyr",
  #"b014a"="educ",
  "b017m"="degree2",
  "b020"="ses",
  #"b028a"="hispanic2",
  #"b031a"="race2",
  "b050"="religion",
  "b082"="religious_service",
  "b053"="religious_import",
  "b054"="englishH",
  "b065"="nummarry",
  "b063"="married"
  ))

data2<- subset(data, select=c("hhidpn","birthdate","birthyr","birthmo","degree","gender","hispanic","race","study", "phhidpn","interviewdate", "nursinghomedate","firstmarriagedate","secondmarriagedate","thirdmarriagedate","fourthmarriagedate",
               "btw_wavemarriagedate", "btw_wave_divorcewidow_date","proxy_self_interview", "interview_language", "currentage","nursinghome",
               "usborn", "arriveyr", "degree", "ses","religion", "religious_service","religious_import",
               "englishH","nummarry","married"))

return(data2)
}


#Variables in years later than 2004 find these for 2004.  
#  "a100"="nonresidentkids",
# "a099"="residentkids",
#"b000"="lifesatisfaction"

basicdemographics2014<-function(data){
  
  #recode as missing missing year variables
  data$b066.2[data$b066_2==9999|data$b066_2==9998] <- NA
  
  
   
  #Could not find birthday variables
  #data[,'birthdate'] <- paste(data[,"birthyr"],data[,'birthmo'], sep = "-")
  #data[,'birthdate'] <- as.yearmon(data$birthdate, "%Y-%m")
  
  data[,'interviewdate'] <- paste(data[,'a500'], data[,'a501'], sep = "-")
  data[,'interviewdate'] <- as.yearmon(data$interviewdate, "%m-%Y")
  data[,'nursinghomedate'] <- as.yearmon(paste(data[,'a065'], data[,'a066'], sep = "-"))
  data[,'firstmarriagedate'] <- as.yearmon(paste(data[,'b066.1'], data[,'b067.1'], sep = "-"))
  data[,'secondmarriagedate'] <- as.yearmon(paste(data[,'b066.2'], data[,'b067.2'], sep = "-"))
  data[,'thirdmarriagedate'] <- as.yearmon(paste(data[,'b066.3'], data[,'b067.3'], sep = "-"))
  data[,'fourthmarriagedate'] <- as.yearmon(paste(data[,'b066.4'], data[,'b067.4'], sep = "-"))
  data[,'btw_wavemarriagedate'] <- as.yearmon(paste(data[,'b056'], data[,'b057'], sep = "-"))
  data[,'btw_wave_divorcewidow_date'] <- as.yearmon(paste(data[,'b059'], data[,'b060'], sep = "-"))                                               
  
  #proxy_self_interview a value of 2 means spouse is the reporter, 4 means spouse that does not live with respondent
  data <- plyr::rename(data, 
                       replace = c(
                         "a009"="proxy_self_interview",
                         "a011"="proxyratecogfunction",
                         "a012"="interview_language",
                         "a019"="currentage",
                         "a028"="nursinghome",
                         #"pn_sp.x"="phhidpn",
                         "b002"="usborn",
                         "b006"="arriveyr",
                         "b014"="educ",
                         "b017m"="degree",
                         "b020"="ses",
                         "b028"="hispanic",
                         "b091m"="race",
                         "b050"="religion",
                         "b082"="religious_service",
                         #"b053"="religious_import",
                         "b054"="englishH",
                         "b065"="nummarry",
                         "b063"="married"
                       ))
  
  data2<- subset(data, select=c("hhidpn","degree","hispanic","race","educ", "phhidpn","interviewdate", "nursinghomedate","firstmarriagedate","secondmarriagedate","thirdmarriagedate","fourthmarriagedate",
                                "btw_wavemarriagedate", "btw_wave_divorcewidow_date","proxy_self_interview", "interview_language", "currentage","nursinghome",
                                "usborn", "arriveyr", "degree", "ses","religion", "religious_service","englishH","nummarry","married"))
  
  return(data2)
}