#CED-S is actually in the cognition section

# Function that recodes and summarizes the ceds data
CEDS <- function(data){
  
  plyr::rename(data, replace = c(
    "d110"="ceds1",
    "d111"="ceds2",
    "d112"="ceds3",
    "d113"="ceds4",
    "d114"="ceds5",
    "d115"="ceds6",
    "d116"="ceds7",
    "d117"="ceds8",
    "d118"="ceds9"
  ))
  #Variables need to recoded 1 is the depressive answer for items 1,2,3; 5 means No, 8 is DK and 9 is refused
  ceds_to_recode <- c("ceds1", "ceds2","ceds3", "ceds5","ceds7","ceds8")
  #Variables need to recoded 5 is the depressive answer; 1 means yes the non-depressive answer, 8 is DK and 9 is refused
  ceds_to_recodepositive <- c("ceds4","ceds6","ceds9")
  
  reverse_coding_ceds <- function(data, variables){
    data[variables] <- plyr::mapvalues(data[,variables], from=c(1,5,8,9), to =c(1,0,NA,NA))
    return(data)
  }
  
  for(i in ceds_to_recode){
    data <- reverse_coding_ceds(data, variable = i) 
  }
  
  reverse_coding_ceds_positive <- function(data, variables){
    data[variables] <- plyr::mapvalues(data[,variables], from=c(1,5,8,9), to =c(0,1,NA,NA))
    return(data)
  }
  
  for(i in ceds_to_recodepositive){
    data <- reverse_coding_ceds_positive(data, variable = i) 
  }
  
  s <- which(colnames(data)=="ceds1")
  f <- which(colnames(data)=="ceds9")
  data$cedstotal <-rowSums(data[s:f], na.rm=TRUE)
  
  #Create a variable that indicates the number of missing per person.
  #data$missing <- (is.na(data$wellbeing_1) + is.na(data$wellbeing_2) + is.na(data$wellbeing_3) + is.na(data$wellbeing_4)+is.na(data$wellbeing_5)+is.na(data$wellbeing_6)+is.na(data$wellbeing_7))
  #data$wellbeing_mean <- ifelse(data$missing<3, data$wellbeing_total/(7-data$missing), NA)
  
  return(data)
}
  #Total the Mental Status Questions
  #Recode the variables first
MentalStatus <- function(data){
  data$msmonth[data$msmonth==5|data$msmonth==8] <- 0
  data$msmonth[data$msmonth==9] <- NA
  
  data$msdate[data$msdate==5|data$msdate==8] <- 0 
  data$msdate[data$msdate==9] <- NA
  data$msyear[data$msyear==5|data$msyear==8] <- 0
  data$msyear[data$msyear==9] <- NA
  data$msday[data$msday==5|data$msday==8]<-0
  data$msday[data$msday==9]<-NA
  
  data$msnaming1[data$msnaming1==5|data$msnaming1==8] <- 0 
  data$msnaming1[data$msnaming1==9] <- NA
  data$msnaming2[data$msnaming2==5|data$msnaming2==8] <- 0
  data$msnaming2[data$msnaming2==9] <- NA
  
  data$mspresident[data$mspresident==5|data$mspresident==8]<-0
  data$mspresident[data$mspresident==9]<-NA
  data$msvp[data$msvp==5|data$msvp==8]<-0
  data$msvp[data$msvp==9]<-NA
  
  s <- which(colnames(data)=="msmonth")
  f <- which(colnames(data)=="mspresident")
  data$mentalstot <-rowSums(data[s:f], na.rm=TRUE)
  return(data)
}

#Vocabulary
Vocabulary<-function(data){
  #Variables need to recoded 9 is refused recoded as NA
  vocab_to_recode <- c("vocab1", "vocab2","vocab3", "vocab5")

  reverse_coding_vocab <- function(data, variables){
    data[variables] <- plyr::mapvalues(data[,variables], from=c(9), to =c(NA))
    return(data)
  }
  
  for(i in vocab_to_recode){
    data <- reverse_coding_vocab(data, variable = i) 
  }
  
  s <- which(colnames(data)=="vocab1")
  f <- which(colnames(data)=="vocab5")
  data$vocabtotal <-rowSums(data[s:f], na.rm=TRUE)

  return(data)
}

#numeracy
ds1 <- plyr::rename(x=ds1, replace = c(
  "d178" = "numer1",
  "d179" = "numer2",
  "d180" = "numer3"
))

Numeracy<-function(data){
  #Variables need to recoded 9999998 is DK or not ascertained, 9999999 is refused recoded as NA
  numeracy_to_recode <- c("numer1", "numer2","numer3")
  
  reverse_coding_numeracy <- function(data, variables){
    data[variables] <- plyr::mapvalues(data[,variables], from=c(9), to =c(NA))
    return(data)
  }
  
  for(i in vocab_to_recode){
    data <- reverse_coding_vocab(data, variable = i) 
  }
  
  s <- which(colnames(data)=="vocab1")
  f <- which(colnames(data)=="vocab5")
  data$vocabtotal <-rowSums(data[s:f], na.rm=TRUE)
  
  return(data)
}


ds04 <- readRDS("./Data/Derived/unshared/h04f1a.rds")

source(paste0(pathDir,"/scripts/data/cassandra/rename2004.R"))
ds04 <- preparing_variable_labels(ds04, "J", "04")
ds04<-rename2004(ds04)
range(ds04$serial71, na.rm=TRUE)

#Data checking Serial 7s prior to creating a summary variable.
a <- table(ds04$serial71)
a
b <- table(ds04$serial72)
b

ds04$serial71[ds04$serial71==998] <- 0
ds04$serial71[ds04$serial71==999] <- NA
row <- which(ds04$serial71>100)
s <- which(colnames(ds04)=="serial71")
f <- which(colnames(ds04)=="serial75")
print(ds04[row,445:449])

if

b <- table(ds06$serial72)
b


hist(ds04$serial71)
#Recode typo responses as 93
ds04$serial71[ds04$serial71==933] <-93
ds04$serial71[ds04$serial71==93] <- 1

ds04$serial71[ds04$serial71==993] <- NA
ds04$serial71[ds04$serial71==992] <- NA
ds04$serial71[]
ds04$serial71[ds04$serial71!=93|ds04$serial71!=998|ds04$serial71!=999|is.na(ds04$serial71)==FALSE]<- 0

mean(ds04$serial71, na.rm=TRUE)
head(ds04$serial71)
source(paste0(pathDir,"/scripts/data/cassandra/rename2006.R"))
ds06 <- preparing_variable_labels(ds06,"K", "06")
ds06 <- rename2006(ds06)
head(ds06$serial71)

mean(ds06$serial71, na.rm=TRUE)
#Working memory
WorkingMemory <- function(data){
mean(ds04$serial71)  
ds04$serial1[ds04$serial71==93] <- 1
data$serial1[data$serial71==998] <- 0
data$serial1[data$serial71==999] <- NA
data$serial1[data$serial71!=93|data$serial71!=998|data$serial71!=999|data$serial71!=NA]<- 0
}
