options(width=160)
rm(list=ls())
cat("\f")
library(data.table)

pathDir <- getwd() # establish home directory
pathFiles <- file.path(pathDir,"Data/Derived/unshared//")
ds<- readRDS(paste0(pathFiles,"H14LB_R.RDS"))
year_letterid <-"O"
#only for 2014
ds$hhidpn <- paste0(ds$HHID,"0",ds$PN)

#Temporarily blocked this out
#psychosocial<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,2)=="LB" | substring(varnames2,1,2)=="lb"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  ds1<-ds[section]
  
#} uncomment this to create the function

#Note that the numbering of specific items occasionally changes from year to year.
#Loneliness scale summary

#Reverse code items 20a 20b 20c and 20e (19a 19b 19c and 19e in some years)

ds1$lone1[ds1$LB019A==1] <- 3
ds1$lone1[ds1$LB019A==2]<- 2
ds1$lone1[ds1$LB019A==3]<- 1

ds1$lone2[ds1$LB019B==1] <- 3
ds1$lone2[ds1$LB019B==2]<- 2
ds1$lone2[ds1$LB019B==3]<- 1

ds1$lone3[ds1$LB019C==1] <- 3
ds1$lone3[ds1$LB019C==2]<- 2
ds1$lone3[ds1$LB019C==3]<- 1

ds1$lone5[ds1$LB019E==1] <- 3
ds1$lone5[ds1$LB019E==2]<- 2
ds1$lone5[ds1$LB019E==3]<- 1

#otherwise rename loneliness scale variables for consistency
ds1$lone4 <- ds1$LB019D
ds1$lone6 <- ds1$LB019F
ds1$lone7 <- ds1$LB019G
ds1$lone8 <- ds1$LB019H
ds1$lone9 <- ds1$LB019I
ds1$lone10 <- ds1$LB019J
ds1$lone11 <- ds1$LB019K


#create variables that indicate missing numbers

m1 <- ifelse(is.na(ds1$lone1)==TRUE, 1, 0)
m2 <- ifelse(is.na(ds1$lone2)==TRUE, 1, 0)
m3 <- ifelse(is.na(ds1$lone3)==TRUE, 1, 0)
m4 <- ifelse(is.na(ds1$lone4)==TRUE, 1, 0)
m5 <- ifelse(is.na(ds1$lone5)==TRUE, 1, 0)
m6 <- ifelse(is.na(ds1$lone6)==TRUE, 1, 0)
m7 <- ifelse(is.na(ds1$lone7)==TRUE, 1, 0)
m8 <- ifelse(is.na(ds1$lone8)==TRUE, 1, 0)
m9 <- ifelse(is.na(ds1$lone9)==TRUE, 1, 0)
m10 <- ifelse(is.na(ds1$lone10)==TRUE, 1, 0)
m11 <- ifelse(is.na(ds1$lone11)==TRUE, 1, 0)

ds1$lonemiss <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m10


which(colnames(ds1)=="lone1")
which(colnames(ds1)=="lone11")
ds1$lonetot <- rowSums(ds1[388:398], na.rm=TRUE)

#Create the loneliness scale score only if there is less than 6 missing
#this is as per codebook instructions.
ds1$lonemean <- ifelse(ds1$lonemiss<6, ds1$lonetot/(11-ds1$lonemiss), NA)

summary(ds1$lonemean)



####

# Final step add the year to the  variables
vars<-colnames(ds1)
for (i in 1:length(ds1)){
  vars[i]<-paste0(vars[i],year_label)
}

colnames(data)<-vars
return(data)
  

