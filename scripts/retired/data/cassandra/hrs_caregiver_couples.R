options(width=160)
rm(list=ls())
cat("\f")

#Read in merged data file created with 0_import_raw.R

#Create a variable that returns true if there is a valid spouse pphhidpn
# indicates those who have spouses who are also HRS participants.
ds0 <- readRDS("./data/derived/unshared/hrs.rds")

ds0$everAD <- ds0$AD_10==1 | ds0$AD_12==1 | ds0$AD_14==1
ds0$everdem <- ds0$dementia_12==1|ds0$dementia_10==1|ds0$dementia_14==1
ds0$eMempr <- ds0$memoryproblem_04==1 | ds0$memoryproblem_06==1 | ds0$memoryproblem_08==1

ds0$dem <- ifelse(everAD==TRUE, 3, 
                  ifelse(everdem==TRUE, 2, 0)) 
                         #ifelse(eMempr==TRUE, 1, 0)))

ds0$dem_14 <- ifelse(ds0$AD_14==1 | ds0$dementia_14==1, 1, 0)
ds0$dem_12 <- ifelse(ds0$AD_12==1 | ds0$dementia_12==1, 1, 0)
ds0$dem_10 <- ifelse(ds0$AD_10==1 | ds0$dementia_10==1, 1, 0)

ds0$dem_08 <- ifelse(ds0$memoryproblem_08==1,1, 0)
ds0$dem_06 <- ifelse(ds0$memoryproblem_06==1, 1,0)
ds0$dem_04 <- ifelse(ds0$memoryproblem_04==1,1,0)

#Creates a time to dementia variable
s <- which(colnames(ds0)=="dem_14")
f <- which(colnames(ds0)=="dem_10")
ds0$demtime <- rowSums(ds0[s:f])

# selecting observations based on variable values
#####################working here
ds0$dem_14
ds0$spouse_dem_14 <- NA

rows <- nrow(ds0)
column <- which(colnames(ds0)=="spouse_dem_14")

for(i in 1:rows){
row <- i
partner <- ds0[row, colnames(ds0)=="phhidpn_14"]
prow <-which(ds0$ID==partner)
ifelse(ds0[row, which(colnames(ds0)=="dem_14")]==1, ds0[prow,column] <- 1, ds0[prow,column] <- 0)
}
ds0$spouse_dem_14 <- factor(ds0$spouse_dem_14)
describe(ds0$spouse_dem_14)

##############spouse dementia at 2012
ds0$dem_12
ds0$spouse_dem_12 <- NA

rows <- nrow(ds0)
column <- which(colnames(ds0)=="spouse_dem_12")

for(i in 1:rows){
  row <- i
  partner <- ds0[row, colnames(ds0)=="phhidpn_12"]
  prow <-which(ds0$ID==partner)
  ifelse(ds0[row, which(colnames(ds0)=="dem_12")]==1, ds0[prow,column] <- 1, ds0[prow,column] <- 0)
}
ds0$spouse_dem_12 <- factor(ds0$spouse_dem_12)
describe(ds0$spouse_dem_12)

################spouse dementia 2010
ds0$dem_10
ds0$spouse_dem_10 <- NA

rows <- nrow(ds0)
column <- which(colnames(ds0)=="spouse_dem_10")

for(i in 1:rows){
  row <- i
  partner <- ds0[row, colnames(ds0)=="phhidpn_10"]
  prow <-which(ds0$ID==partner)
  ifelse(ds0[row, which(colnames(ds0)=="dem_10")]==1, ds0[prow,column] <- 1, ds0[prow,column] <- 0)
}
ds0$spouse_dem_10 <- factor(ds0$spouse_dem_10)
describe(ds0$spouse_dem_10)

################spouse memory problems 2008
ds0$dem_08
ds0$spouse_dem_08 <- NA

rows <- nrow(ds0)
column <- which(colnames(ds0)=="spouse_dem_08")

for(i in 1:rows){
  row <- i
  partner <- ds0[row, colnames(ds0)=="phhidpn_08"]
  prow <-which(ds0$ID==partner)
  ifelse(ds0[row, which(colnames(ds0)=="dem_08")]==1, ds0[prow,column] <- 1, ds0[prow,column] <- 0)
}
ds0$spouse_dem_08 <- factor(ds0$spouse_dem_08)
describe(ds0$spouse_dem_08)

##########Spouse memory problems 2006#########

ds0$spouse_dem_06 <- NA

rows <- nrow(ds0)
column <- which(colnames(ds0)=="spouse_dem_06")

for(i in 1:rows){
  row <- i
  partner <- ds0[row, colnames(ds0)=="phhidpn_06"]
  prow <-which(ds0$ID==partner)
  ifelse(ds0[row, which(colnames(ds0)=="dem_06")]==1, ds0[prow,column] <- 1, ds0[prow,column] <- 0)
}
ds0$spouse_dem_06 <- factor(ds0$spouse_dem_06)
describe(ds0$spouse_dem_06)

##########Spouse memory problems 2004 #########

ds0$spouse_dem_04 <- NA

rows <- nrow(ds0)
column <- which(colnames(ds0)=="spouse_dem_04")

for(i in 1:rows){
  row <- i
  partner <- ds0[row, colnames(ds0)=="phhidpn_04"]
  prow <-which(ds0$ID==partner)
  ifelse(ds0[row, which(colnames(ds0)=="dem_04")]==1, ds0[prow,column] <- 1, ds0[prow,column] <- 0)
}
ds0$spouse_dem_04 <- factor(ds0$spouse_dem_04)
describe(ds0$spouse_dem_04)

###### Calculate how long the spouse has had dementia for #############


ds0$spousedem_time <- ifelse(ds0$spouse_dem_14==1, 1, 0)
ds0$spousedem_time <- ifelse(ds0$spouse_dem_12==1, ds0$spousedem_time+1, ds0$spousedem_time)  
ds0$spousedem_time <- ifelse(ds0$spouse_dem_10==1, ds0$spousedem_time+1, ds0$spousedem_time)  
ds0$spousedem_time <- ifelse(ds0$spouse_dem_08==1, ds0$spousedem_time+1, ds0$spousedem_time)
ds0$spousedem_time <- ifelse(ds0$spouse_dem_06==1, ds0$spousedem_time+1, ds0$spousedem_time)
ds0$spousedem_time <- ifelse(ds0$spouse_dem_04==1, ds0$spousedem_time+1, ds0$spousedem_time)  
describe(ds0$spousedem_time)
ds0$spousedem_time

ds0$spousedem_time2 <- ifelse(ds0$spouse_dem_14==1, 1, 0)
ds0$spousedem_time2 <- ifelse(ds0$spouse_dem_12==1, ds0$spousedem_time+1, ds0$spousedem_time)  
ds0$spousedem_time2<- ifelse(ds0$spouse_dem_10==1, ds0$spousedem_time+1, ds0$spousedem_time)  


ds1 <- ds0[which(ds0$spouse_dem_14 == 1),]

write.csv(ds1,"./data/derived/unshared/hrsdementia.csv")

library(psych)
vars <- c("loneliness_mean_14")
describeBy(ds0[vars], list(spousedem=ds0$spouse_dem_14))

library(MASS)
t.test(loneliness_mean_14 ~ spouse_dem_14, data = ds0)
spouses <- subset(ds0,hrsspouse==TRUE)


demspouse <- subset(spouses,dem>1)
table(demspouse$a009.10)                                    


print(which(demspouse$dem.12=="0"))
which(demspouse$dem.10=="1"& demspouse$dem.12=="0")
table(demspouse$dem.12)
table(demspouse$a009.12)

ds1$lbdata <- ifelse(is.na(ds1$loneliness_mean_14)==FALSE, 1, 0)
describe(ds1$lbdata)


ds1$spousedem_time2 <- ifelse(ds1$spouse_dem_14==1, 1, 0)
ds1$spousedem_time2 <- ifelse(ds1$spouse_dem_12==1, ds1$spousedem_time2+1, ds1$spousedem_time2)  
ds1$spousedem_time2<- ifelse(ds1$spouse_dem_10==1, ds1$spousedem_time2+1, ds1$spousedem_time2) 

describe(ds1$spousedem_time2)
describe(ds1$spousedem_time)

loneliness <- c("loneliness_mean_14","loneliness_mean_12","loneliness_mean_10","loneliness_mean_08","loneliness_mean_06","loneliness_mean_04")
psych::describeBy(ds1[loneliness], list(am=ds1$spouse_dem_12))

