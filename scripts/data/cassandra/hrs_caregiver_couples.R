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

ds1 <- ds0[ds0$dementia_14==1|ds0$AD_14==1,]
ds1<- ds1[is.na(ds1$dementia_14)==FALSE,]
ds2<-ds0[ds0$dementia_14==1|ds0$AD_14==1,]
ds2<- ds2[is.na(ds2$AD_14)==FALSE,]

write.csv(ds2,"./data/derived/unshared/hrsdementia.csv")
hist(ds2$demtime)

# selecting observations based on variable values
#####################working here
ds0$dem_14
ds0$spouse_dem_14 <- NA

rows <- nrow(ds0)
column <- which(colnames(ds0)=="spouse_dem_14")

for(i in 1:rows){
row <- i
partner <- ds0[row, colnames(ds0)=="phhidpn_12"]
prow <-which(ds0$ID==partner)
ifelse(ds0[row, which(colnames(ds0)=="dem_14")]==1, ds0[prow,column] <- 1, ds0[prow,column] <- 0)
}
ds0$spouse_dem_14 <- factor(ds0$spouse_dem_14)
describe(ds0$spouse_dem_14)

library(psych)
vars <- c("loneliness_mean_14")
describeBy(ds0[vars], list(spousedem=ds0$spouse_dem_14))
write.csv(ds0,"./data/derived/unshared/hrsdementia.csv")
library(MASS)
t.test(loneliness_mean_14 ~ spouse_dem_14, data = ds0)
spouses <- subset(ds0,hrsspouse==TRUE)


demspouse <- subset(spouses,dem>1)
table(demspouse$a009.10)                                    

demspouse <- subset(demspouse, select=c(hhidpn, a009.12,a103.12,dem.12, dem.10, dem.08, dem.06, dem.04,c272.12, c272.10, c273.12, c273.10, c069.08, C069.06,C069.04))

print(which(demspouse$dem.12=="0"))
which(demspouse$dem.10=="1"& demspouse$dem.12=="0")
table(demspouse$dem.12)
table(demspouse$a009.12)

table(spouses$dem)
table(demspouse$c272.12)
table(demspouse$c273.12)
table(demspouse$c272.10)
table(demspouse$c273.10)
table(demspouse$c069.08)
table(demspouse$C069.06)
table(demspouse$C069.04)
