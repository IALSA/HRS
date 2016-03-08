options(width=160)
rm(list=ls())
cat("\f")
library(data.table)
library(plyr)

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
  
  #changes all the variable names to lower case
  names(ds) <- tolower(varnames)
  varnames2 <- names(ds)
  #rename essential variables with names for consistency
  #varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  id<-c("hhidpn")
  condition <- substring(varnames2,1,2)=="lb"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  ds1<-ds[section]
  
#} uncomment this to create the function

#Note that the numbering of specific items occasionally changes from year to year.
#Loneliness scale summary


setnames(ds1, old = c('lb019a','lb019b','lb019c','lb019d','lb019e','lb019f','lb019g','lb019h','lb019i'
                    ,'lb019j','lb019k'), new = c('lone1','lone2','lone3','lone4','lone5','lone6','lone7',
                    'lone8','lone9','lone10','lone11'))
  
#Reverse code items 20a 20b 20c and 20e (19a 19b 19c and 19e in some years)

ds1$lone1 <- plyr::mapvalues(ds1$lone1, from=c(1,2,3), to =c(3,2,1))
ds1$lone2 <- plyr::mapvalues(ds1$lone2, from=c(1,2,3), to =c(3,2,1))
ds1$lone3 <- plyr::mapvalues(ds1$lone3, from=c(1,2,3), to =c(3,2,1))
ds1$lone5 <- plyr::mapvalues(ds1$lone5, from=c(1,2,3), to =c(3,2,1))


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


s <- which(colnames(ds1)=="lone1")
f <- which(colnames(ds1)=="lone11")
ds1$lonetot <- rowSums(ds1[s:f], na.rm=TRUE)


#Create the loneliness scale score only if there is less than 6 missing
#this is as per codebook instructions.
ds1$lonemean <- ifelse(ds1$lonemiss<6, ds1$lonetot/(11-ds1$lonemiss), NA)

summary(ds1$lonemean)

#Create activity variable list
#start by renaming the activity variables for consistency
#Rename for 2014
setnames(ds1, old = c("lb001a","lb001b","lb001c","lb001d","lb001e","lb001f","lb001g","lb001h","lb001i","lb001j",
                      "lb001k","lb001l","lb001m","lb001n","lb001o","lb001p","lb001q","lb001r","lb001s","lb001t","lb001u"),
              new = c('act1','act2','act3','act4','act5','act6','act7','act8','act9','act10','act11','act12','act13','act14'
                      ,'act15','act16','act17','act18','act19','act20','act21'))


#Adding variables should be consistent across years
attr(ds1$act1,"label") <- "Q01A OFTEN CARE ADULT"
attr(ds1$act2,"label") <- "OFTEN DO ACTIVITIES WITH GRANDCHILDREN"
attr(ds1$act3,"label") <- "OFTEN VOLUNTEER YOUTH"
attr(ds1$act4,"label") <- "OFTEN CHARITY WORK"
attr(ds1$act5,"label") <- "Q01E. OFTEN EDUCATION"
attr(ds1$act6,"label") <- "Q01F. OFTEN ATTEND SPORTS/SOCIAL/CLUB"
attr(ds1$act7,"label") <- "Q01G. OFTEN ATTEND NON RELIGIOUS ORGS"
attr(ds1$act8,"label") <- "Q01H. OFTEN PRAY PRIVATELY"
attr(ds1$act9,"label") <- "Q01I. OFTEN READ"
attr(ds1$act10,"label") <- "Q01J. OFTEN WATCH TELEVISION"
attr(ds1$act11,"label") <- "Q01K. OFTEN DO WORD GAMES"
attr(ds1$act12,"label") <- "Q01L. OFTEN PLAY CARDS AND GAMES"
attr(ds1$act13,"label") <- "Q01M. OFTEN DO WRITING"
attr(ds1$act14,"label") <- "Q01N. OFTEN USE COMPUTER"
attr(ds1$act15,"label") <- "Q01O. OFTEN MAINTENANCE/GARDENING"
attr(ds1$act16,"label") <- "Q01P. OFTEN BAKE OR COOK"
attr(ds1$act17,"label") <- "Q01Q. OFTEN SEW OR KNIT"
attr(ds1$act18,"label") <- "Q01R. OFTEN DO HOBBY"
attr(ds1$act19,"label") <- "Q01S. OFTEN PLAY SPORT/EXERCISE"
attr(ds1$act20,"label") <- "Q01T. OFTEN WALK FOR 20 MINS"
attr(ds1$act21,"label") <- "Q01U. PARTICIPATE COMMUNITY ARTS GRP"

s <- which(colnames(ds1)=="act1")
f <- which(colnames(ds1)=="act21")
ds1$ActivityTot <- rowSums(ds1[s:f], na.rm=FALSE)
summary(ds1$ActivityTot)
ds1$mixAct <- ds1$act1+ds1$act2+ds1$act3+ds1$act4+ds1$act5+ds1$act6+ds1$act7+ds1$act12+ds1$act21
ds1$cogAct <- ds1$act9+ds1$act11+ds1$act13+ds1$act14
ds1$physAct <- ds1$act19+ds1$act20+ds1$act15 
ds1$otherAct <- ds1$act8+ds1$act10+ds1$act16+ds1$act17+ds1$act18

summary(ds1$mixAct)

####


#Q2 on the LB questionnaire is (in some years) the 5 item-life satisfaction scale
#In 06, 08, 10, the life satisfaction scale is Q3 (Q2 is a retrospective social participation question)


# rename Life satisfaction '14
setnames(ds1, old=c('lb002a','lb002b','lb002c','lb002d','lb002e'),new=('lifesat1','lifesat2','lifesat3','lifesat4','lifesat5'))
  


attr(ds1$lifesat1,"label") <- "Q02A. LIFE IS CLOSE TO IDEAL"
attr(ds1$lifesat2,"label") <- "Q02B. CONDITIONS OF LIFE ARE EXCELLENT"
attr(ds1$lifesat3,"label") <- "Q02C. SATISFIED WITH LIFE"
attr(ds1$lifesat4,"label") <- "Q02D. HAVE IMPORTANT THINGS IN LIFE"
attr(ds1$lifesat5,"label") <- "Q02E. CHANGE NOTHING IF LIVED LIFE OVER"

which(colnames(ds1)=="lifesat1")
which(colnames(ds1)=="lifesat5")

ds1$lifetot <- rowSums(ds1[428:432], na.rm=TRUE)
m1 <- ifelse(is.na(ds1$lifesat1)==TRUE, 1, 0)
m2 <- ifelse(is.na(ds1$lifesat2)==TRUE, 1, 0)
m3 <- ifelse(is.na(ds1$lifesat3)==TRUE, 1, 0)
m4 <- ifelse(is.na(ds1$lifesat4)==TRUE, 1, 0)
m5 <- ifelse(is.na(ds1$lifesat5)==TRUE, 1, 0)
ds1$lifesatmiss <- m1+m2+m3+m4+m5
summary(ds1$lifesatmiss)
sum(ds1)
#Calculate a life satisfaction score averaging across 5 items, less if some are missing.
#Note that '06 scale is 1-6, '08 and onwards scale is 1-7.
ds1$lifesatm <- ifelse(ds1$lifesatmiss<3, ds1$lifetot/(5-ds1$lifesatmiss), NA)
summary(ds1$lifesatm)

#Q4-Q18 Social network - social integration-quality of relationships-social support
#Q4-6 spouse/partner
#Q7-10 children
#Q11-14 family
#Q15-18 friends

#composition of social network '14 (note the variable numbers change)
ds3 <- within(ds1, {snspouse <- lb003
                    snchild <- lb006
                    snfamily <- lb010
                    snfriends <- lb014})

#Past here not year specific
ds3 <- within(ds3, {snspouse[snspouse==5]<- 0
                    snchild[snchild==5]<-0
                    snfamily[snfamily==5]<-0
                    snfriends[snfriends==5]<-0
                    snfriends[snfriends==7] <- NA
                    })

#number of close relationships, spouse excluded from total score
ds3 <- within(ds3, {closespouse<-lb005
                    closechild <-lb009
                    closefam <- lb013
                    closefri <- lb017
                    closerel <- lb009+lb013+lb017})
#social network contact excluding spouse
ds3 <- within(ds3, {meetch <- lb008a
                    speakch <- lb008b
                    writech <- lb008c
                    socialmech <- lb008d
                    meetfam<- lb012a
                    speakfam <- lb012b
                    writefam <- lb012c
                    socialmefam <- lb012d
                    meetfri <- lb016a
                    speakfri <- lb016b
                    writefr <- lb016c
                    socialmefri <- lb016d})
ds3 <- within(ds3, {snfriends[snfriends==7] <- NA})

summary(ds3$snspouse)
summary(ds3$snchild)
summary(ds3$snfamily)
summary(ds3$snfriends)
which(ds3$snfriends==7)
#recode the 7 value to NA
ds3$snfriends[ds3$snfriends==7] <- NA
which(colnames(ds3)=="snspouse")
which(colnames(ds3)=="snfriends")
ds3$socnetwork <- rowSums(ds3[436:439])
summary(ds3$socnetwork)

# Final step add the year to the  variables

vars<-colnames(ds1)
for (i in 1:length(ds1)){
  vars[i]<-paste0(vars[i],year_label)
}

colnames(data)<-vars
return(data)
  

