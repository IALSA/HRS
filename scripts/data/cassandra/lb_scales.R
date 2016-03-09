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
setnames(ds1, old=c("lb002a","lb002b","lb002c","lb002d","lb002e"), new= c("lifesat1","lifesat2","lifesat3","lifesat4","lifesat5"))
  
names(ds1)

attr(ds1$lifesat1,"label") <- "Q02A. LIFE IS CLOSE TO IDEAL"
attr(ds1$lifesat2,"label") <- "Q02B. CONDITIONS OF LIFE ARE EXCELLENT"
attr(ds1$lifesat3,"label") <- "Q02C. SATISFIED WITH LIFE"
attr(ds1$lifesat4,"label") <- "Q02D. HAVE IMPORTANT THINGS IN LIFE"
attr(ds1$lifesat5,"label") <- "Q02E. CHANGE NOTHING IF LIVED LIFE OVER"

s <- which(colnames(ds1)=="lifesat1")
f <- which(colnames(ds1)=="lifesat5")

ds1$lifetot <- rowSums(ds1[s:f], na.rm=TRUE)
m1 <- ifelse(is.na(ds1$lifesat1)==TRUE, 1, 0)
m2 <- ifelse(is.na(ds1$lifesat2)==TRUE, 1, 0)
m3 <- ifelse(is.na(ds1$lifesat3)==TRUE, 1, 0)
m4 <- ifelse(is.na(ds1$lifesat4)==TRUE, 1, 0)
m5 <- ifelse(is.na(ds1$lifesat5)==TRUE, 1, 0)
ds1$lifesatmiss <- m1+m2+m3+m4+m5
summary(ds1$lifesatmiss)

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
setnames(ds1, old = c('lb003','lb006','lb010','lb014'), new=c('snspouse','snchild','snfamily','snfriends'))


#Past here not year specific
ds1 <- within(ds1, {snspouse[snspouse==5]<- 0
                    snchild[snchild==5]<-0
                    snfamily[snfamily==5]<-0
                    snfriends[snfriends==5]<-0
                    snfriends[snfriends==7] <- NA
                    })

#number of close relationships, spouse excluded from total score
setnames(ds1, old=c('lb005','lb009','lb013','lb017'), new=c('closespouse','closechild','closefam','closefri'))
names(ds1)

ds1 <- within(ds1,{closerel<-closechild+closefam+closefri})
summary(ds1$closerel)

#social network contact excluding spouse
setnames(ds1, old=c("lb008a",'lb008b','lb008c','lb008d','lb012a','lb012b','lb012c','lb012d','lb016a','lb016b','lb016c','lb016d'),
         new=c('mtchild','spkchild','wrtchild','mediachild','mtfam','spkfam','wrtfam','mediafam','mtfriend','spkfriend','wrtfriend','mediafriend'))

s <- which(colnames(ds1)=="snspouse")
f <- which(colnames(ds1)=="snfriends")
ds1$socnetwork <- rowSums(ds1[s:f])
summary(ds1$socnetwork)

#Perceived social support(or relationship quality)
#Positive social support rename items associated with spouse
setnames(ds1, old=c('lb004a','lb004b','lb004c','lb004d','lb004e','lb004f','lb004g'), new=c('ssup1sp','ssup2sp','ssup3sp','ssup4sp','ssup5sp','ssup6sp','ssup7sp'))
setnames(ds1, old=c('lb007a','lb007b','lb007c','lb007d','lb007e','lb007f','lb007g'), new=c('ssup1ch','ssup2ch','ssup3ch','ssup4ch','ssup5ch','ssup6ch','ssup7ch'))
setnames(ds1, old=c('lb011a','lb011b','lb011c','lb011d','lb011e','lb011f','lb011g'), new=c('ssup1fam','ssup2fam','ssup3fam','ssup4fam','ssup5fam','ssup6fam','ssup7fam'))
setnames(ds1, old=c('lb015a','lb015b','lb015c','lb015d','lb015e','lb015f','lb015g'), new=c('ssup1fr','ssup2fr','ssup3fr','ssup4fr','ssup5fr','ssup6fr','ssup7fr'))

typeof(ds1$ssup1ch)
print(ds1$ssup1sp)
#reverse code all items
ds1$ssup1sp <- plyr::mapvalues(ds1$ssup1sp, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup2sp <- plyr::mapvalues(ds1$ssup2sp, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup3sp <- plyr::mapvalues(ds1$ssup3sp, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup4sp <- plyr::mapvalues(ds1$ssup4sp, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup5sp <- plyr::mapvalues(ds1$ssup5sp, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup6sp <- plyr::mapvalues(ds1$ssup6sp, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup7sp <- plyr::mapvalues(ds1$ssup7sp, from=c(1,2,3,4), to =c(4,3,2,1))

ds1$ssup1ch <- plyr::mapvalues(ds1$ssup1ch, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup2ch <- plyr::mapvalues(ds1$ssup2ch, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup3ch <- plyr::mapvalues(ds1$ssup3ch, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup4ch <- plyr::mapvalues(ds1$ssup4ch, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup5ch <- plyr::mapvalues(ds1$ssup5ch, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup6ch <- plyr::mapvalues(ds1$ssup6ch, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup7ch <- plyr::mapvalues(ds1$ssup7ch, from=c(1,2,3,4), to =c(4,3,2,1))

ds1$ssup1fam<- plyr::mapvalues(ds1$ssup1fam, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup2fam<- plyr::mapvalues(ds1$ssup2fam, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup3fam<- plyr::mapvalues(ds1$ssup3fam, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup4fam<- plyr::mapvalues(ds1$ssup4fam, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup5fam<- plyr::mapvalues(ds1$ssup5fam, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup6fam<- plyr::mapvalues(ds1$ssup6fam, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup7fam<- plyr::mapvalues(ds1$ssup7fam, from=c(1,2,3,4), to =c(4,3,2,1))

ds1$ssup1fr<- plyr::mapvalues(ds1$ssup1fr, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup2fr<- plyr::mapvalues(ds1$ssup2fr, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup3fr<- plyr::mapvalues(ds1$ssup3fr, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup4fr<- plyr::mapvalues(ds1$ssup4fr, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup5fr<- plyr::mapvalues(ds1$ssup5fr, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup6fr<- plyr::mapvalues(ds1$ssup6fr, from=c(1,2,3,4), to =c(4,3,2,1))
ds1$ssup7fr<- plyr::mapvalues(ds1$ssup7fr, from=c(1,2,3,4), to =c(4,3,2,1))

ds1$pssspouse <- ds1$ssup1sp+ds1$ssup2sp+ds1$ssup3sp
ds1$psschild <- ds1$ssup1ch+ds1$ssup2ch+ds1$ssup3ch
ds1$pssfam <- ds1$ssup1fam+ds1$ssup2fam+ds1$ssup3fam
ds1$pssfr <- ds1$ssup1fr+ds1$ssup2fr+ds1$ssup3fr

ds1$nssspouse <- ds1$ssup4sp+ds1$ssup5sp+ds1$ssup6sp+ds1$ssup7sp
ds1$nsschild <- ds1$ssup4ch+ds1$ssup5ch+ds1$ssup6ch+ds1$ssup7ch
ds1$nssfam <- ds1$ssup4fam+ds1$ssup5fam+ds1$ssup6fam+ds1$ssup7fam
ds1$nssfr <- ds1$ssup4fr+ds1$ssup5fr+ds1$ssup6fr+ds1$ssup7ch

#Positive and Negative Affect
setnames(ds1, old=c('lb026a','lb026b','lb026c','lb026d','lb026e','lb026f','lb026g','lb026h','lb026i','lb026j','lb026k','lb026l','lb026m','lb026n'
                    ,'lb026o','lb026p','lb026q','lb026r','lb026s','lb026t','lb026u','lb026v','lb026w','lb026x','lb026y'),
         new=c('panas1','panas2','panas3','panas4','panas5','panas6','panas7','panas8','panas9','panas10','panas11','panas12','panas13'
               ,'panas14','panas15','panas16','panas17','panas18','panas19','panas20','panas21','panas22','panas23','panas24','panas25'))
print(ds1$panas1)
s <- which(colnames(ds1)=="panas1")
f <- which(colnames(ds1)=="panas25")


ds1$panas1<- plyr::mapvalues(ds1$panas1, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas2<- plyr::mapvalues(ds1$panas2, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas3<- plyr::mapvalues(ds1$panas3, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas4<- plyr::mapvalues(ds1$panas4, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas5<- plyr::mapvalues(ds1$panas5, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas6<- plyr::mapvalues(ds1$panas6, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas7<- plyr::mapvalues(ds1$panas7, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas8<- plyr::mapvalues(ds1$panas8, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas9<- plyr::mapvalues(ds1$panas9, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas10<- plyr::mapvalues(ds1$panas10, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas11<- plyr::mapvalues(ds1$panas11, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas12<- plyr::mapvalues(ds1$panas12, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas13<- plyr::mapvalues(ds1$panas13, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas14<- plyr::mapvalues(ds1$panas14, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas15<- plyr::mapvalues(ds1$panas15, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas16<- plyr::mapvalues(ds1$panas16, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas17<- plyr::mapvalues(ds1$panas17, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas18<- plyr::mapvalues(ds1$panas18, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas19<- plyr::mapvalues(ds1$panas19, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas20<- plyr::mapvalues(ds1$panas20, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas21<- plyr::mapvalues(ds1$panas21, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas22<- plyr::mapvalues(ds1$panas22, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas23<- plyr::mapvalues(ds1$panas23, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas24<- plyr::mapvalues(ds1$panas24, from=c(1,2,3,4,5), to =c(5,4,3,2,1))
ds1$panas25<- plyr::mapvalues(ds1$panas25, from=c(1,2,3,4,5), to =c(5,4,3,2,1))

#positive affect score c,d,f, g, h, k, p, q, t, u, v, x, y
#negative affect score a, b, e, i, j, l, m, n, o, r, s, w
ds2 <- within(ds1, {paffect <- panas3+panas4+panas6+panas7+panas8+panas11+panas17+panas20+panas21+panas22+panas24+panas25
                    naffect <- panas1+panas2+panas5+panas9+panas10+panas12+panas13+panas14+panas15+panas18+panas19+panas23
  })

#social effort/reward balance possibility but does not seem to be in the 2014 data set

#Ryff Well-being measure purspose in life scale 06-12 Q35
setnames(ds2, old=c('lb033a','lb033b','lb033c','lb033d','lb033e','lb033f','lb033g'),
         new=c('wellb1','wellb2','wellb3', 'wellb4','wellb5','wellb6','wellb7'))
#Reverse-code items 35 b, d, e, f
ds2$wellb2<- plyr::mapvalues(ds2$wellb2, from=c(1,2,3, 4, 5, 6), to=c(6,5,4,3,2,1))
ds2$wellb4<- plyr::mapvalues(ds2$wellb4, from=c(1,2,3, 4, 5, 6), to=c(6,5,4,3,2,1))
ds2$wellb5<- plyr::mapvalues(ds2$wellb5, from=c(1,2,3, 4, 5, 6), to=c(6,5,4,3,2,1))
ds2$wellb6<- plyr::mapvalues(ds2$wellb6, from=c(1,2,3, 4, 5, 6), to=c(6,5,4,3,2,1))

s <- which(colnames(ds2)=="wellb1")
f <- which(colnames(ds2)=="wellb7")
ds2$wellbtot <-rowSums(ds2[s:f], na.rm=TRUE)

summary(ds2$wellbtot)

m1 <- ifelse(is.na(ds2$wellb1)==TRUE, 1, 0)
m2 <- ifelse(is.na(ds2$wellb2)==TRUE, 1, 0)
m3 <- ifelse(is.na(ds2$wellb3)==TRUE, 1, 0)
m4 <- ifelse(is.na(ds2$wellb4)==TRUE, 1, 0)
m5 <- ifelse(is.na(ds2$wellb5)==TRUE, 1, 0)
m6 <- ifelse(is.na(ds2$wellb6)==TRUE, 1, 0)
m7 <- ifelse(is.na(ds2$wellb7)==TRUE, 1, 0)

ds2$missing<-m1+m2+m3+m4+m5+m6+m7
ds2$wellbm <- ifelse(ds2$missing<3, ds2$wellbtot/(7-ds2$missing), NA)
summary(ds2$wellbm)

ds3<- subset(ds2, select=c('lone1','lone2','lone3','lone4','lone5','lone6','lone7',
                                 'lone8','lone9','lone10','lone11',"lonetot","lonemean",
                                 'act1','act2','act3','act4','act5','act6','act7','act8',
                                 'act9','act10','act11','act12','act13','act14'
                                 ,'act15','act16','act17','act18','act19','act20','act21',
                                 "mixAct","cogAct","physAct","otherAct", "ActivityTot","lifesatm",
                                 'snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri',
                                 'closerel','socnetwork','pssspouse','psschild','pssfam','pssfr','nssspouse','nsschild','nssfam','nssfr',
                                 'paffect','naffect','wellbtot','wellbm'))

# Final step add the year to the  variables
vars<-colnames(ds3)
for (i in 1:length(ds3)){
  vars[i]<-paste0(vars[i],year_label)
}

colnames(data)<-vars
return(data)
  

