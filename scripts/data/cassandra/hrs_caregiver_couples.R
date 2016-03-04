options(width=160)
rm(list=ls())
cat("\f")

#Read in merged data file created with 0_import_raw.R
ds0 <- readRDS("./data/Derived/unshared/hrs.rds")

#Create a variable that returns true if there is a valid spouse pphhidpn
# indicates those who have spouses who are also HRS participants.

hrsspouse <- ds0$phhidpn.04 > 0 | ds0$phhidpn.06 > 0 | ds0$phhidpn.08>0 |ds0$phhidpn.10 > 0 | ds0$phhidpn.12 > 0



everAD <- ds0$c272.10==1 | ds0$c272.12==1
everDem <- ds0$c273.12==1|ds0$c273.10==1
eMempr <- ds0$C069.04==1 | ds0$C069.06==1 | ds0$c069.08==1

ds0$dem <- ifelse(everAD==TRUE, "3", 
                  ifelse(everDem==TRUE, "2", 
                         ifelse(eMempr==TRUE, "1", "0")))

ds0$dem.12 <- ifelse(ds0$c272.12==1 | ds0$c273.12==1, "1",ds0$c273.12)
ds0$dem.10 <- ifelse(ds0$c272.10==1 | ds0$c273.10==1, "1",ds0$c273.10)
ds0$dem.08 <- ifelse(ds0$c069.08==1, "1", ds0$c069.08)
ds0$dem.06 <- ifelse(ds0$C069.06==1, "1",ds0$C069.06)
ds0$dem.04 <- ifelse(ds0$C069.04==1,"1",ds0$C069.04)

table(ds0$dem)


# selecting observations based on variable values

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
