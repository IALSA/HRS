
rename2004 <- function(ds1){

ds1 <- plyr::rename(ds1, replace = c(
    "lb531I" = "who_answered"
))  

#Rename for Activity variables 
# Slightly different wording but there is an almost equivalent group participation question
#Not including attendance at religious services, how often do you attend
#meetings or programs of groups, clubs, or organizations that you belong to?
#Response options 1 more than once a week to 6 never
ds1 <- plyr::rename(ds1, replace = c(
  "lb502" = "Group_participation"
))

#2004 Loneliness items available were only these labeled to match the equivalent items from other years
ds1 <- plyr::rename(ds1, replace = c(
  "lb504a" = "loneliness_1",
  "lb504b" = "loneliness_2",
  "lb504c" = "loneliness_3",
  "lb504d" = "loneliness_4"
))

attr(ds1$loneliness_1,"label") <- "Lack companionship"
attr(ds1$loneliness_2, "label") <- "Feel left out"
attr(ds1$loneliness_3, "label") <- "Feel isolated from others"
attr(ds1$loneliness_4, "label") <- "Feel in tune with people around"

#2004 life satisfaction (In 2004 the 7-point scale consistent with 2008 onwards was used)
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb505a" = "lifesatisfaction_1",
  "lb505b" = "lifesatisfaction_2",
  "lb505c" = "lifesatisfaction_3",
  "lb505d" = "lifesatisfaction_4",
  "lb505e" = "lifesatisfaction_5"
))

# social network '04 (wording is essentially the same)
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb507" = "snspouse",
  "lb510" = "snchild",
  "lb514" = "snfamily",
  "lb518" = "snfriends"
))

#number of close relationships '04
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb509" = "closespouse",
  "lb513" = "closechild",
  "lb517" = "closefam",
  "lb521" = "closefri"
))

# 2004 Social network contact note no social media question
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb512a" = "mtchild",
  "lb512b" = "spkchild",
  "lb512c" = "wrtchild",
  
  'lb516a' = "mtfam",
  'lb516b' = "spkfam",
  'lb516c' = "wrtfam",
  
  'lb520a' = "mtfriend",
  'lb520b' = "spkfriend",
  'lb520c' = "wrtfriend"
))

#2004 there was one less question per category
ds1 <- plyr::rename(x=ds1, replace = c(
  'lb508a' = 'ssup1sp',
  'lb508b' = 'ssup2sp',
  'lb508c' = 'ssup3sp',
  
  'lb508d' = "ssup5sp",
  'lb508e' = 'ssup6sp',
  'lb508f' = "ssup7sp",
  
  'lb511a' = 'ssup1ch',
  'lb511b' = 'ssup2ch',
  'lb511c' = 'ssup3ch',
  
  'lb511d' = 'ssup5ch',
  'lb511e' = 'ssup6ch',
  'lb511f' = 'ssup7ch',
  
  'lb515a' = 'ssup1fam',
  'lb515b' = 'ssup2fam',
  'lb515c' = 'ssup3fam',
  
  'lb515d' = 'ssup5fam',
  'lb515e' = 'ssup6fam',
  'lb515f' = 'ssup7fam',
  
  'lb519a' = 'ssup1fr',
  'lb519b' = 'ssup2fr',
  'lb519c' = 'ssup3fr',
  
  'lb519d' = 'ssup5fr',
  'lb519e' = 'ssup6fr',
  'lb519f' = 'ssup7fr'
))
#04 wellbeing questions
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb506g"="wellbeing_1",
  "lb506i"="wellbeing_5"
))
attr(ds1$wellbeing_1,"label") <- "Enjoy making plans for the future"
attr(ds1$wellbeing_5, "label") <- "Done all there is to do in life"
return(ds1)
}


