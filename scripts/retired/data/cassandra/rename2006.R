rename2006 <- function(ds1){
#Rename for Activity variables 
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb001a" = "newspaper_read",
  "lb001b" = "hobby",
  "lb001c" = "vacation_US",
  "lb001d" = "vacation_abroad",
  "lb001e" = "daytrip",
  "lb001f" = "use_internet",
  "lb001g" = "own_cellphone",
  "lb001h" = "none_of_above",
  "lb002" = "group_participation"
))

#2006 Loneliness scale only had three questions
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb020a" = "loneliness_1",
  "lb020b" = "loneliness_2",
  "lb020c" = "loneliness_3"
))

#attr(ds1$lb020a,"label") <- "Lack companionship"
#attr(ds1$lb020b, "label") <- "Feel left out"
#attr(ds1$lb020c, "label") <- "Feel isolated from others"


# 2012, 2010, 2008, 2006 Life satisfaction (Note that the scale used in 2006 is different)
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb003a" = "lifesatisfaction_1",
  "lb003b" = "lifesatisfaction_2",
  "lb003c" = "lifesatisfaction_3",
  "lb003d" = "lifesatisfaction_4",
  "lb003e" = "lifesatisfaction_5"
))

attr(ds1$lifesatisfaction_1,"label") <- "Q02A. LIFE IS CLOSE TO IDEAL"
attr(ds1$lifesatisfaction_2,"label") <- "Q02B. CONDITIONS OF LIFE ARE EXCELLENT"
attr(ds1$lifesatisfaction_3,"label") <- "Q02C. SATISFIED WITH LIFE"
attr(ds1$lifesatisfaction_4,"label") <- "Q02D. HAVE IMPORTANT THINGS IN LIFE"
attr(ds1$lifesatisfaction_5,"label") <- "Q02E. CHANGE NOTHING IF LIVED LIFE OVER"

#composition of social network '12, '10, '08, '06,
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb004" = "snspouse",
  "lb007" = "snchild",
  "lb011" = "snfamily",
  "lb015" = "snfriends"
))

#number of close relationships '12, '10, '08, '06 
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb006" = "closespouse",
  "lb010" = "closechild",
  "lb014" = "closefam",
  "lb018" = "closefri"
))

# '06, 08, 10, 12
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb009a" = "mtchild",
  "lb009b" = "spkchild",
  "lb009c" = "wrtchild",
  
  'lb013a' = "mtfam",
  'lb013b' = "spkfam",
  'lb013c' = "wrtfam",
  
  'lb017a' = "mtfriend",
  'lb017b' = "spkfriend",
  'lb017c' = "wrtfriend"
))

#Social support items '12, '10, '08, '06
ds1 <- plyr::rename(x=ds1, replace = c(
  'lb005a' = 'ssup1sp',
  'lb005b' = 'ssup2sp',
  'lb005c' = 'ssup3sp',
  'lb005d' = "ssup4sp",
  'lb005e' = "ssup5sp",
  'lb005f' = 'ssup6sp',
  'lb005g' = "ssup7sp",
  'lb008a' = 'ssup1ch',
  'lb008b' = 'ssup2ch',
  'lb008c' = 'ssup3ch',
  'lb008d' = "ssup4ch",
  'lb008e' = 'ssup5ch',
  'lb008f' = 'ssup6ch',
  'lb008g' = 'ssup7ch',
  'lb012a' = 'ssup1fam',
  'lb012b' = 'ssup2fam',
  'lb012c' = 'ssup3fam',
  'lb012d' = 'ssup4fam',
  'lb012e' = 'ssup5fam',
  'lb012f' = 'ssup6fam',
  'lb012g' = 'ssup7fam',
  'lb016a' = 'ssup1fr',
  'lb016b' = 'ssup2fr',
  'lb016c' = 'ssup3fr',
  'lb016d' = 'ssup4fr',
  'lb016e' = 'ssup5fr',
  'lb016f' = 'ssup6fr',
  'lb016g' = 'ssup7fr'
))

#06-12 Q35
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb035a"="wellbeing_1",
  "lb035b"="wellbeing_2",
  "lb035c"="wellbeing_3",
  "lb035d"="wellbeing_4",
  "lb035e"="wellbeing_5",
  "lb035f"="wellbeing_6",
  "lb035g"="wellbeing_7"
))

#########Cognitive data rename

#Self-Memory rating
ds1 <- plyr::rename(x=ds1, replace = c(
  "d101"="srmemory",
  "d102"="srmemoryp"
))
attr(ds1$srmemory, "label") <- "Rate memory"
attr(ds1$srmemoryp, "label") <- "Rate past memory"

#Word list recall
ds1 <- plyr::rename(x=ds1, replace = c(
  "d104" = "listassi",
  "d174" = "wrectoti",
  "d175" = "wrecwrongim",
  "d177" = "wrecnoneim",
  "d184" = "wrectotd",
  "d185" = "wrecwrongd",
  "d187" = "wrecnoned"
))

#Mental Status given to those 65 and older only
ds1 <- plyr::rename(x=ds1, replace = c(
  "d151" = "msmonth",
  "d152" = "msdate",
  "d153" = "msyear",
  "d154" = "msday",
  "d155" = "msnaming1",
  "d156" = "msnaming2",
  "d157" = "mspresident",
  "d158" = "msvp"
))

#Vocabulary
ds1<- plyr::rename(x=ds1, replace = c(
  "d159" = "voclist",
  "d161" = "vocab1",
  "d163" = "vocab2",
  "d165" = "vocab3",
  "d167" = "vocab4",
  "d169" = "vocab5"
))

#numeracy
ds1 <- plyr::rename(x=ds1, replace = c(
  "d178" = "numer1",
  "d179" = "numer2",
  "d180" = "numer3"
))

#Working memory
ds1 <- plyr::rename(x=ds1, replace = c(
  "d142" = "serial71",
  "d143" = "serial72",
  "d144" = "serial73",
  "d145" = "serial74",
  "d146" = "serial75"
))
}