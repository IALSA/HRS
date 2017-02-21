rename2014 <- function(ds1){

#
#Rename for Activity variables 2014 
#lb001u is PARTICIPATE COMMUNITY ARTS GRP
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb001a" = "activity_1",
  "lb001b" = "activity_2",
  "lb001c" = "activity_3",
  "lb001d" = "activity_4",
  "lb001e" = "activity_5",
  "lb001f" = "activity_6",
  "lb001g" = "activity_7",
  "lb001h" = "activity_8",
  "lb001i" = "activity_9",
  "lb001j" = "activity_10",
  "lb001k" = "activity_11",
  "lb001l" = "activity_12",
  "lb001m" = "activity_13",
  "lb001n" = "activity_14",
  "lb001o" = "activity_15",
  "lb001p" = "activity_16",
  "lb001q" = "activity_17",
  "lb001r" = "activity_18",
  "lb001s" = "activity_19",
  "lb001t" = "activity_20",
  "lb001u" = "activity_21"
))

#Activity variable labels (should be consistent across years)
attr(ds1$activity_1,"label") <- "Q01A OFTEN CARE ADULT"
attr(ds1$activity_2,"label") <- "OFTEN DO ACTIVITIES WITH GRANDCHILDREN"
attr(ds1$activity_3,"label") <- "OFTEN VOLUNTEER YOUTH"
attr(ds1$activity_4,"label") <- "OFTEN CHARITY WORK"
attr(ds1$activity_5,"label") <- "Q01E. OFTEN EDUCATION"
attr(ds1$activity_6,"label") <- "Q01F. OFTEN ATTEND SPORTS/SOCIAL/CLUB"
attr(ds1$activity_7,"label") <- "Q01G. OFTEN ATTEND NON RELIGIOUS ORGS"
attr(ds1$activity_8,"label") <- "Q01H. OFTEN PRAY PRIVATELY"
attr(ds1$activity_9,"label") <- "Q01I. OFTEN READ"
attr(ds1$activity_10,"label") <- "Q01J. OFTEN WATCH TELEVISION"
attr(ds1$activity_11,"label") <- "Q01K. OFTEN DO WORD GAMES"
attr(ds1$activity_12,"label") <- "Q01L. OFTEN PLAY CARDS AND GAMES"
attr(ds1$activity_13,"label") <- "Q01M. OFTEN DO WRITING"
attr(ds1$activity_14,"label") <- "Q01N. OFTEN USE COMPUTER"
attr(ds1$activity_15,"label") <- "Q01O. OFTEN MAINTENANCE/GARDENING"
attr(ds1$activity_16,"label") <- "Q01P. OFTEN BAKE OR COOK"
attr(ds1$activity_17,"label") <- "Q01Q. OFTEN SEW OR KNIT"
attr(ds1$activity_18,"label") <- "Q01R. OFTEN DO HOBBY"
attr(ds1$activity_19,"label") <- "Q01S. OFTEN PLAY SPORT/EXERCISE"
attr(ds1$activity_20,"label") <- "Q01T. OFTEN WALK FOR 20 MINS"
attr(ds1$activity_21,"label") <- "Q01U. PARTICIPATE COMMUNITY ARTS GRP"

#Renaming loneliness scale, Note that the numbering of specific items occasionally changes from year to year.
#2014
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb019a" = "loneliness_1", 
  "lb019b" = "loneliness_2", 
  "lb019c" = "loneliness_3", 
  "lb019d" = "loneliness_4", 
  "lb019e" = "loneliness_5", 
  "lb019f" = "loneliness_6", 
  "lb019g" = "loneliness_7", 
  "lb019h" = "loneliness_8", 
  "lb019i" = "loneliness_9", 
  "lb019j" = "loneliness_10", 
  "lb019k" = "loneliness_11" 
))

# rename Life satisfaction '14

ds1 <- plyr::rename(x=ds1, replace = c(
  "lb002a" = "lifesatisfaction_1",
  "lb002b" = "lifesatisfaction_2",
  "lb002c" = "lifesatisfaction_3",
  "lb002d" = "lifesatisfaction_4",
  "lb002e" = "lifesatisfaction_5"
))

#Q4-6 spouse/partner
#Q7-10 children
#Q11-14 family
#Q15-18 friends

#composition of social network '14 (note the variable numbers change)
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb003" = "snspouse",
  "lb006" = "snchild",
  "lb010" = "snfamily",
  "lb014" = "snfriends"
))

#number of close relationships  '14, spouse excluded from total score
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb005" = "closespouse",
  "lb009" = "closechild",
  "lb013" = "closefam",
  "lb017" = "closefri"
))

#social network contact excluding spouse '14

ds1 <- plyr::rename(x=ds1, replace = c(
  "lb008a" = "mtchild",
  "lb008b" = "spkchild",
  "lb008c" = "wrtchild",
  "lb008d" = "mediachild",
  'lb012a' = "mtfam",
  'lb012b' = "spkfam",
  'lb012c' = "wrtfam",
  'lb012d' = "mediafam",
  'lb016a' = "mtfriend",
  'lb016b' = "spkfriend",
  'lb016c' = "wrtfriend",
  'lb016d' = "mediafriend"
))

#Social support items '14
ds1 <- plyr::rename(x=ds1, replace = c(
  'lb004a' = 'ssup1sp',
  'lb004b' = 'ssup2sp',
  'lb004c' = 'ssup3sp',
  'lb004d' = "ssup4sp",
  'lb004e' = "ssup5sp",
  'lb004f' = 'ssup6sp',
  'lb004g' = "ssup7sp",
  'lb007a' = 'ssup1ch',
  'lb007b' = 'ssup2ch',
  'lb007c' = 'ssup3ch',
  'lb007d' = "ssup4ch",
  'lb007e' = 'ssup5ch',
  'lb007f' = 'ssup6ch',
  'lb007g' = 'ssup7ch',
  'lb011a' = 'ssup1fam',
  'lb011b' = 'ssup2fam',
  'lb011c' = 'ssup3fam',
  'lb011d' = 'ssup4fam',
  'lb011e' = 'ssup5fam',
  'lb011f' = 'ssup6fam',
  'lb011g' = 'ssup7fam',
  'lb015a' = 'ssup1fr',
  'lb015b' = 'ssup2fr',
  'lb015c' = 'ssup3fr',
  'lb015d' = 'ssup4fr',
  'lb015e' = 'ssup5fr',
  'lb015f' = 'ssup6fr',
  'lb015g' = 'ssup7fr'
))

#Positive and Negative Affect (This scale was changed substantially in 2008 from the 2006 measure it would be difficult to compare these)
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb026a"="panas_1",
  "lb026b"="panas_2",
  "lb026c"="panas_3",
  "lb026d"="panas_4",
  "lb026e"="panas_5",
  "lb026f"="panas_6",
  "lb026g"="panas_7",
  "lb026h"="panas_8",
  "lb026i"="panas_9",
  "lb026j"="panas_10",
  "lb026k"="panas_11",
  "lb026l"="panas_12",
  "lb026m"="panas_13",
  "lb026n"="panas_14",
  "lb026o"="panas_15",
  "lb026p"="panas_16",
  "lb026q"="panas_17",
  "lb026r"="panas_18",
  "lb026s"="panas_19",
  "lb026t"="panas_20",
  "lb026u"="panas_21",
  "lb026v"="panas_22",
  "lb026w"="panas_23",
  "lb026x"="panas_24",
  "lb026y"="panas_25"
))

#Ryff Well-being measure purspose in life scale 
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb033a"="wellbeing_1",
  "lb033b"="wellbeing_2",
  "lb033c"="wellbeing_3",
  "lb033d"="wellbeing_4",
  "lb033e"="wellbeing_5",
  "lb033f"="wellbeing_6",
  "lb033g"="wellbeing_7"
))

attr(ds1$wellbeing_1,"label") <- "Enjoy making plans for the future"
attr(ds1$wellbeing_2, "label") <- "Daily activities seem trivial"
attr(ds1$wellbeing_3, "label") <- "Active in carrying out own plans"
attr(ds1$wellbeing_4, "label") <- "No good sense trying to accomplish"
attr(ds1$wellbeing_5, "label") <- "Done all there is to do in life"
attr(ds1$wellbeing_6, "label") <- "Live life one day at a time"
attr(ds1$wellbeing_7, "label") <- "Have a sense of direction in life"

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
  "ds156" = "msnaming2",
  "ds157" = "mspresident",
  "ds158" = "msvp"
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

return(ds1)

}

