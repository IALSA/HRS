rename2008 <- function(ds1){
#Rename for Activity variables 
#2008 (two of the items were not included)
#Do activities with grandchildren, nieces/nephews, or neighborhood children (NA for 2008)
#watch television (NA for 2008)
ds1$activity_2 <- NA
ds1$activity_10 <- NA
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb001a" = "activity_1",
  "lb001b" = "activity_3",
  "lb001c" = "activity_4",
  "lb001d" = "activity_5",
  "lb001e" = "activity_6",
  "lb001f" = "activity_7",
  "lb001g" = "activity_8",
  "lb001h" = "activity_9",
  "lb001i" = "activity_11",
  "lb001j" = "activity_12",
  "lb001k" = "activity_13",
  "lb001l" = "activity_14",
  "lb001m" = "activity_15",
  "lb001n" = "activity_16",
  "lb001o" = "activity_17",
  "lb001p" = "activity_18",
  "lb001q" = "activity_19",
  "lb001r" = "activity_20"
))

#Loneliness scale 2008, 2010, 2012
ds1 <- plyr::rename(x=ds1, replace = c(
  "lb020a" = "loneliness_1", 
  "lb020b" = "loneliness_2", 
  "lb020c" = "loneliness_3", 
  "lb020d" = "loneliness_4", 
  "lb020e" = "loneliness_5", 
  "lb020f" = "loneliness_6", 
  "lb020g" = "loneliness_7", 
  "lb020h" = "loneliness_8", 
  "lb020i" = "loneliness_9", 
  "lb020j" = "loneliness_10", 
  "lb020k" = "loneliness_11" 
))

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
}