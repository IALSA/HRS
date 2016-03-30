#################################################
#Below, recoding and summary score calculations based on the renamed variables.

#source(paste0(pathDir,"/scripts/data/cassandra/rename2014.R"))
#dslone14 <- rename2014(ds2)

#Function that returns recoded loneliness scale items and loneliness scale summary scores
full_loneliness_scale_recode <- function(data){
  
  #Reverse code items loneliness_1, 2, 3, and 5. 

  reverse_coding <- function(data2, variables){
  data2[variables] <- plyr::mapvalues(data2[,variables], from=c(1,2,3), to =c(3,2,1))
  return(data2)
  }

  variables_to_reverse_code <- c("loneliness_5")

  for(i in variables_to_reverse_code){
  data <- reverse_coding(data2=data, variable = i) 
  }

  #Check
  summary(data$loneliness_1)
  data[1:10,"loneliness_1"]

  #create a variable to indicate the number of missing loneliness scale items.
  data$lonemiss <- (is.na(data$loneliness_1)+is.na(data$loneliness_2)+is.na(data$loneliness_3)+is.na(data$loneliness_4)+is.na(data$loneliness_5)+is.na(data$loneliness_6)+is.na(data$loneliness_7)+is.na(data$loneliness_8)+is.na(data$loneliness_9)+is.na(data$loneliness_10)+is.na(data$loneliness_11))

  #check
  summary(data$lonemiss)

  s <- which(colnames(data)=="loneliness_1")
  f <- which(colnames(data)=="loneliness_11")

  data$loneliness_total <- rowSums(data[s:f], na.rm=TRUE)

  #Create the loneliness scale score only if there is less than 6 missing values
  #this is as per codebook instructions.
  data$loneliness_mean <- ifelse(data$lonemiss<6, data$loneliness_total/(11-data$lonemiss), NA)

  summary(data$loneliness_mean)
  return(data)

}

#Function that returns recoded loneliness scale items (3 items that are consistent across all years)
#Run this first to recode the first 3 items

loneliness_items_recode <- function(data){
  
#Reverse code items loneliness_1, 2, 3, and 5. 
  
  reverse_coding <- function(data, variables){
    data[variables] <- plyr::mapvalues(data[,variables], from=c(1,2,3), to =c(3,2,1))
    return(data)
  }
  
  variables_to_reverse_code <- c("loneliness_1","loneliness_2", "loneliness_3")
  
  for(i in variables_to_reverse_code){
    data <- reverse_coding(data, variable = i) 
  }

  #create a variable to indicate the number of missing loneliness scale items.
  data$lonemiss <- (is.na(data$loneliness_1)+is.na(data$loneliness_2)+is.na(data$loneliness_3))

  
  s <- which(colnames(data)=="loneliness_1")
  f <- which(colnames(data)=="loneliness_3")
  
  data$loneliness_total <- rowSums(data[s:f], na.rm=TRUE)
  
  #Create the loneliness scale score only if there is less than 6 missing values
  #this is as per codebook instructions.
  data$loneliness_mean <- ifelse(data$lonemiss<1, data$loneliness_total/(3-data$lonemiss), NA)
  
  summary(data$loneliness_mean)
  return(data)
  
}

#Testing functions

full_activity_scale_summaryscores <- function(data){
  s <- which(colnames(data)=="activity_1")
  f <- which(colnames(data)=="activity_21")
  data$ActivityTot <- rowSums(data[s:f], na.rm=FALSE)
  summary(data$ActivityTot)
  data$mixAct <- data$activity_1+data$activity_2+data$activity_3+data$activity_4+data$activity_5+data$activity_6+data$activity_7+data$activity_12+data$activity_21
  data$cogAct <- data$activity_9+data$activity_11+data$activity_13+data$activity_14
  data$physAct <- data$activity_19+data$activity_20+data$activity_15 
  data$otherAct <- data$activity_8+data$activity_10+data$activity_16+data$activity_17+data$activity_18
  
  return(data)
}

#dsact14R <- full_activity_scale_summaryscores(data = dslone14R)

#A function that creates a total score for life satisfaction and a mean score.
lifesatisfaction_summaryscores <- function(data){
  
  s <- which(colnames(data)=="lifesatisfaction_1")
  f <- which(colnames(data)=="lifesatisfaction_5")

  lifesatisfaction_total <- rowSums(data[s:f], na.rm=TRUE)

  data$lifesatmiss <- (is.na(data$lifesatisfaction_1)+is.na(data$lifesatisfaction_2)+is.na(data$lifesatisfaction_3)+is.na(data$lifesatisfaction_4)+is.na(data$lifesatisfaction_5))
  summary(data$lifesatmiss)

  #Calculate a life satisfaction score averaging across 5 items, less if some are missing.
  #Note that '06 scale is 1-6, '08 and onwards scale is 1-7.
  data$lifesatisfaction_mean <- ifelse(data$lifesatmiss<3, lifesatisfaction_total/(5-data$lifesatmiss), NA)
  summary(data$lifesatisfaction_mean)
  return(data)
}


#Q4-Q18 Social network - social integration-quality of relationships-social support
#This should work for all years

social_support_network_recode <- function(data){
  
  data$snspouse[data$snspouse==5] <- 0
  data$snchild[data$snchild==5] <-0
  data$snfamily[data$snfamily==5] <- 0
  data$snfriends[data$snfriends==5] <- 0
  data$snfriends[data$snfriends==7] <- NA

  data$close_relations <- data$closechild + data$closefam + data$closefri
  
  s <- which(colnames(data)=="snspouse")
  f <- which(colnames(data)=="snfriends")
  data$socnetwork <- rowSums(data[s:f])
  

  #Perceived social support(or relationship quality)
  #reverse code all social support items
  reverse_coding_socialsupport <- function(data, variables){
  data[variables] <- plyr::mapvalues(data[,variables], from=c(1,2,3,4), to =c(4,3,2,1))
  return(data)
  }

  socialsupport_to_reverse_code <- c('ssup1sp', 'ssup2sp', 'ssup3sp', "ssup4sp", "ssup5sp",'ssup6sp',"ssup7sp",'ssup1ch',
  'ssup2ch', 'ssup3ch', "ssup4ch", 'ssup5ch', 'ssup6ch', 'ssup7ch','ssup1fam','ssup2fam','ssup3fam', 'ssup4fam',
  'ssup5fam', 'ssup6fam', 'ssup7fam', 'ssup1fr', 'ssup2fr','ssup3fr','ssup4fr', 'ssup5fr','ssup6fr', 'ssup7fr')

  for(i in socialsupport_to_reverse_code){
  data <- reverse_coding_socialsupport(data, variables = i) 
  }

 
  data$positive_support_spouse <- data$ssup1sp+data$ssup2sp+data$ssup3sp
  data$positive_support_child <- data$ssup1ch+data$ssup2ch+data$ssup3ch
  data$positive_support_fam <- data$ssup1fam+data$ssup2fam+data$ssup3fam
  data$positive_support_fri <- data$ssup1fr+data$ssup2fr+data$ssup3fr

  data$negative_support_spouse <- data$ssup4sp+data$ssup5sp+data$ssup6sp+data$ssup7sp
  data$negative_support_child <- data$ssup4ch+data$ssup5ch+data$ssup6ch+data$ssup7ch
  data$negative_support_fam <- data$ssup4fam+data$ssup5fam+data$ssup6fam+data$ssup7fam
  data$negative_support_fri <- data$ssup4fr+data$ssup5fr+data$ssup6fr+data$ssup7ch
  
  return(data)
}

#Q4-Q18 Social network - social integration-quality of relationships-social support
#This is only for 2004 there is one less item in 2004 (a negative support item)

social_support_network_recode2004 <- function(data){
  
  data$snspouse[data$snspouse==5] <- 0
  data$snchild[data$snchild==5] <-0
  data$snfamily[data$snfamily==5] <- 0
  data$snfriends[data$snfriends==5] <- 0
  data$snfriends[data$snfriends==7] <- NA
  
  data$close_relations <- data$closechild + data$closefam + data$closefri
  
  s <- which(colnames(data)=="snspouse")
  f <- which(colnames(data)=="snfriends")
  data$socnetwork <- rowSums(data[s:f])
  
  
  #Perceived social support(or relationship quality)
  #reverse code all social support items
  reverse_coding_socialsupport <- function(data, variables){
    data[variables] <- plyr::mapvalues(data[,variables], from=c(1,2,3,4), to =c(4,3,2,1))
    return(data)
  }
  
  socialsupport_to_reverse_code <- c('ssup1sp', 'ssup2sp', 'ssup3sp', "ssup5sp",'ssup6sp',"ssup7sp",'ssup1ch',
                                     'ssup2ch', 'ssup3ch', 'ssup5ch', 'ssup6ch', 'ssup7ch','ssup1fam','ssup2fam','ssup3fam', 
                                     'ssup5fam', 'ssup6fam', 'ssup7fam', 'ssup1fr', 'ssup2fr','ssup3fr', 'ssup5fr','ssup6fr', 'ssup7fr')
  
  for(i in socialsupport_to_reverse_code){
    data <- reverse_coding_socialsupport(data, variables = i) 
  }
  
  
  data$positive_support_spouse <- data$ssup1sp+data$ssup2sp+data$ssup3sp
  data$positive_support_child <- data$ssup1ch+data$ssup2ch+data$ssup3ch
  data$positive_support_fam <- data$ssup1fam+data$ssup2fam+data$ssup3fam
  data$positive_support_fri <- data$ssup1fr+data$ssup2fr+data$ssup3fr
  
  data$negative_support_spouse <- data$ssup5sp+data$ssup6sp+data$ssup7sp
  data$negative_support_child <- data$ssup5ch+data$ssup6ch+data$ssup7ch
  data$negative_support_fam <- data$ssup5fam+data$ssup6fam+data$ssup7fam
  data$negative_support_fri <- data$ssup5fr+data$ssup6fr+data$ssup7ch
  
  return(data)
}
#socialds <- social_support_network_recode(dsact14R)


#PANAS only available for some years. 
panas_recode <- function(data){
  s <- which(colnames(data)=="panas_1")
  f <- which(colnames(data)=="panas_25")

  #reverse code all items
  reverse_coding_panas <- function(data, variables){
  data[variables] <- plyr::mapvalues(data[,variables], from=c(1,2,3,4,5), to =c(5, 4,3,2,1))
  return(data)
  }

  panas_to_reverse_code <- c("panas_1", "panas_2","panas_3","panas_4","panas_5","panas_6","panas_7","panas_8",
                           "panas_9", "panas_10", "panas_11", "panas_12", "panas_13", "panas_14","panas_15",
                           "panas_16", "panas_17", "panas_18", "panas_19", "panas_20", "panas_21", "panas_22", 
                            "panas_23", "panas_24", "panas_25")

  for(i in panas_to_reverse_code){
  data <- reverse_coding_panas(data, variable = i) 
  }

  #positive affect score c,d,f, g, h, k, p, q, t, u, v, x, y
  #negative affect score a, b, e, i, j, l, m, n, o, r, s, w
  data <- within(data, {positive_affect <- panas_3+panas_4+panas_6+panas_7+panas_8+panas_11+panas_17+panas_20+panas_21+panas_22+panas_24+panas_25
                    negative_affect <- panas_1+panas_2+panas_5+panas_9+panas_10+panas_12+panas_13+panas_14+panas_15+panas_18+panas_19+panas_23
  })
  
  return(data)

}

# Function that recodes and summarizes the wellbeing data
welling_scale_summarize <- function(data){
#Reverse-code items 35 b, d, e, f
  reverse_coding_wellbeing <- function(data, variables){
  data[variables] <- plyr::mapvalues(data[,variables], from=c(1,2,3,4,5,6), to =c(6, 5, 4,3,2,1))
  return(data)
  }

  wellbeing_to_reverse_code <- c("wellbeing_2", "wellbeing_4","wellbeing_5", "wellbeing_6")

  for(i in wellbeing_to_reverse_code){
  data <- reverse_coding_wellbeing(data, variable = i) 
  }

  s <- which(colnames(data)=="wellbeing_1")
  f <- which(colnames(data)=="wellbeing_7")
  data$wellbeing_total <-rowSums(data[s:f], na.rm=TRUE)

  #Create a variable that indicates the number of missing per person.
  data$missing <- (is.na(data$wellbeing_1) + is.na(data$wellbeing_2) + is.na(data$wellbeing_3) + is.na(data$wellbeing_4)+is.na(data$wellbeing_5)+is.na(data$wellbeing_6)+is.na(data$wellbeing_7))
  data$wellbeing_mean <- ifelse(data$missing<3, data$wellbeing_total/(7-data$missing), NA)
  
  return(data)
}

# Function that recodes and summarizes the wellbeing data with only two wellbeing items given in 2004
welling_scale_summarize2004 <- function(data){
  #Reverse-code items 35 b, d, e, f

  data[,"wellbeing_5"] <- plyr::mapvalues(data[,"wellbeing_5"], from=c(1,2,3,4,5,6), to =c(6, 5, 4,3,2,1))

  
  s <- which(colnames(data)=="wellbeing_1")
  f <- which(colnames(data)=="wellbeing_5")
  data$wellbeing_total <- data$wellbeing_1 + data$wellbeing_5
  
  #Create a variable that indicates the number of missing per person.
  data$missing <- (is.na(data$wellbeing_1)+is.na(data$wellbeing_5))
  data$wellbeing_mean <- data$wellbeing_total/2
  
  return(data)
}
#ds3<- subset(ds2, select=c('loneliness_1','loneliness_2','loneliness_3','loneliness_4','loneliness_5','loneliness_6','loneliness_7',
#                           'loneliness_8','loneliness_9','loneliness_10','loneliness_11',"loneliness_total","loneliness_mean",
#                           'activity_1','activity_2','activity_3','activity_4','activity_5','activity_6','activity_7','activity_8',
#                             activity_9','activity_10','activity_11','activity_12','activity_13','activity_14'
#                            ,'activity_15','activity_16','activity_17','activity_18','activity_19','activity_20','activity_21',
#                            "mixAct","cogAct","physAct","otherAct", "ActivityTot","lifesatisfaction_mean",
#                           'snspouse','snchild','snfamily','snfriends','closespouse','closechild','closefam','closefri',
#                           'closerel','socnetwork',"mtchild", "spkchild", "wrtchild", "mediachild", "mtfam", "spkfam", "wrtfam", "mediafam", 
#                            "mtfriend", "spkfriend", "wrtfriend", "mediafriend",'positive_support_spouse','positive_support_child','positive_support_fam','positive_support_fri',
#                           'negative_support_spouse','negative_support_child','negative_support_fam','negative_support_fri',
#                            'positive_affect','negative_affect','wellbeing_total','wellbeing_mean'))



  

