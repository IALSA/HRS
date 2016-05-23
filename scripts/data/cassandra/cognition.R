#CED-S is actually in the cognition section

# Function that recodes and summarizes the ceds data
CEDS <- function(data){
  
  plyr::rename(data, replace = c(
    "d110"="ceds1",
    "d111"="ceds2",
    "d112"="ceds3",
    "d113"="ceds4",
    "d114"="ceds5",
    "d115"="ceds6",
    "d116"="ceds7",
    "d117"="ceds8",
    "d118"="ceds9"
  ))
  #Variables need to recoded 1 is the depressive answer for items 1,2,3; 5 means No, 8 is DK and 9 is refused
  ceds_to_recode <- c("ceds1", "ceds2","ceds3", "ceds5","ceds7","ceds8")
  #Variables need to recoded 5 is the depressive answer; 1 means yes the non-depressive answer, 8 is DK and 9 is refused
  ceds_to_recodepositive <- c("ceds4","ceds6","ceds9")
  
  reverse_coding_ceds <- function(data, variables){
    data[variables] <- plyr::mapvalues(data[,variables], from=c(1,5,8,9), to =c(1,0,NA,NA))
    return(data)
  }
  
  for(i in ceds_to_recode){
    data <- reverse_coding_wellbeing(data, variable = i) 
  }
  
  reverse_coding_ceds_positive <- function(data, variables){
    data[variables] <- plyr::mapvalues(data[,variables], from=c(1,5,8,9), to =c(0,1,NA,NA))
    return(data)
  }
  
  for(i in ceds_to_recodepositive){
    data <- reverse_coding_ceds_positive(data, variable = i) 
  }
  
  s <- which(colnames(data)=="ceds1")
  f <- which(colnames(data)=="ceds9")
  data$cedstotal <-rowSums(data[s:f], na.rm=TRUE)
  
  #Create a variable that indicates the number of missing per person.
  #data$missing <- (is.na(data$wellbeing_1) + is.na(data$wellbeing_2) + is.na(data$wellbeing_3) + is.na(data$wellbeing_4)+is.na(data$wellbeing_5)+is.na(data$wellbeing_6)+is.na(data$wellbeing_7))
  #data$wellbeing_mean <- ifelse(data$missing<3, data$wellbeing_total/(7-data$missing), NA)
  
  return(data)
}