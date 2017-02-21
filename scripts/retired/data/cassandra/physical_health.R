
physhealthrename2004_2008 <- function(data){
  plyr::rename(data, replace = c(
    "c069"="memoryproblem",
    "c223"="Activity_vigorous",
    "c224"="Activity_moderate",
    "c225"="Activity_mild"
  ))
  
}

physhealthrename2010_2014 <- function(data){
  plyr::rename(data, replace = c(
    "c272"="AD",
    "c273"="dementia",
    "c223"="Activity_vigorous",
    "c224"="Activity_moderate",
    "c225"="Activity_mild"
  ))
}

#Physical activity questions in the physical health section (on a 1 to 7 scales)
physicalactivity <- function(data){
plyr::rename(data, replace = c(
  "c223"="Activity_vigorous",
  "c224"="Activity_moderate",
  "c225"="Activity_mild"
))
}