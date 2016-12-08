#Create a series of functions that prepare the cognitive variables to be included.

#Total the Mental Status Questions
#Recode the variables first

  data$msmonth[data$msmonth==5|data$msmonth==8] <- 0
  data$msmonth[data$msmonth==9] <- NA
  
  data$msdate[data$msdate==5|data$msdate==8] <- 0 
  data$msdate[data$msdate==9] <- NA
  data$msyear[data$msyear==5|data$msyear==8] <- 0
  data$msyear[data$msyear==9] <- NA
  data$msday[data$msday==5|data$msday==8]<-0
  data$msday[data$msday==9]<-NA
  
  data$msnaming[data$msnaming==5|data$msnaming==8] <- 0 
  data$msnaming[data$msnaming==9] <- NA
  data$msnaming2[data$msnaming2==5|data$msnaming2==8] <- 0
  data$msnaming2[data$msnaming2==9] <- NA
  
  data$mspresident[data$mspresident==5|data$mspresident==8]<-0
  data$mspresident[data$mspresident==9]<-NA
  data$msvp[data$msvp==5|data$msvp==8]<-0
  data$msvp[data$msvp==9]<-NA
  
