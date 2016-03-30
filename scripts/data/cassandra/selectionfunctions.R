#ds04 <- readRDS("./data/derived/unshared/RAND/h04f1a.rds")

# A function
preparing_variable_labels <- function(ds, year_letterid, year_label){
varnames<-colnames(ds)
#create a for loop that remove the first character from variable/column name  
for (i in 1:length(varnames)){
  if(substring(varnames[i],1,1)==year_letterid){
    varnames[i]<-substring(varnames[i],2)
  }else{
    varnames[i]<-varnames[i]
  }
}

#changes all the variable names to lower case
colnames(ds) <- tolower(varnames)
return(ds)

}


demographics <- function(ds, year_label){
  
varnames2<-colnames(ds)

  #create a list of demographic variables
  prescreenvars<-c("hhidpn","birthyr","birthmo","degree","gender","hispanic","race","study", "phhidpn")
  condition <- substring(varnames2,1,1)=="a"
  coverscreen<-varnames2[which(condition)]
  condition <- substring(varnames2,1,1)=="b"
  demographic<-varnames2[which(condition)]
  demovars<-c(prescreenvars,coverscreen,demographic)
  # browser()
  data<-ds[demovars]
  data[1:10,]
  
  #add the year to the prescreen variables
  prescvars<-colnames(data)
  for (i in 1:length(data)){
    prescvars[i]<-paste0(prescvars[i], ".", year_label)
  }
  
  colnames(data)<-prescvars
  return(data)
} 
#dstemp <-  demographics(ds = ds04, year_letterid = "J", year_label = "04")

physicalhealth <- function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  varnames[1:100]
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of demographic variables
  colnames(ds)<-varnames2
  id <- c("hhidpn")
  # browser()
  condition <- substring(varnames2,1,1)=="C" | substring(varnames2,1,1)=="c"
  physhlth<-varnames2[which(condition)]
  physhlthvars<-c(id,physhlth)
  
  data<-ds[physhlthvars]
  data[1:10,]
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

cognition<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  varnames[1:100]
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of demographic variables
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="D" | substring(varnames2,1,1)=="d"
  cog<-varnames2[which(condition)]
  cogvars<-c(id,cog)
  
  data<-ds[cogvars]
  data[1:10,]
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

children<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="E"|substring(varnames2,1,1)=="e"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]

  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

parents<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="F"|substring(varnames2,1,1)=="f"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

funlimitshelp<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="G"|substring(varnames2,1,1)=="g"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

housing<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="H"|substring(varnames2,1,1)=="h"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}



physicalfunction<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="I" | substring(varnames2,1,1)=="i"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

employment<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="J" | substring(varnames2,1,1)=="j"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

lastjob<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="K" | substring(varnames2,1,1)=="k"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

jobhistory<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="L" | substring(varnames2,1,1)=="l"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

disability<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="M" | substring(varnames2,1,1)=="m"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

Healthcare<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="N" | substring(varnames2,1,1)=="n"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

expectations<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="P" | substring(varnames2,1,1)=="p"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

assetsincome<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="Q" | substring(varnames2,1,1)=="q"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

assetchange<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="R" | substring(varnames2,1,1)=="r"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

repeatcognition<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="RC" | substring(varnames2,1,1)=="rc"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

widowdivorce<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="S" | substring(varnames2,1,1)=="s"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

willsinsurance<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="T" | substring(varnames2,1,1)=="t"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

assetverification<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="U" | substring(varnames2,1,1)=="u"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

modules<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="V" | substring(varnames2,1,1)=="v"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,section)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

socialsecurity<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,1)=="W" | substring(varnames2,1,1)=="w"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}

psychosocial<-function(ds,year_letterid,year_label){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(ds)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(ds)<-varnames2
  id<-c("hhidpn")
  condition <- substring(varnames2,1,2)=="LB" | substring(varnames2,1,2)=="lb"
  sectionvars<-varnames2[which(condition)]
  section<-c(id,sectionvars)
  
  data<-ds[section]
  
  
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],year_label)
  }
  
  colnames(data)<-vars
  return(data)
}
