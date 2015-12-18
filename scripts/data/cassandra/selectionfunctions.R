demographics <- function(ds, year_letterid, year_label){
  
  #create a for loop that remove the first character from variable/column name
  varnames<-colnames(ds)
  
  # grep("^O",x=varnames, perl=T, value=TRUE)
  
  
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==year_letterid){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
#   
#  gsub(pattern = "HHIDPN", replacement = "hhidpn", x = varnames)
#  gsub(pattern = "BIRTHYR", replacement = "birthyr", x = varnames)
#  gsub(pattern = "BIRTHMO", replacement = "birthmo", x = varnames)
#  gsub(pattern = "DEGREE", replacement = "degree", x = varnames)
#  gsub(pattern = "GENDER", replacement = "gender", x = varnames)
#  gsub(pattern = "HISPANIC", replacement = "hispanic", x = varnames)
#  gsub(pattern = "RACE", replacement = "race", x = varnames)
#  gsub(pattern = "STUDY", replacement = "study", x = varnames)

   
  varnames[1:100]
  # rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",
                    ifelse(varnames=="BIRTHYR","birthyr",
                           ifelse(varnames=="BIRTHMO","birthmo",
                                  ifelse(varnames=="DEGREE","degree",
                                         ifelse(varnames=="GENDER","gender",
                                                ifelse(varnames=="HISPANIC","hispanic",
                                                       ifelse(varnames=="RACE","race",
                                                              ifelse(varnames=="STUDY","study",varnames))))))))
#   
  #create a list of demographic variables
  # varnames2 <- varnames 
  colnames(ds)<-varnames2
  prescreenvars<-c("hhidpn","birthyr","birthmo","degree","gender","hispanic","race","study")
  coverscreen<-varnames2[which(substring(varnames2,1,1)=="A")]
  demographic<-varnames2[which(substring(varnames2,1,1)=="B")]
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
dstemp <-  demographics(ds = ds04, year_letterid = "J", year_label = "04")

physicalhealth<-function(ds,year_letterid,year_label){
  
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
  physhlth<-varnames2[which(substring(varnames2,1,1)=="C")]
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
  cog<-varnames2[which(substring(varnames2,1,1)=="D")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="E")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="F")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="G")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="H")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="I")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="J")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="K")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="L")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="M")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="N")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="P")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="Q")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="R")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="RC")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="S")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="T")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="U")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="V")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="W")]
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
  sectionvars<-varnames2[which(substring(varnames2,1,2)=="LB")]
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