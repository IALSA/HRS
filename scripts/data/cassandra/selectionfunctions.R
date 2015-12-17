demographics<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  varnames[1:100]
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",
                    ifelse(varnames=="BIRTHYR","birthyr",
                           ifelse(varnames=="BIRTHMO","birthmo",
                                  ifelse(varnames=="DEGREE","degree",
                                         ifelse(varnames=="GENDER","gender",
                                                ifelse(varnames=="HISPANIC","hispanic",
                                                       ifelse(varnames=="RACE","race",
                                                              ifelse(varnames=="STUDY","study",varnames))))))))
  
  #create a list of demographic variables
  colnames(datayr)<-varnames2
  prescreenvars<-c("hhidpn","birthyr","birthmo","degree","gender","hispanic","race","study")
  coverscreen<-varnames2[which(substring(varnames2,1,1)=="A")]
  demographic<-varnames2[which(substring(varnames2,1,1)=="B")]
  demovars<-c(prescreenvars,coverscreen,demographic)
  
  data<-datayr[demovars]
  data[1:10,]
  
  #add the year to the prescreen variables
  prescvars<-colnames(data)
  for (i in 1:length(data)){
    prescvars[i]<-paste0(prescvars[i],yr)
  }
  
  colnames(data)<-prescvars
  return(data)
}

physicalhealth<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  varnames[1:100]
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of demographic variables
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  physhlth<-varnames2[which(substring(varnames2,1,1)=="C")]
  physhlthvars<-c(id,physhlth)
  
  data<-datayr[physhlthvars]
  data[1:10,]
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

cognition<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  varnames[1:100]
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of demographic variables
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  cog<-varnames2[which(substring(varnames2,1,1)=="D")]
  cogvars<-c(id,cog)
  
  data<-datayr[cogvars]
  data[1:10,]
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

children<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="E")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]

  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

parents<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="F")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

funlimitshelp<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="G")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

housing<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="H")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}



physicalfunction<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="I")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

employment<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="J")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

lastjob<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="K")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

jobhistory<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="L")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

disability<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="M")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

Healthcare<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="N")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

expectations<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="P")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

assetsincome<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="Q")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

assetchange<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="R")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

repeatcognition<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="RC")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

widowdivorce<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="S")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

willsinsurance<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="T")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

assetverification<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="U")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

modules<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="V")]
  section<-c(id,section)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

socialsecurity<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,1)=="W")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}

psychosocial<-function(datayr,yrletter,yr){
  
  #create a for loop that takes the first letter off 
  varnames<-colnames(datayr)
  
  for (i in 1:length(varnames)){
    if(substring(varnames[i],1,1)==yrletter){
      varnames[i]<-substring(varnames[i],2)
    }else{
      varnames[i]<-varnames[i]
    }
  }
  
  #rename essential variables with names for consistency
  varnames2<-ifelse(varnames=="HHIDPN","hhidpn",varnames)
  
  #create a list of variables to include
  colnames(datayr)<-varnames2
  id<-c("hhidpn")
  sectionvars<-varnames2[which(substring(varnames2,1,2)=="LB")]
  section<-c(id,sectionvars)
  
  data<-datayr[section]
  
  #add the year to the  variables
  vars<-colnames(data)
  for (i in 1:length(data)){
    vars[i]<-paste0(vars[i],yr)
  }
  
  colnames(data)<-vars
  return(data)
}