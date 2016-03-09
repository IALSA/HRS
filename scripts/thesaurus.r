# this file contains code examples of what I've learned working in this repository

# ---- renaming_with_plyr ------------------------
# given the dataset, 
(ds <- data.frame(
  id           = c(111,111,111,222,222,222, 333, 333),
  A        = c(0, 1, 2, 0, 1, 2, 0, 1),
  B = c(0, 0, 1, 0, 0, NA, 0, 1)
))

# rename one variable
ds <- plyr::rename(x=ds,replace = c("A" = "newA"))
ds

# rename multiple variable at the same time
ds <- plyr::rename(x=ds,replace = c("A" = "newA", "B" = "newB"))
ds

# ---- recode_values_in_multiple_variables_with_plyr --------------
# given the dataset, 
(ds <- data.frame(
  id = c(111,111,111,222,222,222, 333, 333),
  A  = c(0, 1, 2, 0, 1, 2, 0, 1),
  B  = c(0, 0, 1, 0, 0, NA, 0, 1),
  C  = c(1, 1, 1, 1, 1, 1, 2, 2)
))

ds$A; str(ds$A)
ds["A"]; str(ds["A"])
ds[,"A"]; str(ds[,"A"])

reverse_coding <- function(data, variable){
  data[,variable] <- plyr::mapvalues(data[,variable], from=c(0,1), to =c(999,888))
  return(data)
} 

variables_to_reverse_code <- c("A","B", "C")
for(i in variables_to_reverse_code){
  ds <- reverse_coding(data=ds, variable = i) 
}
ds



