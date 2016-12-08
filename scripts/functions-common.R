# ---- define_lookup_function -------------------------------------------------
# Create function that inspects names and labels
names_labels <- function(ds){
  dd <- as.data.frame(ds)
  
  nl <- data.frame(matrix(NA, nrow=ncol(dd), ncol=2))
  names(nl) <- c("name","label")
  for (i in seq_along(names(dd))){
    # i = 2
    nl[i,"name"] <- attr(dd[i], "names")
    if(is.null(attr(dd[[i]], "label")) ){
      nl[i,"label"] <- NA}else{
        nl[i,"label"] <- attr(dd[,i], "label")  
      }
  }
  return(nl)
}
# names_labels(ds=oneFile)

