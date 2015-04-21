rbind.columns <- function(x, y,type="all") {
  if(type=="all"){
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    
    x[, c(as.character(y.diff))] <- NA
    
    y[, c(as.character(x.diff))] <- NA
    
    return(rbind(x, y))
  }
  
  if(type=="match"){
    n.input1 <- ncol(input1)
    n.input2 <- ncol(input2)
    
    if (n.input2 < n.input1) {
      TF.names <- which(names(input2) %in% names(input1))
      column.names <- names(input2[, TF.names])
    } else {
      TF.names <- which(names(input1) %in% names(input2))
      column.names <- names(input1[, TF.names])
    }
    
    return(rbind(input1[, column.names], input2[, column.names]))
  }
}