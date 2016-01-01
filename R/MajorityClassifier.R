MajorityClassifier <- function(y, x){
  
  tab <- table(y)
  
  majority <- names(tab)[ tab == max(tab) ]
  
  fitted <- rep(majority, nrow(x))
  
  if(! is.null(rownames(x))) names(fitted) <- rownames(x)
  
  result <- list(majority=majority,
                 fitted=fitted)
  
  class(result) <- "MajorityClassifier"
  
  result
}

