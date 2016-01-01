"predict.MajorityClassifier" <- function(object, newdata){
  
  fitted <- rep(object$majority, nrow(newdata))
  
  if(! is.null(rownames(x))) names(fitted) <- rownames(x)
  
  return(fitted)
}