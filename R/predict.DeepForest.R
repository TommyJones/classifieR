"predict.DeepForest" <- function(object, newdata){
  # Load libraries needed
  library(parallel)
  library(randomForest)
  
  # checking of inputs
  if(! inherits(object, "DeepForest")){
    stop("object not of class DeepForest")
  }
  
  if(is.null(object$forest$forest_models)){
    stop("no forest components in the object")
  }
  
  if(is.null(object$logit$logit_models)){
    stop("no logit models in object")
  }
  
  if(ncol(object$forest$forest_probs) != ncol(object$logit$logit_probs)){
    stop("forest and logit on object do not match")
  }
  
  if(ncol(object$forest$forest_probs) != length(levels(object$y)) |
       ncol(object$logit$logit_probs) != length(levels(object$y))){
    stop("levels of y do not match logit or forest models")
  }
  
  # declare some global variables
  y <- object$y
  
  num_classes <- length(levels(y))
  
  if(! .Platform$OS.type == "windows"){ # only one core for windows :(
    num_cores <- min(detectCores() , num_classes)
  }else{
    num_cores <- 1
  }
  
  class_levels <- levels(y)
  names(class_levels) <- class_levels
  
  # run random forest predictions  
  forest_preds <- mclapply(object$forest$forest_models, function(M){
    p <- predict(object = M, newdata=newdata, type="prob")
    p[ , "TRUE" ]
  }, mc.cores = num_cores)
    
  forest_preds <- do.call(cbind, forest_preds)

  forest_preds <- data.frame(forest_preds, stringsAsFactors=F)
  
  colnames(forest_preds) <- class_levels
  
  if(! is.null(rownames(newdata))){
    rownames(forest_preds) <- rownames(newdata)
  }else{
    rownames(forest_preds) <- 1:nrow(newdata)
  }
  
  # run logit predictions
  logit_preds <- mclapply(object$logit$logit_models, function(M){
    d <- forest_preds
    if( "" %in% colnames(d)){
      colnames(d)[ colnames(d) == "" ] <- paste("V", which(colnames(d) == ""),
                                                sep="")
    }
    
    p <- predict(object = M, newdata=d, type="response")
  }, mc.cores = num_cores)
  
  logit_preds <- do.call(cbind, logit_preds)

  logit_preds <- data.frame(logit_preds, stringsAsFactors=F)
  
  colnames(logit_preds) <- class_levels

  if(! is.null(rownames(newdata))){
    rownames(logit_preds) <- rownames(newdata)
  }else{
    rownames(logit_preds) <- 1:nrow(newdata)
  }
  
  
  
  # get final assignments
  if(nrow(logit_preds) > 10000){ # if you have a big dataset, parallelize
    # chunk data to split across num_cores cores
    rows <- floor(nrow(logit_preds) / num_cores)
    
    batches <- seq(1, nrow(logit_preds), by=rows)
    
    batches <- lapply(batches, function(B){
      logit_preds[ B:min(B + rows, nrow(logit_preds)) , ]
    })
    
    classifications <- mclapply(batches, function(B){
      apply(B, 1, which.max)
    }, mc.cores = num_cores)
    
    classifications <- do.call(c, classifications)
    
  }else{ # otherwise a simple apply will do
    classifications <- apply(logit_preds, 1, which.max)
  }
  
  classifications <- class_levels[ classifications ]
  names(classifications) <- rownames(logit_preds)
  
  
  # return result
  result <- list(fitted.values=classifications,
                 forest_preds=forest_preds,
                 logit_preds=logit_preds)
  
  return(result)
}