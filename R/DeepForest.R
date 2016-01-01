#' Create a "deep" random forest for many-level classification
#' @description This function creates an ensemble of random forest models for many-level (c > 3) classification
#' 
#' @param y = A response vector of class factor or class character. 
#' @param x = a data frame of predictor variables 
#' @export




DeepForest <- function(y, x){
  # Load libraries needed
  library(parallel)
  library(randomForest)
    
  # checking to see if inputs are formatted correctly
  if(! is.data.frame(x)){
    stop("'x' must be of class data.frame")
  }
    
  if(! is.factor(y)){
    if(! is.character(y)){
      stop("y must be a character or, preferrably, a factor variable.")
    }
    warning("y is a character, converting to factor")
    y <- factor(y)
  }
  
  if(length(levels(y)) < 3){
    stop("y must have at least 3 levels. Try using randomForest() instead.")
  }
  
  
  # Initialize necessary variables  
  num_classes <- length(levels( y ))
  
  
  if(! .Platform$OS.type == "windows"){
    num_cores <- min(detectCores() , num_classes)
  }else{
    num_cores <- 1
  }
  
  
  class_levels <- levels(y)
  names(class_levels) <- class_levels
  
  # create a random forest model for each level
  forest_models <- mclapply(class_levels, function(L){
    y_tmp <- as.factor(y == L)
    
    randomForest(y = y_tmp, x = x)
    
  }, mc.cores = num_cores)
  

  # create a matrix of probabilities for each class and each variable
  forest_probs <- mclapply(forest_models, function(M){
    as.numeric(M$votes[ , "TRUE" ])
  }, mc.cores  = num_cores)
  
  forest_probs <- do.call(cbind, forest_probs)
  
  colnames(forest_probs) <- class_levels
  rownames(forest_probs) <- rownames(x)
  
  forest_probs <- as.data.frame(forest_probs, stringsAsFactors=F)
  
  # Logistic regression on model outputs to make final prediction
  logit_models <- mclapply(class_levels, function(L){
    y_tmp <- as.numeric(y == L)
    f <- paste("y_tmp ~ `", paste(colnames(forest_probs), collapse="` + `"), "`", sep="")
    f <- as.formula(f)
    glm(formula = f, data=cbind(forest_probs, y_tmp), family=binomial("logit"))
  }, mc.cores = num_cores)
  
  logit_probs <- do.call(cbind, lapply(logit_models, function(M) M$fitted.value))
  
  colnames(logit_probs) <- colnames(forest_probs)
  rownames(logit_probs) <- rownames(logit_probs)
  
  logit_probs <- data.frame(logit_probs, stringsAsFactors=F)
  
  # make final classifications
  
  if(nrow(logit_probs) > 10000){ # if you have a big dataset, parallelize
    # chunk data to split across num_cores cores
    rows <- floor(nrow(logit_probs) / num_cores)
    
    batches <- seq(1, nrow(logit_probs), by=rows)
    
    batches <- lapply(batches, function(B){
      logit_probs[ B:min(B + rows, nrow(logit_probs)) , ]
    })
        
    classifications <- mclapply(batches, function(B){
      apply(B, 1, which.max)
    }, mc.cores = num_cores)
        
    classifications <- do.call(c, classifications)
    
  }else{ # otherwise a simple apply will do
    classifications <- apply(logit_probs, 1, which.max)
  }
  
  classifications <- class_levels[ classifications ]
  names(classifications) <- rownames(logit_probs)
  
  # return results
  result <- list(fitted.values=classifications, 
                 forest=list(forest_models=forest_models, 
                             forest_probs=forest_probs),
                 logit=list(logit_models=logit_models,
                            logit_probs=logit_probs), 
                 y=y
                 )
  class(result) <- "DeepForest"
  
  return(result)
}




