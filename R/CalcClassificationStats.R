
CalcClassificationStats <- function(predicted_probabilities, true_values){
  # Declare a function to calculate AUC
  Trapezoid <- function(x,y){
    len = length(x)
    w <- x[2:len] - x[1:(len-1)]
    h <- (y[2:len] + y[1:(len-1)])/2
    sum(h*w)
  }

  # error checking
  if( length(unique(true_values)) != 2){
    error("true_values must be a vector with two levels. Currently, 0/1 and TRUE/FALSE are supported")
  }

  # put true_values into a 0/1 vector
  true_values <- as.numeric(true_values)

  # get a list of unique thresholds to the thousandths place
  thresholds <- seq(0, 1, 0.01)

  # get TP, TN, FP, FN & calc other stats
  result <- lapply(thresholds, function(T){
    preds <- as.numeric(predicted_probabilities >= T)

	tp <- sum(preds == 1 & true_values == 1)
	fp <- sum(preds == 1 & true_values == 0)
	tn <- sum(preds == 0 & true_values == 0)
	fn <- sum(preds == 0 & true_values == 1)

	sens_recall <- tp / (tp + fn)
	spec_tnr <- tn / (fp + tn)
	prec_ppv <- tp / (tp + fp)
	npv <- tn / (tn + fn)
	fpr <- fp / (fp + tn)
	fdr <- fp / (fp + tp)
	fnr <- fn / (fn + tp)
	acc <- (tp + tn) / (tp + tn + fp + fn)
	f1 <- 2 * tp / (2 * tp + fp + fn)
	# mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

	data.frame(threshold=T, TP=tp, FP=fp, TN=tn, FN=fn,
	           Sens_Recall=sens_recall,
			   Spec_TNR=spec_tnr,
			   Prec_PPV=prec_ppv,
			   NPV=npv,
			   FPR=fpr,
			   FDR=fdr,
			   FNR=fnr,
			   Accuracy=acc,
			   F1=f1,
			   # MCC=mcc,
			   stringsAsFactors=FALSE)
  })

  result <- do.call(rbind, result)

  roc <- data.frame(xOneMinusSpec=1 - result$Spec_TNR,
                    ySensitivity=result$Sens_Recall,
                    stringsAsFactors=FALSE)
  auc <- Trapezoid(x=1 - roc$xOneMinusSpec, y=roc$ySensitivity)


  result <- list(stats=result, roc=roc, auc=auc)

  return(result)
}

