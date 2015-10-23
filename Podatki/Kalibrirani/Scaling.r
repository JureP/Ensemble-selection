	## from http://danielnee.com/tag/platt-scaling/
	
	
	reliability.plot <- function(obs, pred, bins=10, scale=T) {
  #  Plots a reliability chart and histogram of a set of predicitons from a classifier
  #
  # Args:
  #   obs: Vector of true labels. Should be binary (0 or 1)
  #   pred: Vector of predictions of each observation from the classifier. Should be real
  #       number
  #   bins: The number of bins to use in the reliability plot
  #   scale: Scale the pred to be between 0 and 1 before creating reliability plot
  require(plyr)
  library(Hmisc)
 
  min.pred <- min(pred)
  max.pred <- max(pred)
  min.max.diff <- max.pred - min.pred
  
  if (scale) {
    pred <- (pred - min.pred) / min.max.diff 
  }
  
  bin.pred <- cut(pred, bins)
  
  k <- ldply(levels(bin.pred), function(x) {
    idx <- x == bin.pred
    c(sum(obs[idx]) / length(obs[idx]), mean(pred[idx]))
  })
  
  is.nan.idx <- !is.nan(k$V2)
  k <- k[is.nan.idx,]  
  plot(k$V2, k$V1, xlim=c(0,1), ylim=c(0,1), xlab="Mean Prediction", ylab="Observed Fraction", col="red", type="o", main="Reliability Plot")
  lines(c(0,1),c(0,1), col="grey")
  subplot(hist(pred, xlab="", ylab="", main="", xlim=c(0,1), col="blue"), grconvertX(c(.8, 1), "npc"), grconvertY(c(0.08, .25), "npc"))
}


	kateri <- 1
	#reliability.plot(Y, X[,kateri])
	
	meja <- round(length(Y)/2)
	Y.calib <- Y[1:meja]
	Y.calib.pred <- X[1:meja, kateri]
	Y.test <- Y[1:meja]
	Y.test.pred <- X[1:meja, kateri]
	
	calib.data.frame <- data.frame(cbind(Y.calib, Y.calib.pred))
	colnames(calib.data.frame) <- c("y", "x")
	calib.model <- glm(y ~ x, calib.data.frame, family=binomial)
	calib.data.frame <- data.frame(Y.test.pred)
	colnames(calib.data.frame) <- c("x")
	Y.test.pred.calibrated <- predict(calib.model, newdata=calib.data.frame, type="response")
	
	#reliability.plot(Y.test, Y.test.pred.calibrated)
	
	hist(Y.test, Y.test.pred.calibrated)
	hist(Y, X[,kateri])
	