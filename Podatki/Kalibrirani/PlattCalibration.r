	#####
	OkoljePodatki <- 'Z:/JurePodlogar/Ensemble selection/Podatki/Popravljeni'
	OkoljeSave <- 'Z:/JurePodlogar/Ensemble selection/Selection/Scaling'
	setwd(OkoljePodatki)
	podatki <- readRDS('razdelitevPodatkov_list.rds')

	X <- podatki$xTrain
	#Y <- as.numeric(podatki$yTrain)
	Y <- podatki$yTrain

	
	
	Xscale <- 0 * X
	names(Xscale) <- names(X)
	
	for (kateri in 1:ncol(X)){
		#reliability.plot(Y, X[,kateri])
	
		Ycalib <- Y
		Xcalib <- X[ , kateri]
				
		calibDataFrame <- data.frame(cbind(Ycalib, Xcalib))
		colnames(calibDataFrame) <- c("y", "x")
		calibModel <- glm(y ~ x, calibDataFrame, family=binomial)
		calibDataFrame <- data.frame(Xcalib)
		colnames(calibDataFrame) <- c("x")
		Xcalibrated <- predict(calibModel, newdata=calibDataFrame, type="response")
		Xscale[, kateri] <- Xcalibrated
		
		# par(mfrow = c(2,1))
		 hist(Xcalibrated, main = mean(Xcalibrated))
		# hist(X[, kateri], main = mean(X[, kateri]))
		# title(names(X)[kateri], outer=TRUE)
	}
	
	setwd(OkoljeSave)
	saveRDS(Xscale, 'XtrainScaled.rds')
	
	
	
	
	
	## calibrate, test (sepereted set)
		for (kateri in 1:ncol(X)){
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
		
		
		par(mfrow = c(2,1))
		hist(as.numeric(Y.test.pred.calibrated), main = mean(Y.test.pred.calibrated))
		hist(X[,kateri], main = mean(X[, kateri]))
		title(names(X)[kateri], outer=TRUE)
	}
