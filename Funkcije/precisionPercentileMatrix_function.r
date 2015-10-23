precisionPercentileMatirx <- function(	X , ## matrika (verjetnostnih) napoved
										Y,  ## true class
										percentil = seq(0.5,0.05,by = -0.01) ## percentil podatkov za
																			## katere naj izracuna prec.
						){
	## funkcija izracuna precision[percentil] za matriko s stolpci (verjentostnih) napovedi
	precisionMatrix <- data.frame(matrix(NA, length(percentil), ncol(X)))
	names(precisionMatrix) <- names(X)
	for(j in 1:ncol(X)){
		print(j)

		precPerc <- rep(NA, length(percentil))
		i <- 0
		for(q in percentil){
			i <- i + 1 
			precPerc[i] <- precisionPercentile(X[ , j], Ytest, q = q)
		}
		precisionMatrix[ ,j] <- precPerc
		#lines(precPerc, type = 'l', col = j)
	}
	return(precisionMatrix)
}


