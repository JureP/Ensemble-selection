PlattScaling <- function(	X, ## vektor vrejetnostnih napovedi
							Y ## true class
						)
	## funkcija vrne model za skaliranje podatkov
	## uporaba: predict(calibModel, newdata = Data[colname 'x'], type = 'response')					
	{
	calibDataFrame <- data.frame(cbind(Y, X))
	colnames(calibDataFrame) <- c("y", "x")
	calibModel <- glm(y ~ x, calibDataFrame, family=binomial)
	return(calibModel)
}
