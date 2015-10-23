## mesana kriterijska funkcija
## izbere najboljsi model po kriterijih z tezo:
 										# rmse (1/4??)
										# accu (1/2??) 
										# precRec (1/4??)
## Vsak kriterij da modelu tocke. Tocke dobi najboljsih
## n modelov
 
modelSelection <- function(pred, ## verjetnostne napovedi (matrika)
							Y,	## true class
							n = 2, ## stevilo najboljsih modelov ki dobijo tocke
							nSk = c(2,1), ## vektor tock (dolzine n!!!)
							uporabaModelov = c('accu', 'precision', 'p/rF', 'rmse'), ## katere modele naj uporabi
											## accu: accuracy
											## precision: precision
											## p/rF: precision/recall F score
											## rmse: root mean square error
							utezKriterija = c(1, 1, 1, 1) ## utez vsakega kriterija 
													## dolzine 4!!! 
													## vrstni red: accu, precision, p/rF, rmse
						){
	## funkcija sprejme verjetnostne napovedi in true class in 
	## vrne kateri model daje najboljse napovedi po kriteriju 
	## glasovanja accu 1/2, rmse 1/4, precRecF 1/4
	
	##  accuracy in precision/recall
	thrsh <- colMeans(pred)
	napoved <- 0 * pred
	napoved[pred > thrsh] <- 1
	tocke <- rep(0, ncol(pred))
	## accuracy 
	if ('accu' %in% uporabaModelov){
		acc <- rep(NA, ncol(X))
		for (i in 1:length(thrsh)){
			cm <- table(napoved[ , i], Y)
			acc[i] <- (cm[1,1]  + cm[2,2])/sum(cm)
		}
		## tocke
		ac <- order(acc, decreasing = TRUE)
		tocke[ac[1:n]] = tocke[ac[1:n]] + utezKriterija[1]*nSk
	}
	## precision/recall F score  in precsion
	if ('p/rF' %in% uporabaModelov || 'precision' %in% uporabaModelov){
		retrived <- colSums(napoved)
		precision <- colSums(napoved & Y) / retrived
		## precision
		if ('precision' %in% uporabaModelov){
			prec <- order(precision, decreasing = TRUE)
			tocke[prec[1:n]] = tocke[prec[1:n]] + utezKriterija[2]*nSk
		}
		## precision/recall F score
		if ('p/rF' %in% uporabaModelov){
			recall <- colSums(napoved & Y) / sum(Y)
			F1 <- 2 * precision * recall / (precision + recall)
			## tocke
			F <- order(F1, decreasing = TRUE)
			tocke[F[1:n]] = tocke[F[1:n]] + utezKriterija[3]*nSk
		}
	}
	## root mean squared error
	if ('rmse' %in% uporabaModelov){
		errors <- sqrt(colSums(abs(pred - Y)))
		rse <- order(errors)
		tocke[rse[1:n]] = tocke[rse[1:n]] + utezKriterija[4]*nSk
	}
	
	return(tocke)	
}


# t1 <- Sys.time()
# rez <- modelSelection(X,Y, n = 2, rev(1:2), uporabaModelov = c('accu', 'p/rF'), utezKriterija = c(2,1,1))
# t2 <- Sys.time()
# t2-t1


