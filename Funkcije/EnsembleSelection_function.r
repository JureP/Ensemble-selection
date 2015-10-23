## Funkcija z baggingom
## okolje kjer sta skripti modelSelection_function.r in ProbEnsSelKrit_function.r

# OkoljeFunkcije <- 'Z:/JurePodlogar/Ensemble selection/Selection/Funkcije/Izboljsane'
# setwd(OkoljeFunkcije)
# source("modelSelection_function.r")
# source("ProbEnsSelKrit_function.r")



EnsembleSelection <- function(	X, # matrika verjetnostnih napovedi base learnerjev
								Y, # true class
								iter = 5L, # stevilo elementov ensemble (z vracanjem)
								bagFrac = 1L, # delez (nakljucnih) base learnerjev 
											# iz katerih izbira (na iteraciji)
								podIter = 1L, # kolikokrat izbere model na podmnozici
								kriterij = c('accu', 'p/rF', 'rmse') ## katere kriterije izbire
											## modelov naj uporabi:
												## accu: accuracy
												## p/rF: precision/recall F score
												## rmse: root mean square error
						){
	## funkcija iz base-learnerjev sestavi ensemble model tako da iter-krat 
	## nakljucno izbere bagFrac delez base-learnerjev in osnovnemu ensemblu
	## doda najboljsi ensemble (podIter-ih modelov) {glede na izobljsanje
	## osnovnega ensembla}
	N <- ncol(X)
	pred <- 0 * X
	weights <- rep(0L, N)
	sumWeights <- 0L
	potekUtezi <- matrix(NA, iter, N)
	while (sumWeights < iter){
		print(sumWeights)
		## izbor nakljucne podmnozice modelov
		sumWeights <- sumWeights + 1L
		bag <- sort(sample(1:N, N*bagFrac))
		Xb <- X[ , bag]
		## ze sestavljenim utezmi weights, z tezo podIter*sumWeights
		prob = as.matrix(X) %*% (weights/sumWeights)
		addWeights <- probEnsSelKrit(Xb, Y, iter = podIter, prob, podIter*sumWeights,
									kriterij = kriterij)
		# addWeights <- probEnsSel(Xb, Y, iter = podIter, prob, podIter * sumWeights)
		## dodajanje utezi
		weights[bag] <- weights[bag] + addWeights
		potekUtezi[sumWeights, ] <- weights/sumWeights
		#print(weights)
	}
	
	return(potekUtezi)
	#return(weights / sumWeights)
}



# t1 <- Sys.time()
# ensemble <- EnsembleSelection(X,Y, iter =10L, bagFrac = 1, podIter = 1L, kriterij = c('accu', 'r/pF', 'rmse'))
# t2 <- Sys.time()
# tail(ensemble,1)
# t2 - t1

