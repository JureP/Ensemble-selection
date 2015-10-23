## okolje kjer sta funkciji probEnsSel in bagGreed
OkoljeFun  <- 'Z:/JurePodlogar/Ensemble selection/Ensemble/Funkcije'

setwd(OkoljeFun)
## function EnsembleSelection (bagging)
source("EnsembleSelection_function.r")
## function probEnsSelKrit (used in EnsembleSelection) 
source("ProbEnsSelKrit_function.r")
## functio modelSelection (used in probEnsSelKrit)
source("modelSelection_function.r" )
## function for Platt scalint
source("PlattScaling_function.r")


## izbira parametrov funkcije EnsembleSelection + Platt scalint Yes/No 
izvedbaES <- function(	X, ## library of base-learners
						Y, ## true class
						scaled = FALSE, ## ali uporabi Platt scaling
						stIter = 100, ## stevilo iteracij
						bagF =  0.5, ## bagging fraction
						stPodIter = 5, ## st iteracij na nakljucno izbrani podmnozici
						izbKrit = c('accu', 'precision','p/rF', 'rmse') ## kriterij izbire 
	## funkcija izvede ensemble selection s funkcijo EnsembleSelection in uporabi parametre 
	## iter =stIter, bagFrac = bagF, podIter = stPodIter, kriterij = izbKrit
	## scaled = TRUE uporabi Platt scaling na verjetnostnih napovedih,
	## scaled = FALSE uporabi neskalirane napovedi	
		){
	
	## Platt scaling
	if (scaled == TRUE){
		Xcalib <- NA * X
		for(i in 1:ncol(X)){
			print(i)
			## select model on train set
			model <- PlattScaling(as.vector(X[,i]) ,Y)

			## use model on test set
			dataTest <- data.frame(X[,i])
			names(dataTest) <- 'x'
			Xcalib[ , i] <- predict(model, newdata = dataTest, type = 'response')
		}
	}


	## Brez: Platt scaling
	if(scaled == FALSE){
		t1 <- Sys.time()
		ensemble <- EnsembleSelection(X, Y, iter =stIter, bagFrac = bagF, podIter = stPodIter, kriterij = izbKrit)
		t2 <- Sys.time()
		tp <- t2 - t1
	}
	## S: Platt scaling
	if (scaled == TRUE){
		t1 <- Sys.time()
		ensemble <- EnsembleSelection(Xcalib, Y, iter =stIter, bagFrac = bagF, podIter = stPodIter, kriterij = izbKrit)
		t2 <- Sys.time()
		tp <- t2 - t1
	}

	ensemble <- data.frame(ensemble)
	names(ensemble) <- names(X)

	#finalEnsemble <- as.vector(tail(ensemble,1))
	#names(finalEnsemble) <- names(X)
	#plot(finalEnsemble)
	
	return(ensemble)
}


