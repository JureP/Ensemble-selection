## Kriterij: abs error 
## Funkcija z baggingom
## okolje kjer so funkcije kriterijev za izbiro base learneja v ensemble
library(nnet)

# OkoljeKriterij <- 'Z:/JurePodlogar/Ensemble selection/Selection/Funkcije/Izboljsane/Kriteriji'
# setwd(OkoljeKriterij)
# source("modelSelection_function.r")


probEnsSelKrit <- function(	X, ## library of model predictions (matrix)
						Y, ## class (vector)
						iter = 20L, # number of iterations
						prob = rep(0, nrow(X)), # probability predictions from existing model
						sumWeights = 0, # weight of initWeights 
						kriterij = c('accu', 'p/rF', 'rmse'), ## katere kriterije izbire
											## modelov naj uporabi:
												## accu: accuracy
												## p/rF: precision/recall F score
												## rmse: root mean square error
						n = 2L, ## stevilo najboljsih modelov, ki dobi tocke
						nSk = c(2,1), ## lestvica tock (dolzine n)
						utezKriterija = c(1, 1, 1, 1) ## utez vsakega kriterija 
													## dolzine 4!!! 
													## vrstni red: accu, precision, p/rF, rmse
						){
  ## funkcija izvede iter iteracij dodajanja base learner modelov iz matrike X ensemble modelu, ki 
  ## je karakteriziran s pomocjo zacetnih verjetnostnih napovedi (prob) in teze le-teh (sumWeights)
  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- prob * sumWeights
  niter       <- 0L
  while(niter < iter) {
	  #print(niter)
	  
	  niter         <- niter + 1L
      sumWeights    <- sumWeights + 1L
	  pred          <- (pred + X) * (1L / sumWeights)
	  ### kriterij izbire 
	  tocke <- modelSelection(pred, Y, n = n, nSk = nSk, uporabaModelov = kriterij, utezKriterija = utezKriterija)
	  best          <- which.is.max(tocke)
	  ###
      weights[best] <- weights[best] + 1L
      pred          <- pred[, best] * sumWeights
	  #print(weights)
	  }
  return(weights / niter)
}

# t1 <- Sys.time()
# probEnsSelKrit(X,Y, iter = 10L, kriterij = c('accu','p/rF','rmse'))
# t2 <- Sys.time()
# t2-t1

