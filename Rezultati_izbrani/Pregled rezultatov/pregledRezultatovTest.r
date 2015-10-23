## okolje kjer so podatki
OkoljePodatki <- 'Z:/JurePodlogar/Ensemble selection/Ensemble/Podatki'
## okolje kjer sta funkciji PlattScaling
OkoljeFun  <- 'Z:/JurePodlogar/Ensemble selection/Ensemble/Funkcije'
## okoljeKamor se shranijo utezi
OkoljeWeights <- 'Z:/JurePodlogar/Ensemble selection/Ensemble/Rezultati_izbrani'

######################################################
## izbira ensemble selection modela izmed prirpavlenih:
imeEnsemble <- "2015-07-23_15-46-34_ensembleWeight_scaled_10_0.5_5_accu-precision-prF-rmse.rds"
setwd(OkoljeWeights)
## (matrika vseh utezi ensembla (skozi razlicno st iter: glej EnsembleSelection_function))
unscaled <- grepl('unscaled', imeEnsemble)
weights <- readRDS(imeEnsemble)


setwd(OkoljeFun)
source("PlattScaling_function.r")
source("precisionPercentile_function.r")
source("precisionPercentileMatrix_function.r")

## precision vsakega base-learnerja na testni mnozici:
setwd(OkoljePodatki)
podatki <- readRDS('razdelitevPodatkovIzbrani_list.rds')
## podatki train
Ytrain <- podatki$yTrain
Xtrain <- podatki$xTrain
## podatki test
Ytest <- podatki$yTest
Xtest <- podatki$xTest

## Platt scaling:
if (!unscaled){
	Xcalib <- NA * Xtest
	for(i in 1:ncol(Xtest)){
		print(i)
		## select model on train set
		model <- PlattScaling(as.vector(Xtrain[,i]) ,Ytrain)

		## use model on test set
		dataTest <- data.frame(Xtest[,i])
		names(dataTest) <- 'x'
		Xcalib[ , i] <- predict(model, newdata = dataTest, type = 'response')
	}
X <- Xcalib
}else{
	X <- Xtest
}


## precision/percentil base learnejev ########################################
precisionMatrix <- precisionPercentileMatirx(X, Ytest, percentil = seq(0.5,0.05,by = -0.01))


bestBase <- precisionMatrix[, c(1, 2, 8, 9)]
plot(bestBase[,1], type = 'l', ylim = c(0.527, 0.557), lty = 2)


for (i in 1:ncol(bestBase)){
	lines(bestBase[,i], col = i, lty = 2)
	print(names(bestBase)[i])
	# readline(prompt="Press [enter] to continue")
}

## precision/percentil ensembla ##############################################

## napovedi ensembla (za razlicno stevilo clenov ensembla):



## samo izbrani ensemble modeli (po ... iteracijah)
weightsSelected <- weights#[c(10, 20, 40, 60, 90, 100),]

## test za Platt scaled podatke
napovediEnsembla <- as.matrix(X) %*% t(weightsSelected)
precisionEnsembleMatrix <- precisionPercentileMatirx(napovediEnsembla, Ytest, percentil = seq(0.5,0.05,by = -0.01))


## test za unsceled podatke
# napovediEnsembla <- as.matrix(Xtest) %*% t(weightsSelected)
# precisionEnsembleMatrix <- precisionPercentileMatirx(napovediEnsembla, Ytest, percentil = seq(0.5,0.05,by = -0.01))


iter <- seq(1, ncol(precisionEnsembleMatrix), 1)
for (i in iter){
	lines(precisionEnsembleMatrix[,i], col = i)
	print(i)
	readline(prompt="Press [enter] to continue")
}














