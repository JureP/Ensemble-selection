## skripta v s katero se izvede ensemble selection

## okolje kjer so podatki
OkoljePodatki <- 'Z:/JurePodlogar/Ensemble selection/Ensemble/Podatki'
## Okolje kjer je izvedbaES_function.r
OkoljeES <- 'Z:/JurePodlogar/Ensemble selection/Ensemble/'
## okolje kamor naj se shranijo utezi modelov
OkoljeSave <- 'Z:/JurePodlogar/Ensemble selection/Ensemble/Rezultati_izbrani'


setwd(OkoljeES)
source('izvedbaES_function.r')

## base-learner library
setwd(OkoljePodatki)
podatki <- readRDS('razdelitevPodatkovIzbrani_list.rds')
Y <- podatki$yTrain
X <- podatki$xTrain


## izbira parametrov (gelje EnsembleSelection_funtion.r (razen Platt scaling))
scaled = FALSE ## ali uporabi Platt scaling
stIter <- 100 ## stevilo iteracij
bagF <- 0.5 ## bagging fraction
stPodIter <- 5 ## st iteracij na nakljucno izbrani podmnozici
izbKrit <- c('accu', 'precision')#,'p/rF', 'rmse') ## kriterij izbire 


ensemble <- izvedbaES(X,Y, scaled, stIter, bagF, stPodIter, izbKrit)

###################### shranjevanje
setwd(OkoljeSave)
## ime: datum_cas_ensembleWeights_parametri(scaled ,iter, bagFrac, podIter, kriteriji)
izbKrit[izbKrit == 'p/rF'] <- 'prF'
kriterijiIzbire <- paste0(izbKrit, collapse = '-')
## ce scaled
if (scaled == TRUE){
	parametri <- paste0('scaled_', stIter, '_', bagF, '_', stPodIter, '_', kriterijiIzbire)
}
## ce ni scaled
if (scaled == FALSE){
	parametri <- paste0('unscaled_', stIter, '_', bagF, '_', stPodIter, '_', kriterijiIzbire)
}
imeModel <- paste0(Sys.Date(), '_', strftime(Sys.time(), format  = '%H-%M-%S'), '_ensembleWeight_',parametri , '.rds')
imeRuntime <- paste0(Sys.Date(), '_', strftime(Sys.time(), format  = '%H-%M-%S'), '_runTime.rds')
saveRDS(ensemble, imeModel)

q('no')
