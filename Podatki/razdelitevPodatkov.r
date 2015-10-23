## razdelitev podatkov na del kjer se izbira ensemble in kjer se testirajo
OkoljePodatki <- 'Z:\JurePodlogar\Ensemble selection\Ensemble\Podatki'

setwd(OkoljePodatki)
modelLibrary <- readRDS('modelLibrary.rds')
y <- readRDS('ostanekTrainClassifier.RDS')

meja <- round(length(y)/2)

## del na katerem se izbira elemente enselmbla
xTrain <- modelLibrary[1:meja, ]
yTrain <- y[1:meja]
## del na katerem se testira ensemble
xTest <- modelLibrary[-(1:meja), ]
yTest <- y[-(1:meja)]

razdelPod <- list('xTrain' = xTrain, 'yTrain' = yTrain, 'xTest' = xTest, 'yTest' = yTest)

saveRDS(razdelPod, 'razdelitevPodatkov_list.rds')
