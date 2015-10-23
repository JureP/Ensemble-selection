## razdelitev podatkov na del kjer se izbira ensemble in kjer se testirajo (samo izbrani)
OkoljePodatki <- 'Z:/JurePodlogar/Ensemble selection/Ensemble/Podatki'

setwd(OkoljePodatki)
modelLibrary <- readRDS('modelLibrary.rds')
modelLibrary <- modelLibrary[,-c(4:14, 16:18, 20)]
y <- readRDS('ostanekTrainClassifier.RDS')

meja <- round(length(y)/2)

## del na katerem se izbira elemente enselmbla
xTrain <- modelLibrary[1:meja, ]
yTrain <- y[1:meja]
## del na katerem se testira ensemble
xTest <- modelLibrary[-(1:meja), ]
yTest <- y[-(1:meja)]

razdelPod <- list('xTrain' = xTrain, 'yTrain' = yTrain, 'xTest' = xTest, 'yTest' = yTest)


saveRDS(razdelPod, 'razdelitevPodatkovIzbrani_list.rds')
