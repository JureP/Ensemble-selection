razdelitevPodatkov.r:
	razdeli podatke napovedi na ostanku (priblizno) na polovico (train, test)


razdelitevPodatkovIzbrani.r
	razdeli podatke IZBRANIH NAPOVEDI na ostanku (priblizno) na polovico (train, test)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modelLibrary.rds: 
	Vsebuje vse (verjetnostne) napovedi na ostanku (iz mape:Z:\Podatki\Predictions)

razdelitevPodatkov_list.rds:
	Vsebuje napovedi in true class razdeljen na dva (priblizno) enaka dela (train, test).
	Imena: xTrain, yTrain, xTest, yTest.

razdelitevPodatkovIzbrani_list.rds:
	Vsebuje IZBRANE NAPOVEDI in true class razdeljen na dva (priblizno) enaka dela (train, test).
	Imena: xTrain, yTrain, xTest, yTest.

ostanekTrainClassifier.RDS:
	Vsebuje true class.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


IZBRANE NAPOVEDI:
		BS
		JH
		JB_gbm1
		JL1
		JL5
		MF
		ME
		RK
		SVM1
		SVM2