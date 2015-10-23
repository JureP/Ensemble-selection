## precision/percentile funkcija

precisionPercentile <- function(napoved, ## vektor (verjetnostnih) napoved
								Y, ## vektor pravilnih prednosti
								q = 0.5 ## delez najvishih+najnizjih napovedi
								){
	## izracuna precsion na q-tem percentilu napovedi
	qL <- q/2
	qH <- 1 - q/2
	## spodnja meja vzetih napovedi
	spMeja <- quantile(napoved, qL)
	## zgornja meja vzetih napovedi
	zgMeja <- quantile(napoved, qH)
	
	Xizbor <- napoved[napoved > zgMeja | napoved < spMeja]
	Xizbor[Xizbor > zgMeja] <- 1
	Xizbor[Xizbor < spMeja] <- 0
	Yizbor <- Y[napoved > zgMeja | napoved < spMeja]
	retrived <- sum(Xizbor)
	precision <- sum(Xizbor & Yizbor) / retrived
	}
