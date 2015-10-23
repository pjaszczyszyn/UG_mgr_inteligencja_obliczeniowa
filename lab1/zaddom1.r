setwd('E:\\UG\\io\\lab1')

phi1 <- read.csv(file="phi1.cnf", header=FALSE, sep="")
phi2 <- read.csv(file="phi2.cnf", header=FALSE, sep="")

 
 
fitness <- function(podstawienie, formula){
	counter <- 0
	for (i in 1:length(formula[,1])) {
		onerow <- formula[i,]
		for (j in 1:(length(formula)-1)) {
			v <- ifelse (onerow[,j] < 0, ifelse(podstawienie[abs(onerow[,j])] == 1,0,1), podstawienie[abs(onerow[,j])])
			if (v==1){
				counter <- (counter + 1)
				break
			}
		}
	}
	return(counter)
}

 
 
fitness(c(1,0,1,1), phi1)

fitness(sample(0:1,max(phi2, na.rm = TRUE),replace=T), phi2)
 
