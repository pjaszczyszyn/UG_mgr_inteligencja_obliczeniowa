# install.packages("genalg")
# library(genalg)
# install.packages("ggplot2")
# library(ggplot2)

setwd('E:\\UG\\io')


phi1 <- read.csv(file="phi1.cnf", header=FALSE, sep="")
phi2 <- read.csv(file="phi2.cnf", header=FALSE, sep="")

 
phi <- phi2
maxofphi <- max(phi2, na.rm = TRUE)
 
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
	return(-counter)
}

fitnessFunc <- function(x) fitness(x,phi)

 
 

GAmodel <- rbga.bin(size = maxofphi, popSize = 200, iters = 100,
mutationChance = 0.01, elitism = T, evalFunc = fitnessFunc)
summary(GAmodel, echo=TRUE)


iter<-100
animate_plot <- function() {
 for (i in seq(1, iter)) {
 temp <- data.frame(Iteracja = c(seq(1, i), seq(1, i)), Legenda = c(rep("Średnia",
 i), rep("Najlepsza", i)), WartoscFitness = c(-GAmodel$mean[1:i], -GAmodel$best[1:i]))
 pl <- ggplot(temp, aes(x = Iteracja, y = WartoscFitness, group = Legenda,
 colour = Legenda)) + geom_line() + scale_x_continuous(limits = c(0,
 iter)) + scale_y_continuous(limits = c(0, 110)) + geom_hline(y =
max(temp$WartoscFitness),
 lty = 2) + annotate("text", x = 1, y = max(temp$WartoscFitness) +
 2, hjust = 0, size = 3, color = "black", label = paste("Najlepsze rozwiązanie:",
 max(temp$WartoscFitness))) + scale_colour_brewer(palette = "Set1")
 print(pl)
 }
}

animate_plot()