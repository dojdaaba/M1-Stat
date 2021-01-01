# -*- coding: utf-8 -*-
# Exercice 1

cath <- read.table('cathedral.txt', header = TRUE)
head(cath)

##Q1
length(cath$style == 'rom')

##Q2
cath[cath$style == 'rom', ]$nom

##Q3
cath$haut_metre <- cath$haut*0.3048
cath$long_metre <- cath$long*0.3048

##Q4
mean(cath$long_metre)
mean(cath[cath$style == 'rom', ]$long_metre)

##Q5
mean(cath[cath$style == 'rom', ]$long_metre)

##Q6
cath$rapport <- cath$long_metre/cath$haut_metre

##Q7
summary(cath)



# Exercice 2

# Q1
obs <- rnorm(20,10,1)
mean(obs)
mean(obs)-10

var(obs)
var(obs)-1
# en exécutant ces trois lignes une dizaine de fois, on observe toujours un petit écart

# Q2
obs <- rnorm(20,10,sqrt(10))
mean(obs)
mean(obs)-10

var(obs)
var(obs)-10
# l'écart est parfois bien plus important. C'est normal, car la variance des observations est bien plus élevée.


# # Exercie 3

# Q1
n <- 1000
k <- 1
obs <- rchisq(n,k)
mean(obs) - k

k <- 10
obs <- rchisq(n,k)
mean(obs) - k

k <- 100
obs <- rchisq(n,k)
mean(obs) - k

k <- 1000
obs <- rchisq(n,k)
mean(obs) - k

# en exécutant plusieurs les lignes ci-dessus, on observe que les moyennes des 4 lois de khi-deux sont toujours près de k

# Q2
k <- 1
obs <- rchisq(n,k)
var(obs) - 2*k

k <- 10
obs <- rchisq(n,k)
var(obs) - 2*k

k <- 100
obs <- rchisq(n,k)
var(obs) - 2*k

k <- 1000
obs <- rchisq(n,k)
var(obs) - 2*k

# pour les 4 lois de khi-deux, les variances empiriques tournent toujours autour de 2*k, mais pour des grandes valeurs de k on observe beaucoup de variabilité.
# En fait, pour des échantillons de plus grandes taille (n= 10^6) l'estimation de la variance empirique est plus stable (et près de 2*k)



# Complement (Douba) 

### Illustration de la convergence en loi
par(mfrow = c(1, 2))
n <- 10
Nrep <- 200
Y <- rep(0, Nrep)


for (k in 1:Nrep){
  X <- rnorm(n, 0, 1)
  Y[k] <- sqrt(n/2)*(mean(X^2)-1)
}

y <- sort(Y)

### Illustration TCL
f=(1:Nrep)/Nrep
#f=ecdf(y)

plot(y,f,'s',col="blue",xlab="", ylab="", main = 'n=10')
lines(y, pnorm(y, mean= 0, sd= 1),col="red") 



n <- 100
Nrep <- 200
Y <- rep(0, Nrep)


for (k in 1:Nrep){
  X <- rnorm(n, 0, 1)
  Y[k] <- sqrt(n/2)*(mean(X^2)-1)
}

y <- sort(Y)

### Illustration TCL
f=(1:Nrep)/Nrep
#f=ecdf(y)

plot(y,f,'s',col="blue",xlab="", ylab="", main = 'n=100')
lines(y, pnorm(y, mean= 0, sd= 1),col="red") 

