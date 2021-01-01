# Ex. 1
# importer les donn?es
cath <- read.table('cathedral.txt', header = TRUE)

plot(cath$haut, cath$long, col = 'red', xlab = 'hauteur', ylab = 'longueur', pch = 11)

# Ex. 2
# Q1
plot(cath[cath$style == 'goth', ]$haut, cath[cath$style == 'goth', ]$long ,col='red',xlab='hauteur',ylab='longueur',pch=5)
points(cath[cath$style == 'rom', ]$haut, cath[cath$style == 'rom', ]$long ,col='blue',pch=4)
legend(15,180,c('gothique','romane'),col=c('red','blue'),pch=c(5,4))

#Q2
b <- cov(cath$long, cath$haut)/var(cath$haut)
a <- mean(cath$long) - mean(cath$haut)*b
abline(a,b)



# Ex. 3
# Q1
hist(cath$haut, freq = F)

# Q2
hist(cath$haut, freq = F, breaks = 2)
hist(cath$haut, freq = F, breaks=10)
hist(cath$haut, freq = F, breaks = 15)
hist(cath$haut,freq = F, breaks = 50)
hist(cath$haut, freq = F, breaks = 10)

hist(cath$haut, freq = F, breaks = c(40,  60,  70, 75, 80, 90, 110))
# Le bon nombre de sous-intervalles pour l'histogramme d?pend du nombre d'observations ainsi que de la densit? sous-jacente.
# Il faut trouver un compromis entre pas assez de pr?cision (trop peu de sous-intervalles) et une repr?sentation trop d?taill?e (plusieurs intervalles qui ne contiennent qu'une voir aucune observation)

# Q3
hist(cath$long, freq = F)
hist(cath$long, freq = F, breaks = 5)
hist(cath$long, freq = F, breaks = 10)
hist(cath$long, freq = F, breaks = 20)
hist(cath$long, freq = F, breaks = c(100, 200, 400, 500, 550, 700))

# souvent le choix par d?faut de la fonction hist pour la taille de la partition n'est pas si mauvais.

# Q4
hist(long,freq=F,breaks=5)
hist(cath$long, freq = F, breaks = 5)
curve(dnorm(x, mean(cath$long), sd(cath$long)), add = T, col='blue')
# l'histogramme est (? peu pr?s) unimodale et l?g?rement asym?trique. 
# Vu la faible taille d'?chantillon, la densit? gaussienne semble appropri?e (en tout cas on ne peut pas rejeter l'hypoth?se gaussienne avec certitude.)

# Ex. 4
plot(ecdf(cath$haut))
curve(pnorm(x, mean(cath$haut), sd(cath$haut)), add = T, col ='blue')

# L'ad?quation entre la fdr empirique et la fdr th?orique est tr?s bonne.

# Ex. 5

# Q1
qqplot(cath$long, cath$haut)
# les points s'alignent (? peu pr?s) sur une droite. Pour mieux voir, on standardise les donn?es pour pouvoir comparer ? la premi?re bissectrice :

long.st <- (cath$long - mean(cath$long))/sd(cath$long)
haut.st <- (cath$haut - mean(cath$haut))/sd(cath$haut)
qqplot(long.st,haut.st)
abline(0, 1, col='blue')

# Q2
qqnorm(haut.st)
abline(0,1, col = 'blue')

qqnorm(long.st)
abline(0,1, col = 'blue')

# La hauteur semble suivre une loi normale. Pour la longueur, les points s'?cartent un peu plus de la premi?re bissectrice, mais pas au point qu'on pourrait rejeter l'hypoth?se gaussienne.


# Ex. 6
# Q1
cath$rapport <- cath$long/cath$haut
boxplot(cath$long, cath$haut, cath$rapport, names = c('longueur','hauteur','rapport L/H'))
# On voit que les trois variables ont des ordres de grandeurs assez diff?rentes. 
# Les trois boxplots sont sym?triques.
# Seulement la variable rapport L/H contient une observations aberrantes.

# Q2
class(cath$style)
boxplot(long~style, data = cath)
# Beaucoup plus de variabilit? de la longueur des cath?drales gothiques que 
# des cath. romanes

boxplot(haut~style, data = cath)
# 2 Boxplots bien sym?triques. M?mes m?dianes. Mais beaucoup plus de variabilit? de la 
# hauteur des cath?drales gothiques que des cath. romanes

boxplot(rapport~style, data = cath)
# les 2 boxplots ont la m?me forme, mais pour les cath. romanes il est d?cal? vers le haut.
