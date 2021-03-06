#' ---	
#' title: "Devoir n°1"	
#' author: "ALOUI Mohamed, JAFUNO Douba"	
#' date: "11/25/2018"	
#' output: html_document	
#' ---	
#' 	
#' 	
#knitr::opts_chunk$set(echo = TRUE)	
#' 	
#' 	
#' #Les données	
#' 	
#' 	
#' On considère un tableau de données sur des personnes anorexiques qui ont été suivies pendant quelque temps. Certains patients ont suivi une thérapie (soit une thérapie familiale 'FT', soit la thérapie Cognitive Behavioural Treatment 'CBT'), d'autres n'ont suivi aucun traitement (c'est le groupe de contrôle 'Cont'). Le tableau contient pour chaque individu le type de la thérapie et son poids au début et à la fin de l'étude (en livres).	
#' 	
#' Tout d'abord, avant de commencer l'étude nous changeons l'unité de mesure en kilogramme.	
#' 	
#' 	
ano=read.table('DonneesAnorexie.txt',header=TRUE)	
ano$pre=ano$pre*0.453   # conversion du livre au kilogramme	
ano$post=ano$post*0.453 # conversion du livre au kilogramme	
#' 	
#' 	
#' ##########################################################################################################################################################################################################  	
#' #Question 1	
#' 	
#' 	
a=c(29,26,17)	
names(a)=c("CBT","Cont","FT")	
a=as.data.frame(a)	
colnames(a)="Effectif"	
library("knitr")	
kable(a)	
#' 	
#' 	
#' Les 3 groupes de patients sont constitués d'effectifs différents mais du même ordre. Pour l'instant, **rien n'indique que les groupes ne sont pas homogènes**.	
#' 	
#' 	
#calcul de la moyenne	
moy=aggregate(ano$pre,by=list(ano$trait),FUN="mean")	
sd=aggregate(ano$pre,by=list(ano$trait),FUN="sd")	
stat=data.frame(Groupe=moy$Group.1,Moyenne=moy$x,Ecart_type=sd$x)	
kable(stat)	
#' 	
#' 	
#' Au regard de l'analyse de la moyenne et de la variance du poids des patients dans chacun des groupes, nous pouvons conclure que les groupes présentent un centrage  semblable et que l'écart-type (qui constitue un indicateur de la dispersion autour de la moyenne) est similaire.	
#' 	
#' **Représentation des boxplot du poids initial des patients sur les 3 groupes**	
#' 	
#' 	
boxplot(ano$pre~ano$trait)	
#' 	
#' 	
#' Pour compléter l'analyse, nous choisissons de représenter les boxplots des poids paitents pour chacun de groupes (nous n'avons pas représenté d'histoigrammes au vu du nombre trop restreint de patients). Nous pouvons en déduire que la dispersion n'est pas la même dans chacun des groupes. En effet, les écarts inter quantiles (qui correspondent à la largeur des rectangles) ne sont pas égaux. De plus, les médianes, indicateur qui n'est pas influencé par les valeurs extrêmes, sont différentes et les quantiles également. Nous pouvons en conclure que les groupes ne sont pas parfaitement homogènes mais qu'ils sont de même assez représentatifs de la population globale. 	
#' 	
#' 	
#' **Comparaison croisée des quantiles empiriques du poids initial des patients sur les 3 groupes**	
#' 	
#' 	
#' 	
par(mfrow=c(1,3))	
qqplot(ano[ano$trait=='Cont',2],ano[ano$trait=='CBT',2],xlab="Quantiles Cont",ylab="Quantiles CBT")	
abline(0,1)	
qqplot(ano[ano$trait=='Cont',2],ano[ano$trait=='FT',2],xlab="Quantiles Cont",ylab="Quantiles FT")	
abline(0,1)	
qqplot(ano[ano$trait=='FT',2],ano[ano$trait=='CBT',2],xlab="Quantiles FT",ylab="Quantiles CBT")	
abline(0,1)	
#' 	
#' 	
#' Pour soutenir notre propos, nous représentons une comparaison croisée des quantiles empiriques de chacun des groupes. Nous observons que les observations représentées par des cercles ne sont pas superposées à la bissectrice. Cela met en avant les différences des distributions empiriques de chacuns des groupes. Toutefois, ces différences ne sont pas alarmantes étant donné le nombre d'observations.	
#' 	
#' ##########################################################################################################################################################################################################	
#' 	
#' #Question 2	
#' 	
#' Vu que les trois groupes ne sont pas identiques au début de l'étude, il est incohérent de comparer simplement le poids des patients à la fin de l'étude.	
#' 	
#' Il se peut par exemple, que la moyenne de deux groupes à la fin de l'étude soit la même, et que pour un groupe elle ai beaucoup augmenté par rapport au debut de l'étude alors que pour l'autre elle est la même; cela signifie qu'un traitement à peut être été concluant alors que l'autre non, et ce malgrès une moyenne de poids pour les deux groupes égale a la fin de l'étude.	
#' 	
#' Nous souhaitons mesurer l'efficacité de traitements prescrits dans le cadre de symptomes anorexyques. Cette efficacité ce quantifie par l'évolution du poids d'un patient entre la fin du traitement et le début du traitement. Ainsi, nous allons nous intéresser à cette évolution pour quantifier l'efficacité des traitements.	
#' 	
#' 	
#' ##########################################################################################################################################################################################################  	
#' 	
#' #Question 3	
#' 	
#' **Intéressons nous maintenant au groupe Cont:**	
#' 	
ano$evo=ano$post-ano$pre	
#On sélectionne les patients du groupe Cont	
anoCont=ano[ano$trait=="Cont",]	
median=median(anoCont$evo)	
moye=mean(anoCont$evo)	
taux_de_succès=sum(anoCont$evo>0)/length(anoCont$evo)	
kable(data.frame("Mediane"=median,"Moyenne"=moye,"Taux de succès"=taux_de_succès))	
	
#' 	
#' 	
#' Nous nous intéressons à l'évolution des poids des patients du groupe Cont sur la durée du traitement. Nous observons qu'en moyenne, les patients du groupe Cont ont perdu 0,2 kilos sur la période du traitement. La médiane de l'évolution des poids est également négative. Definissons le taux de succès du groupe par la part des patients ayant pris du poids (strictement). Le taux de succès du groupe Cont est d'environ 42%. Lanoréxie s'est donc aggravée pour la majorité des individus du groupe Cont.	
#' 	
#' ##########################################################################################################################################################################################################  	
#' #Question 4	
#' 	
#' **Intéressons nous maintenant au groupe FT:**	
#' 	
ano$evo=ano$post-ano$pre	
#On sélectionne les patients du groupe FT	
anoFT=ano[ano$trait=="FT",]	
median=median(anoFT$evo)	
moye=mean(anoFT$evo)	
taux_de_succès=sum(anoFT$evo>0)/length(anoFT$evo)	
kable(data.frame("Mediane"=median,"Moyenne"=moye,"Taux de succès"=taux_de_succès))	
#' 	
#' 	
#' 	
#' Nous pouvons remarquer qu'en moyenne, les individus du groupe FT ont vu leur poids augmenter de plus de 3 kilos. L'indicateur de la médiane est encore plus favorable au traitement FT puisqu'il indique une évolution positive de 4 kilos. Le taux de succès de ce groupe est de plus de 76% pour ce groupe.	
#' 	
#' 	
#' **Intéressons nous maintenant au groupe CBT:**	
#' 	
#On sélectionne les patients du groupe CBT	
anoCBT=ano[ano$trait=="CBT",]	
median=median(anoCBT$evo)	
moye=mean(anoCBT$evo)	
taux_de_succès=sum(anoCBT$evo>0)/length(anoCBT$evo)	
kable(data.frame("Mediane"=median,"Moyenne"=moye,"Taux de succès"=taux_de_succès))	
#' 	
#' 	
#' 	
#' Nous pouvons remarquer qu'en moyenne, les individus du groupe CBT ont vu leur poids augmenter de plus de 1,3 kilos. L'indicateur de la médiane est moins favorable au traitement CBT puisqu'il indique une évolution positive de 0,63 kilos. Le taux de succès de ce groupe est de plus de 62% pour ce groupe.	
#' 	
#' 	
#' Nous en concluons que le traitement le plus efficace est le traitement FT. Pour appuyer notre propos, nous représentons les boxplot des évolutions de poids pour chacun des groupes.	
#' 	
#' 	
#' **Représentation des boxplots décrivant l'évolution du poids des patients en fonction du groupe**	
#' 	
boxplot(ano$evo~ano$trait)	
#' 	
#' 	
#' Ce graphique confirme que le traitement FT est le meilleur. En effet, il nous confirme que le traitement ne provoque pas de pertes de poids trop importantes, même pour des cas isolés.	
#' 	
#' ##########################################################################################################################################################################################################  	
#' #Question 5	
#' 	
#' Nous représentons les distributions empiriques de l'évolution des poids pour chacun des groupes en normalisant l'échelle de l'axe des abscisses pour faciliter la comparaison entre les graphiques.	
#' 	
#' **Représentation des histogrammes empiriques de l'évolution du poids sur les 3 groupes**	
#' 	
par(mfrow=c(1,3))	
#Histogramme du groupe CBT	
hist(ano$evo[ano$trait=="CBT"],xlim=c(-10,10), freq=FALSE,main="")	
abline(v=0,col='blue')	
#Histogramme du groupe Cont	
hist(ano$evo[ano$trait=="Cont"],xlim=c(-10,10), freq=FALSE,main="")	
abline(v=0,col='blue')	
#Histogramme du groupe FT	
hist(ano$evo[ano$trait=="FT"],xlim=c(-10,10), freq=FALSE,main="")	
abline(v=0,col='blue')	
#' 	
#' 	
#' Nous pouvons dès maintenant conjecturer que la distribution de la loi normale ne pourra pas s'ajuster correctement au groupe Cont. En effet, la distribution n'est pas symétrique et une loi exponentielle pourrait certainement mieux s'ajuster qu'une loi normale. Pour les deux autres groupes, nous observons un mode empirique centré et une dispersion plutôt symétrique. Toutefois, dans les deux cas, la distribution empirique met plus de poids aux valeurs supérieures au mode qu'inférieures. Le test de Kolomogorv Smirnov sera particulièrement utile pour apporter une conclusion à ces deux groupes.	
#' 	
#' Le test de Kolmogorov Smirnov ne peut pas être implémenté si des doublons sont présents dans les effectifs. De ce fait, pour les groupes présentons des doublons, nous introduisont un bruit dans les observations (qui peut s'apparenter à l'incertitude liée à la mesure). Nous pouvons de ce fait implémenter le test de Kolmogorvo Smirnov pour tous les groupes.	
#' 	
#' 	
#set.seed(1) Sert à utiliser un générateur pseudo aléatoire	
	
set.seed(1)	
ks.test(jitter((ano$evo[ano$trait=="CBT"]-mean(ano$evo[ano$trait=="CBT"]))/sd(ano$evo[ano$trait=="CBT"])),"pnorm")	
set.seed(1)	
ks.test((ano$evo[ano$trait=="Cont"]-mean(ano$evo[ano$trait=="Cont"]))/sd(ano$evo[ano$trait=="Cont"]),"pnorm")	
set.seed(1)	
ks.test((ano$evo[ano$trait=="FT"]-mean(ano$evo[ano$trait=="FT"]))/sd(ano$evo[ano$trait=="FT"]),"pnorm")	
set.seed(1)	
ks.test(jitter((ano$pre[ano$trait=="CBT"]-mean(ano$pre[ano$trait=="CBT"]))/sd(ano$pre[ano$trait=="CBT"])),"pnorm")	
set.seed(1)	
ks.test(jitter((ano$pre[ano$trait=="Cont"]-mean(ano$pre[ano$trait=="Cont"]))/sd(ano$pre[ano$trait=="Cont"])),"pnorm")	
set.seed(1)	
ks.test(jitter((ano$pre[ano$trait=="FT"]-mean(ano$pre[ano$trait=="FT"]))/sd(ano$pre[ano$trait=="FT"])),"pnorm")	
set.seed(1)	
ks.test(jitter((ano$post[ano$trait=="CBT"]-mean(ano$post[ano$trait=="CBT"]))/sd(ano$post[ano$trait=="CBT"])),"pnorm")	
set.seed(1)	
ks.test(jitter((ano$post[ano$trait=="Cont"]-mean(ano$post[ano$trait=="Cont"]))/sd(ano$post[ano$trait=="Cont"])),"pnorm")	
set.seed(1)	
ks.test(jitter((ano$post[ano$trait=="FT"]-mean(ano$post[ano$trait=="FT"]))/sd(ano$post[ano$trait=="FT"])),"pnorm")	
	
	
#' 	
#' 	
#' *Tableau récapitulatif des p-value obtenu grâce au test de Kolmogorov Smirnov*	
#' 	
#' 	
A=data.frame(evo=c(0.09674,0.9589, 0.905),pre=c( 0.6602,0.9397, 0.9846),post=c( 0.4049,0.9557, 0.1262))	
row.names(A)=c("CBT","Cont","FT")	
kable(A)	
	
#' 	
#' 	
#' Les distributions qui correspondent le plus à une loi normale sont les couples suivant: (FT,pre) ; (Cont,evo) ; (Cont,post) ; (Cont,pre)	
#' 	
#' 	
#' **Représentation des qqplot pour la variable caractérisant l'évolution du poids selon les 3 groupes**	
#' 	
#' 	
par(mfrow=c(1,3))	
qqnorm(ano$evo[ano$trait=="CBT"],main="CBT")	
abline(mean(ano$evo[ano$trait=="CBT"]),sd(ano$evo[ano$trait=="CBT"]))	
qqnorm(ano$evo[ano$trait=="Cont"],main="Cont")	
abline(mean(ano$evo[ano$trait=="Cont"]),sd(ano$evo[ano$trait=="Cont"]))	
qqnorm(ano$evo[ano$trait=="FT"],main="FT")	
abline(mean(ano$evo[ano$trait=="FT"]),sd(ano$evo[ano$trait=="FT"]))	
#' 	
#' 	
#' Nous remarquons que les réalisations du couple (Cont,evo)  correspond presque parfaitement à une loi normale, l'histograme empirique nous avait induit en erreur à cause d'une grande volatilité empirique. Les réalisations du couple (FT,evo) correspond lui aussi très bien à une loi normale alors que les réalisations du couple (CBT,evo) correspond moins à une loi normale.	
#' 	
#' 	
#' ##########################################################################################################################################################################################################  	
#' #Conclusion	
#' 	
#' 	
#' Le traitement FT est le plus efficace. Le traitement FT ne modifie pas la normalité de la distribution de l'évolution des poids alors que le traitement CBT la modifie.	
#' D'autre part, la forme de la distribution des poids pre-traitement correspond à celle d'une loi normale dans les 3 groupes mais la distribution des poids post-traitement ne correspond à celle d'une loi normale que pour le groupe n'ayant pas suivi de traitement. Nous pouvons donc supposer que la prise d'un traitement non placebo a contribué modifier la forme de la distribution des poids des patients.	
#' 	
#' 	
