#MEMORE D'ECONOMETRIE
#les variations du taux de consommation de cannabis selon des facteurs sociaux-économiques.

#renomer les différentes variables, plus de majuscule ni de BDD$
conso<-BDD$Conso
leg<-BDD$Leg
px<-BDD$px
sco<-BDD$sco #que nous laissons pour que vous puissiez voir notre cheminement mais que nous n'utiliserons pas car nous avons pris l'IDH
chom<-BDD$chom
sdp<-BDD$sdp
qrs<-BDD$qrs
tbc<-BDD$tbc
idh<-BDD$idh
pib<-BDD$PIB
crim<-BDD$crim

#analyse univarié de nos variables
summary(BDD)
var(conso)
var(chom)
var(px)
var(qrs)
var(sdp)
var(tbc)
var(crim)
var(idh)
var(pib)
var(sco)

#transformer ma variable de prix en une distribution normale car 
#la distribution ressemblait a une loi de Poisson 
logpx<-log(BDD$px)
mean(BDD$px) #vérification du changement
mean(logpx)
#de meme pour le pib
logpib<-log(BDD$PIB)
mean(BDD$PIB) #vérification du changement
mean(logpib)
summary(logpib)
var(logpib)

#distribution
hist((conso),main="Histograme présentant la distribution de la 
     consommation de cannabis",xlab="consommation de cannabis",
     ylab="fréquence")
hist(px,main="Histograme présentant la distribution des prix",
     xlab="prix",
     ylab="fréquence")
#puis nous faisons un histograme du prix mais cette fois en utilisant
#notre objet logpx pour faire apparaitre notre distribution de type normale
hist(logpx,main="Histograme présentant la distribution des prix",
     xlab="prix",
     ylab="fréquence")
hist((chom),main="Histograme présentant la distribution du chômage",
     xlab="chômage",
     ylab="fréquence")
hist((sco),main="Histograme présentant la distribution du 
     décrochage scolaire",xlab="décrochage scolaire",
     ylab="fréquence")
hist((qrs),main="Histograme présentant la distribution de la qualité
     du réseau social",xlab="qualité du réseau social",
     ylab="fréquence")
hist((sdp),main="Histograme présentant la distribution du taux 
     de pauvreté",xlab="taux de pauvreté",
     ylab="fréquence")
hist((leg),nclass=2,ylim=c(0,30),axes=FALSE);axis(side=1,at=c(0,0.5,1));axis(side=2,at=c(0,5,10,15,20,25,30),
      main="Histograme présentant la distribution de la tolerance au cannabsi",
      xlab="tolerance",
      ylab="frequence")
counts <- table(leg)
barplot(counts, main="légalité du cannabis", horiz=TRUE,
        names.arg=c("illégal", "toléré à légal"))
hist((tbc),main="Histograme présentant la distribution de
     la consommation de tabac",xlab="consommation de tabac",
     ylab="fréquence")
hist((idh),main="Histograme présentant la distribution de
     l'IDH",xlab="IDH",
     ylab="fréquence")
hist((pib),main="Histograme présentant la distribution du PIB",
     xlab="PIB",ylab="fréquence")
#puis nous faisons apparaitre notre nouvelle distribution cette fois normale grace a la fonction log
hist((logpib),main="Histograme présentant la distribution du PIB",
     xlab="PIB",ylab="fréquence")

#étude de la liaison entre la consommation et nos variables expliquatives
#covariance du taux de consommation avec chaque variable
cov(conso,chom)
cov(conso,px)
cov(conso,sco)
cov(conso,qrs)
cov(conso,sdp)
cov(conso,tbc)
cov(conso,idh)
cov(conso,logpib)
cov(conso,crim)

#représentation par graphique en nuage de points
plot(conso,chom)
plot(conso,logpx)
plot(conso,sco)
plot(conso,sdp)
plot(conso,tbc)
plot(conso,idh)
plot(conso,logpib)
plot(conso,crim)
plot(conso,qrs)

#analyses bivariés
lm1<-lm(conso~chom)
summary(lm1)
#notre p-value = 0,8699 donc notre variable n'est pas significative, 
#nous la laissons de coté pour notre modèle multivarié.

lm2<-lm(conso~px)
summary(lm2) 
#notre p-value = 0,181 donc notre variable n'est pas significative,
#nous refaisons une analyse bivarié avec notre variable transformée en loi normale

lmlogpx<-lm(conso~logpx)
summary(lmlogpx)
#la p-value = 0,3277 elle est donc encore moins significative,nous ne pouvons pas la garder, 
#cela signifie que le prix dans notre séléction de pays n'influe pas sur la consommation ou trop peu.

lm3<-lm(conso~qrs)
summary(lm3)
#notre p-value= 0,0079 donc notre donnée est significative avec un seuil de risque inférieur a 5%
#donc nous gardons cette donnée pour notre multivarié

lm4<-lm(conso~sco)
summary(lm4)
#la p-value = 0,6281 elle n'est donc pas significative,
#nous ne pouvons pas la garder pour notre modèle multivarié

lm5<-lm(conso~sdp)
summary(lm5)
#la p-value = 0.86928   elle n'est donc pas significative
#nous ne pouvons pas la garder dans notre modèle multivarié.

lm6<-lm(conso~tbc)
summary(lm6)
#la p-value est de 0,1213 nous ne pouvons donc pas l'intégrer a notre modèle multivarié.

lm7<-lm(conso~idh)
summary(lm7)
#la p-value est de 0,0103 donc nous pouvons garder cette variable pour notre modèle multivarié

lm8<-lm(conso~pib)
summary(lm8) #la p-value est de 0,0792 mais nous allons refaire l'analyse bivariée avec notre fonction log appliquée au pib
lm9<-lm(conso~logpib)
summary(lm9)
#la p-value est de 0,0151 donc nous pouvons garder cette variable pour notre modèle multivarié

lm10<-lm(conso~crim)
summary(lm10)
#la p-value étant de 0,464 le risque est trop fort pour que nous gardions cette variable pour le modèle multivarié

lm11<-lm(conso~leg)
summary(lm11)
#la p-value étant de O,2357 nous ne pouvons pas garder la variable "légalité" dans notre modèle multivarié.

#réalisation des scatterplots
scatterplot(idh,conso)
scatterplot(chom,conso)
scatterplot(crim,conso)
scatterplot(logpib,conso)
scatterplot(logpx,conso)
scatterplot(qrs,conso)
scatterplot(sco,conso)
scatterplot(sdp,conso)
scatterplot(tbc,conso)
scatterplotMatrix(~idh+chom+crim+logpib+logpx+qrs+sco+sdp+tbc)
#la consommation des etats unis etant exessivement forte par rapport aux reste des pays que nous avons pris,
#nous allons retirer ce pays de nos variables car il se retrouve régulièrement en valeur extreme
BDD1<-BDD[-9,]

conso1<-BDD1$Conso
leg1<-BDD1$Leg
px1<-BDD1$px
sco1<-BDD1$sco #que nous laissons pour que vous puissiez voir notre cheminement mais que nous n'utiliserons pas car nous avons pris l'IDH
chom1<-BDD1$chom
sdp1<-BDD1$sdp
qrs1<-BDD1$qrs
tbc1<-BDD1$tbc
idh1<-BDD1$idh
pib1<-BDD1$PIB
crim1<-BDD1$crim

#analyse multivarié du modèle
summary(lm(conso1~qrs1+idh1+log(pib1)))
#au vu de ce resultat, c'est a dire que seul la qualité du réseau social est validée par le modèle multivarié 
#nous allons subdiviser notre équation en deux modèles :

#on test des analyses multivariées différentes auxilliaires:
summary(lm(conso1~qrs1+idh1))
summary(lm(conso1~qrs1+log(pib1)))

#on renomme ces equations pour faciliter les tests
mg1<-(lm(conso1~qrs1+idh1))
mg2<-(lm(conso1~qrs1+log(pib1)))

#Tester la présence d'hétéroscédasticité
#test de breush-pagan sur l'homoscédasticité
bptest(mg1)
bptest(mg2)
#Les résultats du test d'hétéroscédasticité montrent que toutes les probabilités associées aux coefficients sont 
#toutes supérieures à 0,05. Donc nous rejetons l'hypothèse H1 d'hétéroscédasticité et supposons l'homoscédasticité des résidus.

#TEST DE SPECIFICATIVITE GLOBALE DU MODELE
#test sur la linéarité du modèle
raintest(mg1)
#la p-value de mg1 est de 0,4778 > 0,05 donc nous gardons l'hypothèse 0 : le modèle est linéaire
raintest(mg2)
#la p-value de mg2 est de 0,456 > 0,05 donc nous gardons l'hypothèse 0 : le modèle est linéaire

#test de fisher
#f-stat qu'on trouve dans le resultat de l'analyse multivarié = 0,0098 < 0,05 donc le modèle est globalement significatif
#Hypothèses sur les résidus:
#   -Homocédasticité des résidus
#   -Normalité des résidus
#   -Absence d'autocorelation des résidus

# Analyse graphique des résidus

plot(residuals(mg1), main="residus")
plot(residuals(mg1), predict(mg1)) #résidus versus prédiction
plot(residuals(mg2), main="residus")
plot(residuals(mg2), predict(mg2))

#tester la normalité des résidus avec le test de Shapiro
#Graphique
hist(residuals(mg1)) #on peut déja observer une distribution ne ressemblant pas à une distribution normale des résidus, la médiane est bien proche de 0 mais Q1 et Q3 sont assez différents.
qqnorm(residuals(mg1))
qqline(residuals(mg1))#la distribution des résidus est proche de la droite donc on peut penser qu'en effet le résultat du test shapiro peut accepter la normalité des résidus

shapiro.test(residuals(mg1))
#la p-value est de 0,5529 > 0,05 donc la distribution normale des résidus est acceptée

hist(residuals(mg2)) #on peut déja observer une distribution ne ressemblant pas à une distribution normale des résidus, la médiane est bien proche de 0 mais Q1 et Q3 sont assez différents.
qqnorm(residuals(mg2))
qqline(residuals(mg2))#la distribution des résidus est n'est pas si proche de la droite donc on peut penser qu'en effet le résultat du test shapiro peut ne pas accepter la normalité des résidus

shapiro.test(residuals(mg2))
#la p-value est de 0,3639> 0,05 donc la distribution normale des résidus est acceptée


#test sur la colinéarité des variables
#VIF:facteur d'inflation de la variance : problème de multi-collinéarité si >10
vif(mg1) #donc pas de problème de multi-collinéarité dans le modèle
vif(mg2) #de meme pas de multi-colinéarité
vif(lm(qrs1~idh1+pib1)) #donc la qualité du réseau social n'est pas corrélé au pib ou a l'idh
vif(lm(idh1~pib1+qrs1)) #donc l'idh n'est pas corrélé au pib et a la qualité du réseau social
vif(lm(pib1~idh1+qrs1)) #donc le pib n'est pas corrélé a l'idh ou a la qualité du réseau social

#le critère d'Akaike 
AIC(mg1)
AIC(mg2)
#on retient l'equation dont le coeff est le plus petit soit mg1 donc celle dont les variables explicatives
#sont la qualité du réseau social et l'idh --> mg1

