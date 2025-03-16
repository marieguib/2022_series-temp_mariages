# Dossier series temporelles #

# Initialisation----------------------------------------------------------------

init =read.table("valeurs_mensuelles_corrigees.csv",sep=";",header=TRUE)
View(init)
init$times=seq(1,nrow(init))
init$Mariagest=ts(init$Mariages,start=1996+(01/12),frequency=12,end=2019+(12/12))
init$mois=rep(1:12,24)
init$an=rep(1996:2019,each=12)

# Partie 1 : Analyse descriptive -----------------------------------------------

# Determination du modele-------------------------------------------------------

#methode de la bande
plot(init$Mariagest)
#sur le graphe, on remarque que les droites sont plutet en forme d'entonnoir : modele muliplicatif

#methode du profil
mat=matrix(data = init$Mariagest ,nrow =24,ncol= 12,byrow=TRUE)
ymin=min(mat[1:24,])
ymax=max(mat[1:24,])
plot(mat[1,],type="l",col=1,ylim=c(ymin,ymax), main="Methode du profil", xlab="Temps (mois)", ylab="Nombre de mariages")
for(i in 2:24) {lines(mat[i,],type="l",col=i)}
# On remarque que les profils s'entrecroisent : modele multiplicatif

#Test de buys-ballot
aganmean = aggregate (init$Mariages,list(an=init$an),mean)
agansd = aggregate(init$Mariages,list(an=init$an),FUN="sd")
tbb=lm(agansd$x~aganmean$x)
summary(tbb)
#P-value est de 7.732e-14 

# Conclusion
# Modele multiplicatif






# Partie 2 : Moyennes mobiles --------------------------------------------------


# Transformation de la serie----------------------------------------------------
powerTransform(init$Mariages)
init$mod=((init$Mariages^(-0.1156859))-1)/(-0.1156859)

plot(init$mod, type="l", col="red", main="Transformation BOX-COX de la serie", xlab="init$times")

# Representation graphique la serie transformee
#ymaxbis=max(init$mod)                                                 #pas oblige ymax je pense
#lines(init$mod,type="l",col="red")
legend("topleft",legend = "init$mod",col="red",lty=1)


#autre transformation possible : ln
init$lnmariages = log(init$Mariages)



#representation graphique des 2 series transformees
ymin2 = min(init$mod)
ymax2 = max(init$lnmariages)
plot(init$lnmariages, ylim=c(ymin2, ymax2), type="l", col="blue")
lines(init$mod, type="l", col="red")
legend("topleft",legend = c("init$mod","init$lnmariages"),col=c("red","blue"),lty=1)









# MM arithmetique d'ordre 13----------------------------------------------------
init0 = init[1:276,]
filter12=c(1/24,rep(1/12,11),1/24)
init0$mariages13=filter(init0$Mariagest, filter12, method = "convolution",sides = 2, circular = FALSE)
# graphique
plot(init0$Mariagest~init0$times,type='l',col='black',lwd=1, main="Serie temporelle des mariages apres application d'une MM13", ylab="Nombre de mariages")
lines(init0$mariages13~init0$times,type='p',col='blue',lwd=1)








#etude de la serie initiale ----------------------------------------------------

#calcul des coefs saisonniers
init0$sais0 = init0$Mariagest-init0$mariages13
sais0 = tapply(init0$sais0,init0$mois, mean, na.rm=TRUE)
sais0moy = mean(sais0)
sais0bis=sais0-sais0moy
init0$sais0bis=rep(sais0bis, 23)                       
init$sais0bis=rep(sais0bis,24)                          

#extrapolation de la tendance
init0$desais = init0$Mariagest - init0$sais0bis       #serie desaisonnalise
reg=lm(init0$desais~init0$times)
summary(reg)

#estimation de la tendance
init0$tchap0 = 25266.173+(-22.898)*init0$times          
init0$prev0 = init0$tchap0 + init0$sais0bis
init$tchap0 = 25266.173+(-22.898)*init$times          
init$prev0 = init$tchap0 + init$sais0bis

#calcul de la SCR
res0car = (init0$Mariages- init0$prev0)^2
SCR = sum(res0car)

ymax3 = max(init0$Mariagest)
plot(init0$prev0~init0$times, ylim=c(0,ymax3), type="l", col="black", lwd=1, main="Etude de la serie initiale")    #serie estimee
lines(init0$tchap0~init0$times, type='l', col="red", lwd=2)                                                        #tendance
lines(init0$Mariagest~init0$times, type="l", col="blue", lwd=1)                                                    #vraie serie
legend("topleft",legend = c("serie estimee","tendance", "serie initiale"),col=c("black","red", "blue"),lty=1)


#prevision
ptimes = seq(289,300)
tchap0 = 25266.173+(-22.898)*ptimes
psais0 = c(sais0bis)
prev0 = tchap0 + psais0

plot(init$Mariages~init$times, xlim=c(1,300), col="black", type='l')
lines(init$prev0~init$times, col="blue", type="l")
lines(prev0~ptimes, col="red", type="l")






#etude de la serie modifie par BOX-COX -----------------------------------------

#MM13 sur serie modifie
init0$mod13 = filter(init0$mod, filter12, method="convolution", sides=2, circular=FALSE)

#calcul des coefs saisonnier
init0$modsais1 = init0$mod-init0$mod13
sais1mod = tapply(init0$modsais1, init0$mois, mean, na.rm=TRUE)
sais1modmoy = mean(sais1mod)
sais1modbis = sais1mod - sais1modmoy
init0$sais1modbis = rep(sais1modbis, 23)
init$sais1modbis = rep(sais1modbis, 24)

#extrapolation de la tendance
init0$desais1 = init0$mod - init0$sais1modbis
reg5=lm(init0$desais1 ~ init0$times)
summary(reg5)

#estimation de la serie mod
init0$tchap1 = 5.881+(-0.0002517)*init0$times
init0$modprev1 = init0$tchap1 + init0$sais1modbis
init$tchap1 = 5.881+(-0.0002517)*init$times
init$modprev1 = init$tchap1 + init$sais1modbis


init0$prev1 = ((-0.1156859)*init0$modprev1+1)^(1/(-0.1156859))
init$prev1 = ((-0.1156859)*init$modprev1+1)^(1/(-0.1156859))

#calcul de la SCR
res5car = (init0$Mariages-init0$prev1)^2
SCR5 = sum(res5car)

#graphique
plot(init0$Mariages~init0$times, type="l", col="black", lwd=1, main="Graphique de la serie transformee par Box-Cox", ylab="Nombre de mariages")
lines(init0$prev1~init0$times, type="l", col="red", lwd=1)                 
legend("topleft",legend = c("serie initiale","serie modifiee par Box-Cox"),col=c("black", "red"),lty=1)

plot(init$Mariages~init$times, type="l", col="black", lwd=1, main="Graphique de la serie transformee par Box-Cox", ylab="Nombre de mariages")
lines(init$prev1~init$times, type="l", col="red", lwd=1)                 
legend("topleft",legend = c("serie initiale","serie modifiee par Box-Cox"),col=c("black", "red"),lty=1)

#prevision de la serie mod
modtchap1 = 5.881+(-0.0002517)*ptimes
psais = c(sais1modbis)
modprevision = modtchap1 + psais
mariagesprev1 = ((-0.1156859)*modprevision + 1)^(1/(-0.1156859))
plot(init$Mariages~init$times, xlim=c(1,300), col="black", type='l')
lines(init$prev1~init$times, col="blue", type="l")
lines(mariagesprev1~ptimes, col="red", type="l")






#etude de la serie passage en log-----------------------------------------------

#MM13
init0$lnmariages13 = filter(init0$lnmariages, filter12, method="convolution", sides=2, circular=FALSE)

#graphique
plot(init0$lnmariages~init0$times, type="l", col="black", lwd=1)
lines(init0$lnmariages13~init0$times, type="l", col="red", lwd=1)

#calcul des coefs saisonnier
init0$lnmariagessais2 = init0$lnmariages-init0$lnmariages13
lnmariagessais2 = tapply(init0$lnmariagessais2, init0$mois, mean, na.rm=TRUE)
lnmariagessais2moy = mean(lnmariagessais2)
lnmariagessais2bis = lnmariagessais2 - lnmariagessais2moy
init0$lnmariagessais2bis = rep(lnmariagessais2bis, 23)
init$lnmariagessais2bis = rep(lnmariagessais2bis, 24)

#extrapolation de la tendance
init0$desais2 = init0$lnmariages - init0$lnmariagessais2bis
reg6 = lm(init0$desais2~init0$times)
summary(reg6)

#estimation et prevision de la serie en ln
init0$lnTchap = 9.8894208+(-0.0008012)*init0$times
init$lnTchap = 9.8894208+(-0.0008012)*init$times

init0$lnprev2 = init0$lnTchap + init0$lnmariagessais2bis
init$lnprev2 = init$lnTchap + init$lnmariagessais2bis

#estimation et prevision de la serie Pass reelle
init0$prev2 = exp(init0$lnprev2)
init$prev2 = exp(init$lnprev2)

#calcul de la SCR
res6car = (init0$Mariages-init0$prev2)^2
SCR6 = sum(res6car)


#prevision de la serie en log
lntchap2 = 9.8894208+(-0.0008012)*ptimes
psais2 = c(lnmariagessais2bis)
lnprev2 = lntchap2 + psais2
mariagesprev2 = exp(lnprev2)
plot(init$Mariages~init$times, xlim=c(1,300), col="black", type='l')
lines(init$prev2~init$times, col="blue", type="l")
lines(mariagesprev2~ptimes, col="red", type="l")





#graphique
plot(init0$Mariages~init0$times, type="l", col="burlywood1", lwd=3)
lines(init0$prev0~init0$times, type="l", col="black", lwd=1)
lines(init0$prev1~init0$times, type="l", col="red", lwd=1)               
lines(init0$prev2~init0$times, type="l", col="blue", lwd=1)








#comparaison des estimations----------------------------------------------------
init1 = init[277:288,]
plot(init1$Mariages~init1$times, type="l", col="burlywood1", lwd=3)
lines(init1$prev0~init1$times, type="l", col="black", lwd=1)
lines(init1$prev1~init1$times, type="l", col="red", lwd=2)             
lines(init1$prev2~init1$times, type="l", col="blue", lwd=1)

#prevision modele BOX-COX
ptimes = seq(289,300)
modTchap = 5.881+(-0.0002517)*ptimes
init$modTchap = modTchap
psais = c(sais1modbis)

#prevision de la serie modifie
#modprev = modTchap+psais
#prev3 = ((-0.1156859)*modprev+1)^(1/(-0.1156859))

#plot(init$Mariages~init$times, type="l", col="black", lwd=1)
#lines(init$prev1~init$times, type="l", col="blue", lwd=2)
#lines(prev3~ptimes, type="l", col="green", lwd=2)


#comparaison des tendances
init$mariagesmodT=((-0.1156859)*init$modTchap+1)^(1/(-0.1156859))
init$mariageslnT=exp(init$lnTchap)
plot(init$Mariages~init$times,type='l',col='burlywood1',lwd=1)
lines(init$mariagesmodT~init$times,type='l',col='red',lwd=3)
lines(init$mariageslnT~init$times,type='p',col='blue',lwd=2)

# Comparaison des coefficients saisonniers
mariagesmodS=((-0.1156859)*sais1modbis+1)^(1/(-0.1156859))
mariageslnS=exp(lnmariagessais2bis)
plot(mariagesmodS)
lines(mariageslnS)

# Partie 3 : Lissages ---------------------------------------------------------------------------------------

#lissage exponentiel simple 
LESx1 = HoltWinters(init$Mariages,alpha = 0.1,beta = FALSE,gamma = FALSE)
PREV_LESx1=predict(LESx1,12,prediction.interval=TRUE)
plot(LESx1,PREV_LESx1)

LESx2 = LESx1=HoltWinters(init$Mariages,alpha = 0.9,beta = FALSE,gamma = FALSE)
PREV_LESx2=predict(LESx2,12,prediction.interval=TRUE)
plot(LESx2,PREV_LESx1)

#lissage de Holt
LEHx1=HoltWinters(init$Mariages,alpha = 0.1,beta = 0.1,gamma = FALSE)
PREV_LEHx1=predict(LEHx1,12,prediction.interval=TRUE)
plot(LEHx1,PREV_LEHx1) 

LEHx2=HoltWinters(init$Mariages,alpha = 0.9,beta = 0.9,gamma = FALSE)
PREV_LEHx2=predict(LEHx2,12,prediction.interval=TRUE)
plot(LEHx2,PREV_LEHx2) 


#lissage de Holt-Winters SANS declaration des coefficients de lissage, MODELE ADDITIF---------------------
HW1=HoltWinters(init$Mariagest)
plot(HW1)

HW1fit=fitted(HW1)
HW1fit

p1=predict(HW1,12,prediction.interval=TRUE)
plot(HW1,p1)
SCR1=sum((HW1fit-init$Mariagest)^2)
SCR1


#lissage de Holt-Winters SANS declaration des coefficients de lissage, MODELE MULTIPLICATIF----------------
HW2=HoltWinters(init$Mariagest, seasonal="multiplicative")                                  
plot(HW2)                                                                                   
                                                                                            
HW2fit=fitted(HW2)
HW2fit

p2=predict(HW2,12,prediction.interval=TRUE)
plot(HW2,p2)

SCR2=sum((HW2fit-init$Mariagest)^2)
SCR2



plot(HW1fit)
plot(HW2fit)

