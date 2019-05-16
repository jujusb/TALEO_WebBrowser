courrier = read.table("Courrier.txt")
colnames(courrier)=c("Poids","Nb_lettres")
attach(courrier)
plot(Poids,Nb_lettres,xlab="Poids du courrier (t)",ylab="Nombre de lettres",pch=19,xlim=c(9,39),ylim=c(700,2500))

### Le modèle linéaire semble adapté à notre problème de régression
### Modèle : pour tout i=1,...,21 Nb_lettresi = B0 + B1*Poidsi + Ei 
### Avec Ei suivant la loi N(0,s²)
reg = lm(Nb_lettres~Poids,data=courrier)
resume=summary(reg)
resume 
### str(reg)
### str(resume)
### summary(courrier)


x=c(min(Poids),max(Poids))
y=198+57.7*x
lines(x,y,col="blue",lwd=3)


### reg$fitted.values = 198+57.7*x
### R² est de 0.9628
R2=sum((reg$fitted.values-mean(Nb_lettres))^2)/sum((mean(Nb_lettres)-Nb_lettres)^2)
R2 

residus = reg$fitted.values-Nb_lettres
ychapeau=198+Poids*57.7
plot(ychapeau,residus,col="red")
yr=ychapeau*0
lines(ychapeau,yr,col="green")
###abline(h=0)
yv1=ychapeau*0+2*90
lines(ychapeau,yv1,col="purple")
###abline(h=2*resume$sigma)
yv2=ychapeau*0-2*90
lines(ychapeau,yv2,col="purple")
###abline(h=-2*resume$sigma)

### Les residus semblent avoir tendance à être compris entre -2sigma et 2sigma
R22=sum((198-mean(Nb_lettres))^2)/sum((mean(Nb_lettres)-Nb_lettres)^2)
R22
### R² dans le cas B1=0, on a 0.360, le modèle semble inintéressant
resume
### proba critique P(>|t|) =4,7e-15 pour le poids or Pr<5% donc le test H0 est rejeté et B1 est significativement différent de 0

198+27.5*57.7
frame=data.frame(Poids=27.5)
predict(reg,newdata=frame)
qt(0.975,19)
borne1=1784.75-2.093*90*sqrt(1+1/21+((27.5-mean(Poids))^2)/(sum((Poids-mean(Poids))^2)))
borne1

borne2=1784.75+2.093*90*sqrt(1+1/21+((27.5-mean(Poids))^2)/(sum((Poids-mean(Poids))^2)))
borne2

borne=predict(reg,newdata=data.frame(Poids=27.5),interval='prediction')
borne
plot(Poids,borne1Variable,col="red")
poids2=v[0:40]
borne=predict(reg,newdata=data.frame(Poids))[2]+2.093*90*sqrt(1+1/21+((Poids-mean(Poids))^2)/(sum((Poids-mean(Poids))^2)))
borne2Variable
plot(Poids,borne2Variable,col="blue")
