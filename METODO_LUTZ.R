

library(xts)
library(dplyr)
library(ggplot2)


Pm<- read.csv("C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/levelRainP.csv")            #Lectura de la precipitacion mensual
Temp_mensual <- read.csv("C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/Temp.csv")       #Lectura de la Temperatura promedio mensual
Evp <- read.csv("C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/Evp_Pot.csv")             #Lectura de la Evaporacion promedio mensual
Area_Reg <- read.csv("C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/Areas.csv")             #Lectura de las Areas
Qmen<- read.csv("C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/Q_mensual.csv")             #Lectura de Caudales Aforados



Temp_mensual=ts(Temp_mensual,freq=12)
Evp=ts(Evp,freq=12)
A=ts(Area_Reg,freq=1)   #Areas de acuiferos, lagunas y nevado en la cuenca

#Ploteo de la serie mensual Precipitacion
Pmensual=ts(Pm,freq=12,start=c(1964,1))
plot(Pmensual,type="l",pch=1, col="blue",xlab="Tiempo (Años)",ylab="Pm (mm)",main="Precipitación media mensual (mm) \n multianual de la cuenca del rio Coata")

dev.copy(jpeg,filename="C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/IMAGANES/Pmensual.png")
dev.off()


# type="o",pch=1, col="blue", xlab="Mes",ylab="Q (m3/s)",main="Calibracion del Modelo Lutz Scholz -Aplicacion rio Coata"

#Ploteo de la serie mensual Caudal- Est. Puente Unocolla
Q_mens=ts(Qmen,freq=12,start=c(1964,1))

plot(Q_mens,type="l",pch=1, col="blue",xlab="Tiempo (Años)",ylab="Q (m3/s)",main="Serie de caudales medios mensuales (m3/s) \n Estacion Puente Unocolla.")

dev.copy(jpeg,filename="C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/IMAGANES/Q_mens.png")
dev.off()



Qmen=ts(Qmen,freq=12)
Qmen

##
Qmen<- matrix(Qmen,nrow=12)
Qmen=t(Qmen)
colnames(Qmen)<- c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SETIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")

Qm_ap <- round(c(apply(Qmen,2,mean)),2)
Qm_ap
##

Pm=ts(Pm,freq=12)
Pm


#Cambiar a matrices para realizar las operaciones de una manera mas rapida y asignacion de nombre en filas y columnas

Pm<- matrix(Pm,nrow=12)
Pm=t(Pm)
colnames(Pm)<- c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SETIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")

PE= matrix(0,dim(Pm)[1],dim(Pm)[2])   #matriz de ceros
colnames(PE)<- c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SETIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")

Temp_mensual<- matrix(Temp_mensual,ncol=12)
colnames(Temp_mensual)<- c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SETIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")
rownames(Temp_mensual)<- c("Temp (°c)")

Evp<- matrix(Evp,ncol=12)
colnames(Evp)<- c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SETIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")
rownames(Evp)<- c("ETP (mm)")

A=matrix(A)
rownames(A)<- c("Acuiferos","Lagunas","Nevados","Cuenca")
colnames(A)<- c("Area (km2)")
A=t(A)

##1)Metodo de L.Turc para el calculo del coeficiente de escorrentia

#Generacion de Precipitacion media anual (P)
#Temperatura media anual (T)
#la Evaporacion Total Anual (ETP)

P<- round(mean(apply(Pm,1,sum)),2)
T<- round(mean(Temp_mensual),1)
ETP<- round(sum(Evp),1)

cat("Precipitacion media anual: ", P , "\n","Temperatura media anual: ", T,"\n","Evaporacion Total Anual: ", ETP )

#Calculo del coeficiente de Temperatura (L)

L<-round(300+25*T+0.05*T^3,0)


#Calculo del Deficit de EScurrimiento (D)
 
D<-round(P*(1/(0.9+P^2/L^2)^0.5),1)

#calculo del Coeficiente de Escorrentia (C)

C<-round((P-D)/P,2)

cat("coeficiente de Temperatura: ", L , "\n","Deficit de EScurrimiento: ", D,"\n","Coeficiente de Escorrentia: ", C )


##2)Metodo de Mision Alemana para el calculo del coeficiente de escorrentia (Cma)

Cma=round(3.16E12*(P^-0.571)*(ETP^-3.686),2)




#DATA DE cURVAS cI, CII y CIII

C1<-0.15
PL1=177.8
LC1=120.6
C2<-0.30
PL2=152.4
LC2=86.4
C3<-0.45
PL3=127
LC3=59.7

CI<- c(-0.047,0.009,0.0005,0.00002,-0.00000005,2E-10)
CII<- c(-0.1065,0.1477,-0.0029,0.00005,-0.0000002,2E-10)
CIII<- c(-0.4177,0.3795,-0.0101,0.0002,-9.00E-07,1.00E-09)

#COEFICIENTES DEL POLINOMIO a0 a1 a2 a3 a4 a5 

if (C < C2 ) {
  x=(CII-CI)/15
  Cm=CI+x*100*(C-C1)
  PLm=(PL2-PL1)*(C-C1)/(C2-C1) +PL1
  LCm=(LC2-LC1)*(C-C1)/(C2-C1) +LC1
} else 
  x=(CIII-CII)/15
  Cm=CII+x*100*(C-C2)
  PLm=(PL3-PL2)*(C-C2)/(C3-C2) +PL2
  LCm=(LC3-LC2)*(C-C2)/(C3-C2) +LC2
  
cat("COEFICIENTES DEL POLINOMIO a0 a1 a2 a3 a4 a5: ", Cm,"\n","Rango: " ,PLm,"\n","Limite superior para la PE: P-",LCm) 

#CALCULO DE LA PRECIPITACION EFECTIVA (PE)




for(i in 1:dim(Pm)[1]) {
  for(j in 1:dim(Pm)[2]) {
    if (Pm[i,j]>PLm) {
      PE[i,j]=round(Pm[i,j]-LCm,2)
    } else
      PE[i,j] = round(Cm[1]+Cm[2]*Pm[i,j]+Cm[3]*Pm[i,j]^2+Cm[4]*Pm[i,j]^3+Cm[5]*Pm[i,j]^4+Cm[6]*Pm[i,j]^5,2)
  }
}

cat("Precipitacion efectiva: ",PE)


#######Generación de caudales mensuales para el año promedio (Pm_ap)

#1)Obtencion de la Precipitacion efectiva 

N_dias <- c(30,28,31,30,31,30,31,31,30,31,30,31)

Pm_ap <- round(c(apply(Pm,2,mean)),2)
PEII=round(CII[1]+CII[2]*Pm_ap+CII[3]*Pm_ap^2+CII[4]*Pm_ap^3+CII[5]*Pm_ap^4+CII[6]*Pm_ap^5,1)
PEIII=round(CIII[1]+CIII[2]*Pm_ap+CIII[3]*Pm_ap^2+CIII[4]*Pm_ap^3+CIII[5]*Pm_ap^4+CIII[6]*Pm_ap^5,1)

C_PII=round((C*sum(Pm_ap)-sum(PEIII))/(sum(PEII)-sum(PEIII)),3)
C_PIII=round((C*sum(Pm_ap)-sum(PEII))/(sum(PEIII)-sum(PEII)),3)

PE_ap=round(PEII*C_PII+PEIII*C_PIII,1)   #Precipitacion efectiva para el año promedio mm/mes

cat("Precipitacion media: ", Pm_ap,"\n","Precipitacion efectiva Curva II: ", PEII,"\n","Precipitacion efectiva Curva III: ", PEIII,"\n","Precipitacion efectiva: ", PE_ap)


#2)Calculo del Gasto de la Retencion
#  Epoca seca (abril - octubre)

#Obtencion de la Retencion (R) mm/año

R=round((A[1]*300+A[2]*500+A[3]*500)/A[4],0)  

AR=A[4]   #Area de la cuenca
Ts<- 150   #Dias seco

#Calculo del coeficiente de agotamiento

a=round(3.1249E67*(AR^-0.1144)*((ETP)^-19.336)*(Ts^-3.369)*(R^-1.429),4)

#Calculo de la relacion de caudales (b0)

bo=exp(-a*30)

bi <- c(0,0,0,bo,bo^2,bo^3,bo^4,bo^5,bo^6,bo^7,0,0)
Gi <- bi*R/sum(bi)

cat("Retencion: ", R,"\n","Coeficiente de Agotamiento: ",a,"\n","Relacion de caudales: ", bo)

  
#3)Calculo del Abastecimiento de la Retencion

ai<-c(0.4,0.2,0,0,0,0,0,0,0,0,0.05,0.35)
Ai<-ai*R

#4)Calculo de los caudales Generados

Q_mm<- round(PE_ap+Gi-Ai,1)
Q_m3<- round((Q_mm*AR*1000000/1000)/(3600*24*N_dias),2)
mes<- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Setiembre","Octubre","Noviembre","Diciembre")
cat("Caudal Generado: ", Q_m3,"\n","Caudale medio Medido: ",Qm_ap)


#Comparación de caudales calculados y aforados, luego de la calibración para el año promedio del río Coata

Mes=c(1,2,3,4,5,6,7,8,9,10,11,12)
plot(Mes,Qm_ap, type="o",pch=1, col="blue", xlab="Mes",ylab="Q (m3/s)",main="Calibracion del Modelo Lutz Scholz -Aplicacion rio Coata")
lines(Mes,Q_m3,type="o",pch=2,col="red")
legend("topright",legend=c("Q aforado","Q generado"),pch=c(1,2),col=c("blue","red"))

dev.copy(jpeg,filename="C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/IMAGANES/Q_comp.png")
dev.off()

Q_mm_t1 <- c(Q_mm[12],Q_mm[1],Q_mm[2],Q_mm[3],Q_mm[4],Q_mm[5],Q_mm[6],Q_mm[7],Q_mm[8],Q_mm[9],Q_mm[10],Q_mm[11])
Q_mm_t1  #x1
PE_ap  #x2
Q_mm  #Y

#Obtencion de los parametros de la correlacion lineal multiple

datos <- data.frame(Q_mm_t1, PE_ap, Q_mm)

modelo <- lm(formula = Q_mm ~ Q_mm_t1 + PE_ap, data = datos)
modelo
summary(modelo)  


#names(summary(modelo)) para conecer los campos de las variables y extraerlas

B1=summary(modelo)$coefficients[1]
B2=summary(modelo)$coefficients[2]  
B3=summary(modelo)$coefficients[3]
R2=summary(modelo)$r.squared         
S=summary(modelo)$sigma              

cat("Luego de realizar la regresion lineal multiple se obtuvo los siguientes resultados:","\n","B1: ",B1,"\n","B2 ",B2,"\n","B3: ",B3,"\n","R2: ",R2,"\n","S: ",S)

N_aleat=rnorm(length(Pm),0,1)
N_aleat

N_aleat=ts(N_aleat,freq=12)
N_aleat=matrix(N_aleat,nrow=12)
N_aleat=t(N_aleat)
colnames(N_aleat)<- c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SETIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")

cat("Numeros aleatorios generados con distribucion normal: ",N_aleat)

Q_gen=matrix(0,dim(N_aleat)[1],dim(N_aleat)[2])

z=as.vector(t(N_aleat))
Pe=as.vector(t(PE))
Q=as.vector(t(Q_gen))




for ( i in 1:length(z)) {
  if (i) {
    Q[i]=round(B1+B2*0+B3*Pe[i]+z[i]*S*(1-R2)^0.5,2)
    } else
      Q[i]=round(B1+B2*Q[i-1]+B3*Pe[i]+z[i]*S*(1-R2)^0.5,2)
}


Q=Q*AR*1000000/(1000*30*24*3600)
Q

Q_gen=t(matrix(Q,nrow=12))


#Pmensual=ts(Pm,freq=12,start=c(1964,1))



Q_gen=as.vector(t(Q_gen))
Q_gen=ts(Q_gen,freq=12,start=c(1964,1))




plot(Q_mens,type="l",pch=1, col="blue",xlab="Tiempo (Años)",ylab="Q (m3/s)",main="Histograma de caudales generados y caudales aforados \n del rio coata - periodo 1966 - 2010")
points(Q_gen,type="l",pch=2,col="red")
legend("topleft",legend=c("Q_mens","Q_gen"),pch=c(1,2),col=c("blue","red"))


dev.copy(jpeg,filename="C:/Users/Pablo/Desktop/CE721/CLASE_R/ESCALONADO/IMAGANES/Q_final.png")
dev.off()

plot(Q_mens ,Q_gen)

datos2=data.frame(Q_mens,Q_gen)
modelo2 <- lm(formula = Q_mens ~ Q_gen, data = datos2)
summary(modelo2)


R2_validacion=summary(modelo2)$r.squared  #coeficiente de correlacion multiple 






