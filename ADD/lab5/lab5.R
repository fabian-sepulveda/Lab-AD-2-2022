#------------------------------------------#
library(MTS)
library(signal)
library(TSA)
library(oce)
library(ggplot2)
library(descomponer)
#------------------------------------------#




#--- LEYENDO Y DEJANDO LISTOS LOS DATOS ---#
datosNormo <- read.csv2("C:/Users/fabia/Desktop/Lab-AD-2-2022/ADD/lab5/TJ000.txt", 
#datosNormo <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/02-2022/Análisis de datos/Laboratorio/Lab-AD-2-2022/ADD/lab5/TJ000.txt", 
                   sep = "\t",
                   header = FALSE)

datosHiper <- read.csv2("C:/Users/fabia/Desktop/Lab-AD-2-2022/ADD/lab5/TJ001.txt", 
#datosHiper <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/02-2022/Análisis de datos/Laboratorio/Lab-AD-2-2022/ADD/lab5/TJ001.txt", 
                        sep = "\t",
                        header = FALSE)






#########################################################################################
#########################################################################################
#########################################################################################





#NORMO
PAM_Normo <- datosNormo[,1]
VFSC_Normo <- datosNormo[,3]
datos_Normo <- matrix(c(PAM_Normo,VFSC_Normo),ncol = 2)


#Señales
PamNormoT <- as.numeric(ts(data = PAM_Normo, frequency = 5, deltat = 0.2))
VFSCNormoT <- as.numeric(ts(data = VFSC_Normo, frequency = 5, deltat = 0.2))

plot.ts(PamNormoT, main="PAM paciente normocapnia",
        xlab = "Tiempo", ylab = "PAM")
plot.ts(VFSCNormoT, main= "VFSC paciente normocapnia",
        xlab = "Tiempo", ylab = "VFSC")



#LOS GRAFICOS PARA PAM normocapnia
#periodograma
periodPAM_NormoT <- pwelch(PamNormoT, nfft=256, fs = 5)


#---Validación cruzada--- 

rxy_Normo <- ccf(PamNormoT,VFSCNormoT,pl=TRUE)
plot(rxy_Normo, main = "Correlacion Cruzada PAM y VFSC, normocapnia",
     xlab = "Lag (Default)")
rxy_Normo_max <- ccf(PamNormoT,VFSCNormoT,lag.max = 750,pl=TRUE)
plot(rxy_Normo_max, main = "Correlacion Cruzada PAM y VFSC, normocapnia",
     xlab = "Lag (max = 750)")


#Periodograma suavizado
wRxy_Normo <- pwelch(rxy_Normo$acf, nfft = 256, fs = 5, plot = TRUE)




#---Autocorrelacion---
rxx_Normo <- acf(PamNormoT, lag.max = length(PamNormoT))
plot(rxx_Normo, main = "Auto-Correlación PAM normocapnia")

#------------
Rx_Normo <- acf(PamNormoT, lag.max = length(PamNormoT))
wx_Normo <- pwelch(Rx_Normo$acf, nfft = 256, fs = 5, plot = TRUE)


#Función de transferencia

tfm_Normo <- wRxy_Normo$spec/wx_Normo$spec
plot(tfm_Normo, type="b", main = "Función de transferencia Normocapnia",
     xlab = "Frecuencia", ylab = "")



#-----------------------------#

t=35;
fin = t/0.2;
drop=76;
step=rep(1,fin);
step[drop:fin]=rep(0,fin-drop+1);
fw=butter(2,0.3);
step_f=filter(fw$b,fw$a,step);

tfm_t <- ifft(tfm_Normo)

Yx = conv(step_f,tfm_t)

plot(abs(Yx))




#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################






########### HIPER ###########

PAM_Hiper <- datosHiper[,1]
VFSC_Hiper <- datosHiper[,3]
datos_Hiper <- matrix(c(PAM_Hiper,VFSC_Hiper),ncol = 2)

#Señales

PamHiperT <- as.numeric(ts(data = PAM_Hiper, frequency = 5))
VFSCHiperT <- as.numeric(ts(data = VFSC_Hiper, frequency = 5))

plot.ts(PamHiperT)
plot.ts(VFSCHiperT)



#Periodograma
periodPAM_HiperT <- pwelch(PamHiperT, nfft=256, fs = 5)


#---Validación cruzada---
rxy_Hiper <- ccf(PamHiperT,VFSCHiperT,pl=TRUE)
plot(rxy_Hiper, main = "Correlacion Cruzada PAM y VFSC, normocapnia",
     xlab = "Lag (Default)")
rxy_Hiper <- ccf(PamHiperT,VFSCHiperT,lag.max = 750,pl=TRUE)
plot(rxy_Hiper, main = "Correlacion Cruzada PAM y VFSC, normocapnia",
     xlab = "Lag (750)")
#Periodograma
wRxy_Hiper <- pwelch(rxy_Hiper$acf, nfft = 256, fs = 5, plot = TRUE)


#Autocorrelacion
rxy_Hiper <- acf(PamHiperT, lag.max = length(PamHiperT))
plot(rxy_Hiper, main = "Auto-Correlación PAM hipercapnia")

#------------------------------------------------------------------
Rx_Hiper <- acf(PamHiperT, lag.max = length(PamHiperT))
wx_Hiper <- pwelch(Rx_Hiper$acf, nfft = 256, fs = 5, plot = TRUE)


#Función de transferencia

tfm_Hiper <- wRxy_Hiper$spec/wx_Hiper$spec
plot(tfm_Hiper, type="b")



#-----------------------------#

t=35;
fin = t/0.2;
drop=76;
step=rep(1,fin);
step[drop:fin]=rep(0,fin-drop+1);
fw=butter(2,0.3);
step_f=filter(fw$b,fw$a,step);

tfm_t <- ifft(tfm_Hiper)

Yx = conv(step_f,tfm_t)

plot(abs(Yx))




