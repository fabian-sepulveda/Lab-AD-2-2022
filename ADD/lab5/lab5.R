library(MTS)
library(signal)
library(TSA)
library(oce)
library(ggplot2)
library(descomponer)
#------------------------------------------#

#--- LEYENDO Y DEJANDO LISTOS LOS DATOS ---#
datosNormo <- read.csv2("C:/Users/fabia/Desktop/Lab-AD-2-2022/ADD/lab5/TJ000.txt", 
#datos <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/02-2022/Análisis de datos/Laboratorio/TJ000.txt", 
                   sep = "\t",
                   header = FALSE)

datosHiper <- read.csv2("C:/Users/fabia/Desktop/Lab-AD-2-2022/ADD/lab5/TJ001.txt", 
                        #datos <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/02-2022/Análisis de datos/Laboratorio/TJ000.txt", 
                        sep = "\t",
                        header = FALSE)


PAM_Normo <- datosNormo[,1]
VFSC_Normo <- datosNormo[,3]
datos_Normo <- matrix(c(PAM_Normo,VFSC_Normo),ncol = 2)

PAM_Hiper <- datosHiper[,1]
VFSC_Hiper <- datosHiper[,3]
datos_Hiper <- matrix(c(PAM_Hiper,VFSC_Hiper),ncol = 2)

#Señales
PamNormoT <- as.numeric(ts(data = PAM_Normo, frequency = 5, deltat = 0.2))
VFSCNormoT <- as.numeric(ts(data = VFSC_Normo, frequency = 5, deltat = 0.2))

plot.ts(PamNormoT)
plot.ts(VFSCNormoT)

PamHiperT <- as.numeric(ts(data = PAM_Hiper, frequency = 5))
VFSCHiperT <- as.numeric(ts(data = VFSC_Hiper, frequency = 5))

plot.ts(PamHiperT)
plot.ts(VFSCHiperT)

#LOS GRAFICOS PARA PAM normocapnia
#periodograma
periodPAM_NormoT <- pwelch(PamNormoT, nfft=256, fs = 5)
#Validación cruzada 
rxy_Normo <- ccf(PamNormoT,VFSCNormoT,lag.max = 200,pl=TRUE)
wRxy_normo <- pwelch(rxy_Normo&acf, nfft = 256, fs = 5, plot = TRUE)

#auto correlacion
rxy_NormoAUTO <- acf(PamNormoT, lag.max = length(PamNormoT))

