library(ggpubr)
library(dplyr)
library(stringr)
library(WRS2)
library(patchwork)

#--- LEYENDO Y DEJANDO LISTOS LOS DATOS ---#

datos <- read.csv2("C:/Users/fabia/Desktop/ADD/lab1/allhypo.data", 
                   sep = ",",
                   header = FALSE)
colnames(datos) <- c("age",
                     "sex",
                     "on thyroxine",
                     "query on thyroxine",
                     "on antithyroid medication",
                     "sick",
                     "pregnant",
                     "thyroid surgery",
                     "I131 treatment",
                     "query hypothyroid",
                     "query hyperthyroid",
                     "lithium",
                     "goitre",
                     "tumor",
                     "hypopituitary",
                     "psych",
                     "TSH measured",
                     "TSH",
                     "T3 measured",
                     "T3",
                     "TT4 measured",
                     "TT4",
                     "T4U measured",
                     "T4U",
                     "FTI measured",
                     "FTI",
                     "TBG measured",
                     "TBG",
                     "referral source",
                     "class")



clases <- str_split(datos[["class"]], "\\.\\|", simplify = TRUE)
datos["class"] <- data.frame(clases[,1])

edad <- datos$age
edad_nuevo <- as.numeric(edad)

TSH <- datos$TSH
TSH_nuevo <- as.numeric(TSH)

T3 <- datos$T3
T3_nuevo <- as.numeric(T3)

TT4 <- datos$TT4
TT4_nuevo <- as.numeric(TT4)

T4U <- datos$T4U
T4U_nuevo <- as.numeric(T4U)

FTI <- datos$FTI
FTI_nuevo <- as.numeric(FTI)

TBG <- datos$TBG
TBG_nuevo <- as.numeric(TBG)


datos$age <- edad_nuevo
datos$TSH <- TSH_nuevo
datos$T3 <- T3_nuevo
datos$TT4 <- TT4_nuevo
datos$T4U <- T4U_nuevo
datos$FTI <- FTI_nuevo
datos$TBG <- TBG_nuevo
datos <- datos %>% filter(datos$age < 150)

set.seed(1)

hypo_primaria   <- datos[datos$class=="primary hypothyroid",]
hypo_secundaria <- datos[datos$class=="secondary hypothyroid",]
hypo_comp       <- datos[datos$class=="compensated hypothyroid",]
hypo_negativo   <- datos[datos$class=="negative",]



#-------------------------- Análisis --------------------------------#

#--------------------------------------------------------------------#
#                Edades                   #
#--------------------------------------------------------------------#

# --- Edades de todos los pacientes --- #
hist(datos$age,
     xlab = "Edades",
     ylab = "Frecuencia",
     main = "Gráfico de edades")

# --- Edades de los pacientes con hipotiroidismo --- #
hipotiroidismo <- datos %>% filter(datos$class != "negative")
hist(hipotiroidismo$age,
     xlab = "Edades",
     ylab = "Frecuencia",
     main = "Gráfico de edades")

# DISCULPA COMENTE TODO PORQUE ME TIRABA MUCHAS VARIABLES 

hormonas <- c("TSH","T4","T3")
clases <- c("Poblacón", "Primaria", "Secundaria", "Compensado", "Negativo")
# --- MEDIA --- #

media_TSH_poblacion <- mean(datos$TSH, na.rm = TRUE)
media_TSH_primaria <- mean(hypo_primaria$TSH, na.rm = TRUE)
media_TSH_secundaria <- mean(hypo_secundaria$TSH, na.rm = TRUE)
media_TSH_compensada <- mean(hypo_comp$TSH, na.rm = TRUE)
media_TSH_negativo <- mean(hypo_negativo$TSH, na.rm = TRUE)

medias_TSH_totales <- c(media_TSH_poblacion,media_TSH_primaria,media_TSH_secundaria,media_TSH_compensada,media_TSH_negativo)

media_T4_poblacion <- mean(datos$TT4, na.rm = TRUE)
media_T4_primaria <- mean(hypo_primaria$TT4, na.rm = TRUE)
media_T4_secundaria <- mean(hypo_secundaria$TT4, na.rm = TRUE)
media_T4_compensada <- mean(hypo_comp$TT4, na.rm = TRUE)
media_T4_negativo <- mean(hypo_negativo$TT4, na.rm = TRUE)
  
medias_T4_totales <- c(media_T4_poblacion,media_T4_primaria,media_T4_secundaria,media_T4_compensada, media_T4_negativo)

media_T3_poblacion <- mean(datos$T3, na.rm = TRUE)
media_T3_primaria <- mean(hypo_primaria$T3, na.rm = TRUE)
media_T3_secundaria <- mean(hypo_secundaria$T3, na.rm = TRUE)
media_T3_compensada <- mean(hypo_comp$T3, na.rm = TRUE)
media_T3_negativo <- mean(hypo_negativo$T3, na.rm = TRUE)

medias_T3_totales <- c(media_T3_poblacion,media_T3_primaria,media_T3_secundaria,media_T3_compensada, media_T3_negativo)

#HACER TABLA
tabla_medias <- matrix(c(medias_TSH_totales,medias_T4_totales,medias_T3_totales), ncol = 3, byrow = TRUE)
colnames(tabla_medias) <- hormonas
rownames(tabla_medias) <- clases
tabla_medias <- as.table(tabla_medias)
print(tabla_medias)

 

# --- DESVIACIÓN ESTÁNDAR --- #

ds_TSH_poblacion <- round(sd(datos$TSH, na.rm = TRUE),2)
ds_TSH_primaria <- round(sd(hypo_primaria$TSH, na.rm = TRUE),2)
ds_TSH_secundaria <- round(sd(hypo_secundaria$TSH, na.rm = TRUE),2)
ds_TSH_compensada <- round(sd(hypo_comp$TSH, na.rm = TRUE),2)
ds_TSH_negativo <- round(sd(hypo_negativo$TSH, na.rm = TRUE),2)
  
ds_TSH_totales <- c(ds_TSH_poblacion,ds_TSH_primaria,ds_TSH_secundaria,ds_TSH_compensada,ds_TSH_negativo)


ds_T4_poblacion <- round(sd(datos$TT4, na.rm = TRUE),2)
ds_T4_primaria <- round(sd(hypo_primaria$TT4, na.rm = TRUE),2)
ds_T4_secundaria <- round(sd(hypo_secundaria$TT4, na.rm = TRUE),2)
ds_T4_compensada <- round(sd(hypo_comp$TT4, na.rm = TRUE),2)
ds_T4_negativo <- round(sd(hypo_negativo$TT4, na.rm = TRUE),2)
  
ds_T4_totales <- c(ds_T4_poblacion,ds_T4_primaria,ds_T4_secundaria,ds_T4_compensada,ds_T4_negativo)

ds_T3_poblacion <- round(sd(datos$T3, na.rm = TRUE),2)
ds_T3_primaria <- round(sd(hypo_primaria$T3, na.rm = TRUE),2)
ds_T3_secundaria <- round(sd(hypo_secundaria$T3, na.rm = TRUE),2)
ds_T3_compensada <- round(sd(hypo_comp$T3, na.rm = TRUE),2)
ds_T3_negativo <- round(sd(hypo_negativo$T3, na.rm = TRUE),2)

ds_T3_totales <- c(ds_T3_poblacion,ds_T3_primaria,ds_T3_secundaria,ds_T3_compensada,ds_T3_negativo)

#HACER TABLA
tabla_ds <- matrix(c(ds_TSH_totales,ds_T4_totales,ds_T3_totales), ncol = 3, byrow = TRUE)
colnames(tabla_ds) <- hormonas
rownames(tabla_ds) <- clases
tabla_ds <- as.table(tabla_ds)
print(tabla_ds)


# --- Prueba t para los niveles TSH --- #


#Ahora rezar para arriba y que se pueda usar la prueba t para muestras independientes
# LECTURA 5
# NL : Niveles de TSH para pacientes con tratamiento de litio
# NN : Niveles de TSH para pacientes sin hipotiroidismo

#H0 : No hay diferencia en los niveles de TSH para ambos grupos
#HA : NL presenta, en promedio, mayores niveles de TSH que NN
alfa <- 0.05
litio <- datos %>% filter(datos$lithium == "t")
hypo_primaria_litio <- sample_n(datos %>% filter(datos$lithium == "t"), 10)
TSH_litio <- hypo_primaria_litio$TSH

hypo_negativo   <- sample_n((datos[datos$class=="negative",]), 10)
TSH_negativo <- hypo_negativo$TSH

normalidad_A <- shapiro.test(TSH_litio)
print(normalidad_A)
normalidad_B <- shapiro.test(TSH_negativo)
print(normalidad_B)

#SIIII hay normalidad

prueba <- t.test(x = TSH_litio,
                 y = TSH_negativo,
                 paired = FALSE,
                 alternative = "greater",
                 mu = 0,
                 conf.level = 1-alfa)
print(prueba)


# #--------------------------------------------------------------------#
# #                TSH por clase de hipotiroidismo                     #
# #--------------------------------------------------------------------#
# 
# # -- Agrupando por el tipo de hipotiroidismo -- #
# 
# TSH_datos       <- datos[datos$"TSH measured"=="t",]
# 
# 
# hypo_primaria   <- TSH_datos[TSH_datos$class=="primary hypothyroid",]
# hypo_secundaria <- TSH_datos[TSH_datos$class=="secondary hypothyroid",]
# hypo_comp       <- TSH_datos[TSH_datos$class=="compensated hypothyroid",]
# hypo_negativo   <- TSH_datos[TSH_datos$class=="negative",]
# 
# ggplot(data = hypo_primaria) + 
#   geom_density(aes(x=TSH,fill=factor(class)), position = "identity",alpha = 0.5) + 
#   geom_vline(data=TSH_datos, aes(xintercept=0.37),linetype="dashed") + 
#   geom_vline(data=TSH_datos, aes(xintercept=4.7),linetype="dashed") +
#   scale_x_continuous(limits = c(0, 300)) #Limite del eje x
# 
# ggplot(data = hypo_secundaria) + 
#   geom_density(aes(x=TSH,fill=factor(class)), position = "identity",alpha = 0.5) + 
#   geom_vline(data=TSH_datos, aes(xintercept=0.37),linetype="dashed") + 
#   geom_vline(data=TSH_datos, aes(xintercept=4.7),linetype="dashed") +
#   scale_x_continuous(limits = c(0, 300)) #Limite del eje x
# # -- Gráfico de densidad/TSH por cada clase de hipotiroidismo -- #
# 
# ggplot(data = TSH_datos) + 
#   geom_density(aes(x=TSH,fill=factor(class)), position = "identity",alpha = 0.5) + 
#   geom_vline(data=TSH_datos, aes(xintercept=0.37),linetype="dashed") + 
#   geom_vline(data=TSH_datos, aes(xintercept=4.7),linetype="dashed") +
#   scale_x_continuous(limits = c(0, 300)) #Limite del eje x
# 
# # -- Gráfico de cajas/TSH por cada clase de hipotiroidismo -- #
# 
# ggplot(data = TSH_datos, aes(x = class, y = TSH, fill = class )) +
#   geom_boxplot(alpha=1) + 
#   theme(legend.position="none") +
#   xlab("") + ylab("Nivel de TSH")
# 
# # -- Gráfico de cajas/TSH por cada genero                   -- #
# 
# ggplot(data = TSH_datos, aes(x = sex, y = TSH, fill = sex )) +
#   geom_boxplot(alpha=1) + 
#   theme(legend.position="none") +
#   xlab("") + ylab("Nivel de TSH")
# 
# # -- Gráfico de cajas/TSH por cada clase de hipotiroidismo -- #
# 
# a <- TSH_datos %>% filter(TSH_datos$class != "negative")
# a <- a %>% filter(a$"thyroid surgery" == "t")
# b <- TSH_datos %>% filter(TSH_datos$class == "negative")
# b <- b %>% filter(b$"thyroid surgery" == "f")
# 
# 
# ggplot(data = TSH_datos) + 
#   geom_density(aes(x=TSH,fill="thyroid surgery"), position = "identity",alpha = 0.5) + 
#   geom_vline(data=TSH_datos, aes(xintercept=0.37),linetype="dashed") + 
#   geom_vline(data=TSH_datos, aes(xintercept=4.7),linetype="dashed") +
#   scale_x_continuous(limits = c(0, 300)) #Limite del eje x
# 
# 
# #-------------------------------------------------------------------#
# #                             Operado                               #
# #-------------------------------------------------------------------#
# 
# operados <- datos %>% filter(datos$"thyroid surgery" == "t")
# 
# 
# 
# 
# #-------------------------------------------------------------------#
# #                   Hipotiroidismo por género                       #
# #-------------------------------------------------------------------#
# 
# hipotiroidismo <- datos %>% filter(datos$class != "negative")
# hipotiroidismo <- hipotiroidismo[is.na(hipotiroidismo$age) == FALSE,]
# hipotiroidismo <- hipotiroidismo[hipotiroidismo$sex != "?",]
# 
# ggplot(data = hipotiroidismo, aes(x = sex, y = age, fill = sex )) +
#   geom_boxplot(alpha=1) + 
#   theme(legend.position="none") +
#   xlab("") + ylab("Edad") +
#   geom_jitter()
# 
# 
# 
# #-------------------------------------------------------------------#
# #                  Hipotiroidismo con tiroxina                      #
# #-------------------------------------------------------------------#
# 
# hip_en_tirox  <- datos %>% filter(datos$class != "negative")
# hip_en_tirox  <- hip_en_tirox[hip_en_tirox$"on thyroxine" == "t",]
# hip_en_tirox  <- hip_en_tirox$TSH
# 
# hip_sin_tirox <- datos %>% filter(datos$class != "negative")
# hip_sin_tirox <- hip_sin_tirox[hip_sin_tirox$"on thyroxine" == "f",]
# hip_sin_tirox <- hip_sin_tirox$TSH
# 
# 
# neg_TSH       <- datos %>% filter(datos$class == "negative")
# neg_TSH       <- neg_TSH %>% filter(neg_TSH$"TSH measured" == "t")
# neg_TSH       <- neg_TSH$TSH
# neg_TSH       <- sample(neg_TSH, 50)
# 
# 
# valores      <- c(neg_TSH, hip_en_tirox, hip_sin_tirox)
# tipo         <- c(rep("Negativo", length(neg_TSH)),rep("En tiroxina", length(hip_en_tirox)),
#                   rep("Sin tiroxina", length(hip_sin_tirox)))
# 
# neg_tirox    <- data.frame(tipo,valores)
# 
# 
# ggplot(data = neg_tirox) + 
#   geom_density(aes(x=valores,fill=factor(tipo)), position = "identity",alpha = 0.5) + 
#   geom_vline(data=TSH_datos, aes(xintercept=0.37),linetype="dashed") + 
#   geom_vline(data=TSH_datos, aes(xintercept=4.7),linetype="dashed") +
#   scale_x_continuous(limits = c(0, 300)) #Limite del eje x






