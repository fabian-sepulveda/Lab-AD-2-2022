library(ggpubr)
library(dplyr)
library(stringr)
library(WRS2)
library(patchwork)
library(tidyverse)

library(cluster)
library(factoextra)
library(arulesViz)
#------------------------------------------#

#--- LEYENDO Y DEJANDO LISTOS LOS DATOS ---#

set.seed(123)

datos <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/02-2022/Análisis de datos/Laboratorio/allhypo.data", 
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

# datos$class[datos$class == "negative"] <- "N"
# datos$class[datos$class == "compensated hypothyroid"] <- "C"
# datos$class[datos$class == "secondary hypothyroid"] <- "S"
# datos$class[datos$class == "primary hypothyroid"] <- "P"

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

datos <- datos%>% select(-TBG)
datos <- datos%>% select(-"TBG measured")
datos <- datos%>% select(-"TSH measured")
datos <- datos%>% select(-"T3 measured")
datos <- datos%>% select(-"TT4 measured")
datos <- datos%>% select(-"FTI measured")
datos <- datos%>% select(-"T4U measured")
datos <- datos%>% select(-"referral source")
datos <- datos%>% select(-"query on thyroxine")
datos <- datos%>% select(-"query hyperthyroid")
datos <- datos%>% select(-"query hypothyroid")
#PARA PRUEBAS 
datos <- datos%>% select(-"T4U")
<<<<<<< HEAD
datos <- datos%>% select(-"TT4")
=======
>>>>>>> main
datos <- na.omit(datos)
datos$FTI <- datos$FTI/100

# numeros <- rownames(datos)
# clases <- datos$class
# filas <- paste(numeros,"_",clases)


#----------------------------------------------------------------#
#----------------------------------------------------------------#
#TRANSFORMAR TODAS LAS VARIABLES A VALORES NUMERICOS

#Modificando variables dicotomicas

datos_trans <- datos

datos_trans$`on thyroxine` <- ifelse(datos_trans$`on thyroxine` == "f", 0, 1)
datos_trans$`on antithyroid medication` <- ifelse(datos_trans$`on antithyroid medication` == "f", 0, 1)
datos_trans$sick <- ifelse(datos_trans$sick == "f", 0, 1)
datos_trans$pregnant <- ifelse(datos_trans$pregnant == "f", 0, 1)
datos_trans$`thyroid surgery` <- ifelse(datos_trans$`thyroid surgery` == "f", 0, 1)
datos_trans$`I131 treatment` <- ifelse(datos_trans$`I131 treatment` == "f", 0, 1)
datos_trans$lithium <- ifelse(datos_trans$lithium == "f", 0, 1)
datos_trans$goitre <- ifelse(datos_trans$goitre == "f", 0, 1)
datos_trans$tumor <- ifelse(datos_trans$tumor == "f", 0, 1)
datos_trans$hypopituitary <- ifelse(datos_trans$hypopituitary == "f", 0, 1)
datos_trans$psych <- ifelse(datos_trans$psych == "f", 0, 1)


#discretizamos los niveles de hormonas y edades
# https://reactlab.com.ec/cientifico/guia-de-las-pruebas-de-hormonas-tiroideas-tsh-t4-y-t3/
datos_dis = datos_trans

datos_dis$age = cut(datos_dis$age, breaks = c(1,18,60,100),
                    labels = c("NIÑO/JOVEN","ADULTO","ADULTO MAYOR"))

datos_dis$TSH = cut(datos_dis$TSH, breaks = c(0,0.2,3,500),
                    labels = c("bajo","normal","alto"))
datos_dis$T3 = cut(datos_dis$T3, breaks = c(0,0.67,1.95,15),
                   labels = c("bajo","normal","alto"))
<<<<<<< HEAD
datos_dis$FTI = cut(datos_dis$FTI, breaks = c(0,0.8,2,450),
=======
datos_dis$TT4 = cut(datos_dis$TT4, breaks = c(0,4.4,11.6,450),
>>>>>>> main
                   labels = c("bajo","normal","alto"))

datos_dis$FTI = cut(datos_dis$FTI, breaks = c(0,0.8,2.24,450),
                    labels = c("bajo","normal","alto"))

#_______________________________________________________________________________
# IDEA 
# VER LOS SOPORTES MÁS INTERESANTES DE CADA VARIABLE
# VER LAS REGLAS MAS INTERESANTES, TOMAR 5 DE CADA CLASE Y OTRAS 5 DE ALGO INTERESANTE
# OTRAS INTERESANTES PUEDEN SER QUE AFECTA AL SEXO O LA EDAD
# FINALMENTE SE PUEDE HACER EL ANALISIS DE LA REGLA OBTENIDA, REALIZANDO EL CONTRASTE CON LA REALIDAD
# EN BASE A LOS NIVELES DE HORMONAS, BUSCAR ALGUN PAPER QUE DIGA ALGO SOBRE LA AFECCION A MUJERES EMBARAZADAS
# ETC ETC... ES UNA IDEA PERO LO DEJO AQUI PARA NO PERDERLO
#_______________________________________________________________________________

#CALCULO DE SOPORTES
total_Filas <- 2028
#Soporte Variables

#Operaciones/metodos
frec1<- table(datos_dis$`on thyroxine` == 0)
frec2 <- table(datos_dis$`on antithyroid medication` == 0)
frec3 <- table(datos_dis$sick == 0)
frec4 <- table(datos_dis$`thyroid surgery` == 0)
frec5 <- table(datos_dis$`I131 treatment` == 0)
frec6 <- table(datos_dis$lithium == 0)
frec7 <- table(datos_dis$goitre == 0)
frec8 <- table(datos_dis$tumor == 0)
frec9 <- table(datos_dis$hypopituitary == 0)
frec10 <- table(datos_dis$psych == 0)


soporte1 <- frec1[2]/total_Filas
soporte2 <- frec2[2]/total_Filas
soporte3 <- frec3[2]/total_Filas
soporte4 <- frec4[2]/total_Filas
soporte5 <- frec5[2]/total_Filas
soporte6 <- frec6[2]/total_Filas
soporte7 <- frec7[2]/total_Filas
soporte8 <- frec8[2]/total_Filas
soporte9 <- frec9[2]/total_Filas
soporte10 <- frec10[2]/total_Filas

table_soporte_var <- data.frame(soporte1,soporte2,soporte3,soporte4,soporte5,soporte6,soporte7,soporte8,soporte9,soporte10)
colnames(table_soporte_var) <- c("on thyroxine","on antithyroid medication","sick","thyroid surgery","I131 treatment","lithium","goitre","tumor",
                             "hypopituitary","psych")
rownames(table_soporte_var) <- "=0"
print(table_soporte_var)

#Soporte clases
frec_class_negative <- table(datos_dis$class == "negative")
soporte_negative <- frec_class_negative[2]/total_Filas

frec_class_com <- table(datos_dis$class == "compensated hypothyroid")
soporte_com <- frec_class_com[2]/total_Filas

frec_class_primary <- table(datos_dis$class == "primary hypothyroid")
soporte_primary <- frec_class_primary[2]/total_Filas

frec_class_secundary <- table(datos_dis$class == "secondary hypothyroid")
soporte_secundary <- frec_class_secundary[2]/total_Filas

table_soporte_class <- data.frame(soporte_negative,soporte_com,soporte_primary,soporte_secundary)
colnames(table_soporte_class) <- c("N","C","P","S")
rownames(table_soporte_class) <- "Numeros"
print(table_soporte_class)

#Soporte hormonas
frec_TSH <- table(datos_dis$TSH == "normal")
frec_T3 <- table(datos_dis$T3 == "normal") 
frec_TT4 <- table(datos_dis$TT4 == "normal") 
frec_FTI <- table(datos_dis$FTI == "normal") 

soporte_TSH <- frec_TSH[2]/total_Filas
soporte_T3 <- frec_T3[2]/total_Filas
soporte_TT4 <- frec_TT4[2]/total_Filas
soporte_FTI <- frec_FTI[2]/total_Filas

table_soporte_Hormona <- data.frame(soporte_TSH,soporte_T3,soporte_TT4,soporte_FTI)
colnames(table_soporte_Hormona) <- c("TSH","T3","TT4","FTI")
rownames(table_soporte_Hormona) <- "Niveles"
print(table_soporte_Hormona)

#Calculo de reglas
rules = apriori(
  data = datos_dis, 
  parameter=list(target="rules")
)

rules2 = apriori(
  data = datos_dis, 
  parameter=list(support = 0.3, confidence = 0.7, minlen = 3, maxlen = 6, target="rules")
)


rules_negative = apriori(
  data = datos_dis, 
  parameter=list(support = 0.3, confidence = 0.7, minlen = 3, maxlen = 6, target="rules"),
  appearance=list(rhs = "class=negative")
)

<<<<<<< HEAD

rules_primary_2 = apriori(
  data = datos_dis,
  parameter=list(support = 0.3, minlen = 4, maxlen = 6, target="rules"),
  appearance=list(rhs = "class=primary hypothyroid")
)

inspect(sort(x = rules_primary_2, decreasing = FALSE, by = "confidence")[1:20])

inspect(sort(x = rules, decreasing = FALSE, by = "confidence")[1:20])

inspect(sort(x = rules_negative, decreasing = FALSE, by = "confidence")[1:20])

inspect(sort(x = rules_secondary, decreasing = FALSE, by = "confidence"))

inspect(sort(x = rules_primary, decreasing = FALSE, by = "confidence")[1:20])

inspect(sort(x = rules_compensated, decreasing = FALSE, by = "confidence")[1:20])

=======
>>>>>>> main

inspect(sort(x = rules2, decreasing = FALSE, by = "support")[1:50])


