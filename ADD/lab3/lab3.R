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

datos <- read.csv2("C:/Users/fabia/Desktop/Lab-AD-2-2022/ADD/lab1/allhypo.data", 
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

#PARA PRUEBAS 
datos <- datos%>% select(-"T4U")
datos <- datos%>% select(-"FTI")
datos <- na.omit(datos)

# numeros <- rownames(datos)
# clases <- datos$class
# filas <- paste(numeros,"_",clases)


#----------------------------------------------------------------#
#----------------------------------------------------------------#
#TRANSFORMAR TODAS LAS VARIABLES A VALORES NUMERICOS

#Modificando variables dicotomicas

datos_trans <- datos

datos_trans$`on thyroxine` <- ifelse(datos_trans$`on thyroxine` == "f", 0, 1)
datos_trans$`query hypothyroid` <- ifelse(datos_trans$`query hypothyroid` == "f", 0, 1)
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

datos_dis = datos_trans

datos_dis$age = cut(datos_dis$age, breaks = c(1,18,60,500),
                    labels = c("NIÑO/JOVEN","ADULTO","ADULTO MAYOR"))

datos_dis$TSH = cut(datos_dis$TSH, breaks = c(0,0.4,5.8,500),
                    labels = c("bajo","normal","alto"))
datos_dis$T3 = cut(datos_dis$T3, breaks = c(0,0.8,2,15),
                   labels = c("bajo","normal","alto"))
datos_dis$TT4 = cut(datos_dis$TT4, breaks = c(0,4,11,450),
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


rules = apriori(
  data = datos_dis, 
  parameter=list(support = 0.1, minlen = 2, maxlen = 6, target="rules")
)

rules_negative = apriori(
  data = datos_dis, 
  parameter=list(support = 0.003, minlen = 2, maxlen = 6, target="rules"),
  appearance=list(rhs = "class=negative")
)

rules_secondary = apriori(
  data = datos_dis, 
  parameter=list(support = 0.1, minlen = 2, maxlen = 6, target="rules"),
  appearance=list(rhs = "class=secondary hypothyroid")
)

rules_primary = apriori(
  data = datos_dis, 
  parameter=list(support = 0.003, minlen = 2, maxlen = 6, target="rules"),
  appearance=list(rhs = "class=primary hypothyroid")
)

rules_compensated = apriori(
  data = datos_dis, 
  parameter=list(support = 0.003, minlen = 2, maxlen = 6, target="rules"),
  appearance=list(rhs = "class=compensated hypothyroid")
)

inspect(sort(x = rules, decreasing = FALSE, by = "confidence")[1:20])

inspect(sort(x = rules_negative, decreasing = FALSE, by = "confidence")[1:20])

inspect(sort(x = rules_secondary, decreasing = FALSE, by = "confidence"))

inspect(sort(x = rules_primary, decreasing = FALSE, by = "confidence")[1:20])

inspect(sort(x = rules_compensated, decreasing = FALSE, by = "confidence")[1:20])




