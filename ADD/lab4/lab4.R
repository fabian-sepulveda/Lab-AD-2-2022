library(ggpubr)
library(dplyr)
library(stringr)
library(WRS2)
library(C50)
library(caret)
library(tidyverse)

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
datos <- datos%>% select(-"query hypothyroid")
#PARA PRUEBAS 
datos <- datos%>% select(-"T4U")

datos <- na.omit(datos)

# numeros <- rownames(datos)
# clases <- datos$class
# filas <- paste(numeros,"_",clases)


#----------------------------------------------------------------#
#----------------------------------------------------------------#
#TRANSFORMAR TODAS LAS VARIABLES A VALORES NUMERICOS

#Modificando variables dicotomicas

datos_trans <- datos

datos_trans$`on thyroxine` <- factor(ifelse(datos_trans$`on thyroxine` == "f",  FALSE,TRUE))
datos_trans$`on antithyroid medication` <- factor(ifelse(datos_trans$`on antithyroid medication` == "f", FALSE,TRUE))
datos_trans$sick <- factor(ifelse(datos_trans$sick == "f",FALSE,TRUE))
datos_trans$pregnant <- factor(ifelse(datos_trans$pregnant == "f", FALSE,TRUE))
datos_trans$`thyroid surgery` <- factor(ifelse(datos_trans$`thyroid surgery` == "f",FALSE,TRUE))
datos_trans$`I131 treatment` <- factor(ifelse(datos_trans$`I131 treatment` == "f", FALSE,TRUE))
datos_trans$lithium <- factor(ifelse(datos_trans$lithium == "f", FALSE,TRUE))
datos_trans$goitre <- factor(ifelse(datos_trans$goitre == "f", FALSE,TRUE))
datos_trans$tumor <- factor(ifelse(datos_trans$tumor == "f", FALSE,TRUE))
datos_trans$hypopituitary <- factor(ifelse(datos_trans$hypopituitary == "f", FALSE,TRUE))
datos_trans$psych <- factor(ifelse(datos_trans$psych == "f", FALSE,TRUE))


#discretizamos los niveles de hormonas y edades
# https://reactlab.com.ec/cientifico/guia-de-las-pruebas-de-hormonas-tiroideas-tsh-t4-y-t3/
datos_dis = datos_trans

datos_dis$age = cut(datos_dis$age, breaks = c(1,18,60,100),
                    labels = c("JOVEN","ADULTO","ADULTO MAYOR"))

datos_dis$TSH = cut(datos_dis$TSH, breaks = c(0,0.2,3,500),
                    labels = c("bajo","normal","alto"))
datos_dis$T3 = cut(datos_dis$T3, breaks = c(0,0.67,1.95,15),
                   labels = c("bajo","normal","alto"))

datos_dis$TT4 = cut(datos_dis$TT4, breaks = c(0,60,150,450), #rango en nmol
                   labels = c("bajo","normal","alto"))

datos_dis$FTI = cut(datos_dis$FTI, breaks = c(0,12,30,450), #rango en pmol
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

datos_dis <- datos_dis[datos_dis$class != "secondary hypothyroid", ]
colnames(datos_dis)[3] <- "onTH"
colnames(datos_dis)[4] <- "antiMED"
colnames(datos_dis)[7] <- "thyrSur"
colnames(datos_dis)[8] <- "l131"
data_index = createDataPartition(datos_dis$class, p=0.7)$Resample1
data_training = datos_dis[data_index, ]
data_test = datos_dis[-data_index, ]

data_training$class <- as.factor(data_training$class)
str(data_training)

data_test$class <- as.factor(data_test$class)
str(data_training)

arbol <- C5.0(data_training[-18], data_training$class) 
summary(arbol)

data_predicted <- predict(arbol, data_test)
confusionMatrix(data = data_predicted, reference = data_test$class, positive = "yes")
plot(arbol)
