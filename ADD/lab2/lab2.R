library(ggpubr)
library(dplyr)
library(stringr)
library(WRS2)
library(patchwork)
library(tidyverse)

library(cluster)
library(factoextra)
library(vegan)
#------------------------------------------#
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

#--- LEYENDO Y DEJANDO LISTOS LOS DATOS ---#

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
datos <- na.omit(datos)

#----------------------------------------------------------#
#REVISANDO SOLO LAS VARIABLES NUMÉRICAS

#variables numericas

datos_numericos <- data.frame("Edad"=datos$age,
                              "TSH"= datos$TSH,
                              "T3" = datos$T3,
                              "T4U" =datos$T4U,
                              "TT4" =datos$TT4)


#Normalizando
datos_numericos <- scale(datos_numericos)
colnames(datos_numericos) <- c('Edad','TSH','T3',"T4U","TT4")




#Calculando distancia
distancia_euc <- dist(datos, method = "euclidean")
distancia_man <- dist(datos, method = "manhattan")

#Analizando cantidad optima de clusters
fviz_nbclust(x = datos_numericos, FUNcluster = kmeans, method = "wss", diss = distancia_euc)
fviz_nbclust(x = datos_numericos, FUNcluster = kmeans, method = "silhouette",diss = distancia_euc)

fviz_nbclust(x = datos_numericos, FUNcluster = kmeans, method = "wss", diss = distancia_man)
fviz_nbclust(x = datos_numericos, FUNcluster = kmeans, method = "silhouette",diss = distancia_man)


# k means

kmeans_6 <- kmeans(datos_numericos, 6, iter.max = 1000, nstart = 10)
kmeans_7 <- kmeans(datos_numericos, 7, iter.max = 1000, nstart = 10)
kmeans_8 <- kmeans(datos_numericos, 8, iter.max = 1000, nstart = 10)


#----------------------------------------------------------------#
#----------------------------------------------------------------#
#TRANSFORMAR TODAS LAS VARIABLES A VALORES NUMERICOS

#Normalizar los datos

datos_transformados <- datos

datos_transformados <- datos_transformados %>% 
  mutate(sex = paste("sex", sex, sep = "_"),
  valor_sex = 1,
  
  "on thyroxine" = paste("on thyroxine",`on thyroxine`, sep = "_" ),
  valor_thyroxine = 1,
  
  "query on thyroxine" = paste("query on thyroxine", `query on thyroxine`, sep = "_"),
  valor_query_thy = 1) %>% 
    spread(key = sex, value = valor_sex,fill = 0) %>%
    spread(key = `on thyroxine`, value = valor_thyroxine,fill = 0) %>%
    spread(key = `query on thyroxine`, value = valor_query_thy, fill = 0)





#-------------------------------------------------------------------------------------------#

#----------------------------------------------------------------#
#----------------------------------------------------------------#
#REALIZANDO ANALISIS SIN TRANSFORMAR LAS VARIABLES CATEGORICAS
#Normalizar los datos


datos_normalizados <- datos
datos_normalizados$age <- scale(datos$age)
datos_normalizados$FTI <- scale(datos$FTI)

prueba <- sample_n(datos,20)

#Distancias
gower_dist <- daisy(datos, metric = "gower", type=list(ordratio = c(2, 27,28), 
                                                       symm = c(3:17,19,21,23,25),
                                                       logratio = c(1,26)))
gower_dist2 <- daisy(datos, metric = "gower", type=list(ordratio = c(2, 27,28), 
                                                        symm = c(3:17,19,21,23,25)))
summary(gower_dist)
summary(gower_dist2)

#gower_dist <- daisy(prueba, metric = "gower", type=list(ordratio = c(2, 27,28), symm = c(3:17,19,21,23,25)))
#gower_dist <- daisy(datos, metric = "gower", type=list(ordratio = c(2, 27,28), symm = c(3:17,19,21,23,25)))



#Estimar numero de cluster
#fviz_nbclust(x = , FUNcluster = pam, method = "wss", diss = distancia)


#Dendograma
clustering_jerarquico <- hclust(gower_dist, method = "ward.D2")
plot(clustering_jerarquico)

rect.hclust(clustering_jerarquico, k = 6, border = "green")

fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 15,
             diss = gower_dist)




#-------------------------------------------------------------------------------------------#


