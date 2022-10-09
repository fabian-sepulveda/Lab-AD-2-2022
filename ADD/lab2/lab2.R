library(ggpubr)
library(dplyr)
library(stringr)
library(WRS2)
library(patchwork)
library(tidyverse)

library(cluster)
library(factoextra)
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

datos$class[datos$class == "negative"] <- "N"
datos$class[datos$class == "compensated hypothyroid"] <- "C"
datos$class[datos$class == "secondary hypothyroid"] <- "S"
datos$class[datos$class == "primary hypothyroid"] <- "P"

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
datos <- na.omit(datos)

numeros <- rownames(datos)
clases <- datos$class
filas <- paste(numeros,"_",clases)

datos <- datos%>% select(-"class")


#----------------------------------------------------------------#
#----------------------------------------------------------------#
#TRANSFORMAR TODAS LAS VARIABLES A VALORES NUMERICOS

#Modificando variables dicotomicas

datos_transformados <- datos

datos_transformados <- datos_transformados %>% 
  mutate(
  sex = paste("sex", sex, sep = "_"),
  valor_sex = 1,
  
  "on thyroxine" = paste("on thyroxine",`on thyroxine`, sep = "_" ),
  valor_thyroxine = 1,
  
  "query hypothyroid" = paste("query hypothyroid",`query hypothyroid`, sep = "_" ),
  valor_query_hypo = 1,
  
  "on antithyroid medication" = paste("on antithyroid medication", `on antithyroid medication`, sep = "_"),
  valor_anti_thy = 1,
  
  "sick" = paste("sick", sick, sep = "_"),
  valor_sick = 1,
  
  "pregnant" = paste("pregnant", `pregnant`, sep = "_"),
  valor_pregnat = 1,
  
  "thyroid surgery" = paste("thyroid surgery", `thyroid surgery`, sep = "_"),
  valor_surgery = 1,
  
  "I131 treatment" = paste("I131 treatment", `I131 treatment`, sep = "_"),
  valor_I131 = 1,
  
  "lithium" = paste("lithium", `lithium`, sep = "_"),
  valor_lit = 1,
  
  "goitre" = paste("goitre", `goitre`, sep = "_"),
  valor_goit = 1,
  
  "tumor" = paste("tumor", `tumor`, sep = "_"),
  valor_tumor = 1,
  
  "hypopituitary" = paste("hypopituitary", `hypopituitary`, sep = "_"),
  valor_hypopi = 1,
  
  "psych" = paste("psych", `psych`, sep = "_"),
  valor_psych = 1
  
  ) %>% 
    spread(key = sex, value = valor_sex,fill = 0) %>%
    spread(key = `query hypothyroid`, value = valor_query_hypo,fill = 0) %>%
    spread(key = `on thyroxine`, value = valor_thyroxine,fill = 0) %>%
    spread(key = `on antithyroid medication`, value = valor_anti_thy, fill = 0) %>%
    spread(key = sick, value = valor_sick,fill = 0) %>%
    spread(key = pregnant, value = valor_pregnat,fill = 0) %>%
    spread(key = `thyroid surgery`, value = valor_surgery,fill = 0) %>%
    spread(key = `I131 treatment`, value = valor_I131,fill = 0) %>%
    spread(key = lithium, value = valor_lit,fill = 0) %>%
    spread(key = goitre, value = valor_goit,fill = 0) %>%
    spread(key = tumor, value = valor_tumor,fill = 0) %>%
    spread(key = hypopituitary, value = valor_hypopi,fill = 0) %>%
    spread(key = psych, value = valor_psych,fill = 0) 
  
  

rownames(datos_transformados) <- filas



#---------------------------------------------#


#Distancias
gower_dist <- daisy(datos_transformados, metric = "gower", type = list(symm = c(7:33),
                                                                       logratio = c(1:6))) 


#-----------Estimación jerárquica------------

#Método jerárquico

arbol <- hclust(d = gower_dist,
                method = "ward.D2")
arbol2 <- hclust(d = gower_dist,
                 method = "complete")
arbol3 <- hclust(d = gower_dist,
                 method = "average")


#-------------- GRAFICANDO -------------------#

###################################
dendrograma1 <- fviz_dend(arbol,  main = "Método ''Ward.D2''", cex = 0)
dendrograma2 <- fviz_dend(arbol2, main = "Método ''Complete''", cex = 0)
dendrograma3 <- fviz_dend(arbol3, main = "Método ''Average''",cex = 0) 

dendrograma1 + dendrograma2 + dendrograma3
###################################
# Cree un gráfico de todo el dendrograma
# y estraiga los datos del dendrograma

dendrograma <- fviz_dend(arbol, k=4, cex = 0)

dend_data <- attr(dendrograma, "dendrogram") #  Extraer datos de dendrogramas

# Cortar el dendrograma a la altura h = 10
dend_cuts <- cut(dend_data, h = 5)

#Visualizando los grupos
sub_1 <- fviz_dend(dend_cuts$lower[[1]][[1]], main = "Subtree 1")
sub_2 <- fviz_dend(dend_cuts$lower[[1]][[2]], main = "Subtree 2")
sub_3 <- fviz_dend(dend_cuts$lower[[2]][[1]], main = "Subtree 3")
sub_4 <- fviz_dend(dend_cuts$lower[[2]][[2]], main = "Subtree 4")

#---------------------------No jerarquico--------------------
#Método no jerarquico

wss <- fviz_nbclust(x = datos_transformados, FUNcluster = pam, method = "wss", diss = gower_dist)
silueta <- fviz_nbclust(x = datos_transformados, FUNcluster = pam, method = "silhouette", diss = gower_dist)


pam_clusters_2 <- pam(x = datos_transformados, k = 2)
pam_clusters_4 <- pam(x = datos_transformados, k = 4)
pam_clusters_6 <- pam(x = datos_transformados, k = 6)



#Grafico mas cuadrado

#------ K = 2 ------#
cluster_k2 <- fviz_cluster(object = pam_clusters_2,
             repel = TRUE, 
             show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "-------",
       subtitle = "Distancia gower, K=2") +
  theme_bw() +
  theme(legend.position = "bottom")


#------ K = 4 ------#
cluster_k4 <- fviz_cluster(object = pam_clusters_4,
                           repel = TRUE, 
                           show.clust.cent = FALSE,
                           labelsize = 8)  +
  labs(title = "-------",
       subtitle = "Distancia gower, K=4") +
  theme_bw() +
  theme(legend.position = "bottom")

#------ K = 6 ------#
cluster_k6 <- fviz_cluster(object = pam_clusters_6,
             repel = TRUE, 
             show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "-------",
       subtitle = "Distancia gower, K=6") +
  theme_bw() +
  theme(legend.position = "bottom")

#print(cluster_k2)
#print(cluster_k4)
#print(cluster_k6)





#-------------------------------------------------------------#
#Evaluar clasificacion de los cluster


datos_cluster2 <- cluster_k2[["data"]][["name"]]

individuos <- str_split(datos_cluster2, " _ ", simplify = TRUE)
individuos <- individuos[,2]
individuos[individuos == "N"] <- "negative"
individuos[individuos == "C"] <- "compensated hypothyroid"
individuos[individuos == "S"] <- "secondary hypothyroid"
individuos[individuos == "P"] <- "primary hypothyroid"

individuos_clases <- data.frame("class"= individuos)

datos_cluster <- datos
datos_cluster <- cbind(datos_cluster,individuos_clases)


levels_cluster2 <- cluster_k2[["data"]][["cluster"]]
cluster_clasificacion_2 <- data.frame(datos_cluster,levels_cluster2)
cluster_clasificacion_2_v <- data.frame(individuos,levels_cluster2)
tabla_2 <- table(cluster_clasificacion_2_v)

cluster_clasificacion_2_1 <- cluster_clasificacion_2[cluster_clasificacion_2$levels_cluster2 == 1,]
cluster_clasificacion_2_2 <- cluster_clasificacion_2[cluster_clasificacion_2$levels_cluster2 == 2,]



Conf2x1_EDAD = matrix(c(1:2), nrow=1, byrow=TRUE)
layout(Conf2x1_EDAD)
hist(cluster_clasificacion_2_1$age, xlab = "EDADES", main = "EDAD EN k=1")
hist(cluster_clasificacion_2_2$age, xlab = "EDADES", main = "EDAD EN k=2")

#GENERO
tabla2_1 <- table(cluster_clasificacion_2_1$sex)
tabla2_2 <- table(cluster_clasificacion_2_2$sex)
Tablas_sex <- rbind(tabla2_1,tabla2_2)

#TSH
Conf1x2_TSH = matrix(c(1:2), nrow=1, byrow=TRUE)
layout(Conf1x2_TSH)
hist(cluster_clasificacion_2_1$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=1")
hist(cluster_clasificacion_2_2$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=2")

#T4
Conf1x2_T4 = matrix(c(1:2), nrow=1, byrow=TRUE)
layout(Conf1x2_T4)
hist(cluster_clasificacion_2_1$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=1")
hist(cluster_clasificacion_2_2$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=2")


#T3
Conf1x2_T4 = matrix(c(1:2), nrow=1, byrow=TRUE)
layout(Conf1x2_T4)
hist(cluster_clasificacion_2_1$T3, xlab = "Niveles de T3", main = "T3 en cluster K=1")
hist(cluster_clasificacion_2_2$T3, xlab = "Niveles de T3", main = "T3 en cluster K=2")





nombre_2k_cluster <- c("Cluster 1", "Cluster 2")
datos_2k <- data.frame("on thyroxine" = c(sum(cluster_clasificacion_2_1$"on.thyroxine" == "t"),
                                          sum(cluster_clasificacion_2_2$"on.thyroxine" == "t")),
                       "on antithyroid medication" = c(sum(cluster_clasificacion_2_1$"on.antithyroid.medication" == "t"),
                                                       sum(cluster_clasificacion_2_2$"on.antithyroid.medication" == "t")),
                       "sick" = c(sum(cluster_clasificacion_2_1$"sick" == "t"),
                                  sum(cluster_clasificacion_2_2$"sick"  == "t")),
                       "pregnant" = c(sum(cluster_clasificacion_2_1$"pregnant"  == "t"),
                                      sum(cluster_clasificacion_2_2$"pregnant"   == "t")),
                       "thyroid surgery" = c(sum(cluster_clasificacion_2_1$"thyroid.surgery"  == "t"),
                                             sum(cluster_clasificacion_2_2$"thyroid.surgery"   == "t")),
                       "I131 treatment" = c(sum(cluster_clasificacion_2_1$"I131.treatment"  == "t"),
                                            sum(cluster_clasificacion_2_2$"I131.treatment"   == "t")),
                       "query hypothyroid" = c(sum(cluster_clasificacion_2_1$"query.hypothyroid"  == "t"),
                                               sum(cluster_clasificacion_2_2$"query.hypothyroid"   == "t")),
                       "lithium" = c(sum(cluster_clasificacion_2_1$"lithium"   == "t"),
                                     sum(cluster_clasificacion_2_2$"lithium"    == "t")),
                       "goitre" = c(sum(cluster_clasificacion_2_1$"goitre"   == "t"),
                                    sum(cluster_clasificacion_2_2$"goitre"   == "t")),
                       "tumor" = c(sum(cluster_clasificacion_2_1$"tumor"   == "t"),
                                   sum(cluster_clasificacion_2_2$"tumor"  == "t")),
                       "hypopituitary" = c(sum(cluster_clasificacion_2_1$"hypopituitary"   == "t"),
                                           sum(cluster_clasificacion_2_2$"hypopituitary"  == "t")),
                       "psych" = c(sum(cluster_clasificacion_2_1$psych == "t"),
                                   sum(cluster_clasificacion_2_2$psych == "t")))



# Transpone todas las columnas menos la primer
datos_2k <- data.frame(t(datos_2k[-1]))
# Añadimos los nombres de las columnas
colnames(datos_2k) <- nombre_2k_cluster





#--------------------------------------------------------------------------------------------

levels_cluster4 <- cluster_k4[["data"]][["cluster"]]
cluster_clasificacion_4 <- data.frame(datos_cluster,levels_cluster4)
cluster_clasificacion_4_v <- data.frame(individuos,levels_cluster4)
tabla_4 <- table(cluster_clasificacion_4_v)

#---cluster 1
cluster_clasificacion_4_1 <- cluster_clasificacion_4[cluster_clasificacion_4$levels_cluster4 == 1,]
cluster_clasificacion_4_2 <- cluster_clasificacion_4[cluster_clasificacion_4$levels_cluster4 == 2,]
cluster_clasificacion_4_3 <- cluster_clasificacion_4[cluster_clasificacion_4$levels_cluster4 == 3,]
cluster_clasificacion_4_4 <- cluster_clasificacion_4[cluster_clasificacion_4$levels_cluster4 == 4,]


Conf2x2_EDAD = matrix(c(1:4), nrow=2, byrow=TRUE)
layout(Conf2x2_EDAD)
hist(cluster_clasificacion_4_1$age, xlab = "EDADES", main = "EDAD EN k=1")
hist(cluster_clasificacion_4_2$age, xlab = "EDADES", main = "EDAD EN k=2")
hist(cluster_clasificacion_4_3$age, xlab = "EDADES", main = "EDAD EN k=3")
hist(cluster_clasificacion_4_4$age, xlab = "EDADES", main = "EDAD EN k=4")

#GENERO
tabla4_1 <- table(cluster_clasificacion_4_1$sex)
tabla4_2 <- table(cluster_clasificacion_4_2$sex)
tabla4_3 <- table(cluster_clasificacion_4_3$sex)
tabla4_4 <- table(cluster_clasificacion_4_4$sex)
Tablas_sex <- rbind(tabla4_1,tabla4_2,tabla4_3,tabla4_4)

#TSH
Conf2x2_TSH = matrix(c(1:4), nrow=2, byrow=TRUE)
layout(Conf2x2_TSH)
hist(cluster_clasificacion_4_1$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=1")
hist(cluster_clasificacion_4_2$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=2")
hist(cluster_clasificacion_4_3$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=3")
hist(cluster_clasificacion_4_4$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=4")

#T4
Conf2x2_T4 = matrix(c(1:4), nrow=2, byrow=TRUE)
layout(Conf2x2_T4)
hist(cluster_clasificacion_4_1$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=1")
hist(cluster_clasificacion_4_2$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=2")
hist(cluster_clasificacion_4_3$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=3")
hist(cluster_clasificacion_4_4$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=4")


#T3
Conf2x2_T4 = matrix(c(1:4), nrow=2, byrow=TRUE)
layout(Conf2x2_T4)
hist(cluster_clasificacion_4_1$T3, xlab = "Niveles de T3", main = "T3 en cluster K=1")
hist(cluster_clasificacion_4_2$T3, xlab = "Niveles de T3", main = "T3 en cluster K=2")
hist(cluster_clasificacion_4_3$T3, xlab = "Niveles de T3", main = "T3 en cluster K=3")
hist(cluster_clasificacion_4_4$T3, xlab = "Niveles de T3", main = "T3 en cluster K=4")


nombre_4k_cluster <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")
datos_4k <- data.frame("on thyroxine" = c(sum(cluster_clasificacion_4_1$"on.thyroxine" == "t"),
                                       sum(cluster_clasificacion_4_2$"on.thyroxine" == "t"),
                                       sum(cluster_clasificacion_4_3$"on.thyroxine" == "t"),
                                       sum(cluster_clasificacion_4_4$"on.thyroxine" == "t")),
                    "on antithyroid medication" = c(sum(cluster_clasificacion_4_1$"on.antithyroid.medication" == "t"),
                                       sum(cluster_clasificacion_4_2$"on.antithyroid.medication" == "t"),
                                       sum(cluster_clasificacion_4_3$"on.antithyroid.medication" == "t"),
                                       sum(cluster_clasificacion_4_4$"on.antithyroid.medication" == "t")),
                    "sick" = c(sum(cluster_clasificacion_4_1$"sick" == "t"),
                                                    sum(cluster_clasificacion_4_2$"sick"  == "t"),
                                                    sum(cluster_clasificacion_4_3$"sick"  == "t"),
                                                    sum(cluster_clasificacion_4_4$"sick"  == "t")),
                    "pregnant" = c(sum(cluster_clasificacion_4_1$"pregnant"  == "t"),
                               sum(cluster_clasificacion_4_2$"pregnant"   == "t"),
                               sum(cluster_clasificacion_4_3$"pregnant"   == "t"),
                               sum(cluster_clasificacion_4_4$"pregnant"   == "t")),
                    "thyroid surgery" = c(sum(cluster_clasificacion_4_1$"thyroid.surgery"  == "t"),
                                   sum(cluster_clasificacion_4_2$"thyroid.surgery"   == "t"),
                                   sum(cluster_clasificacion_4_3$"thyroid.surgery"   == "t"),
                                   sum(cluster_clasificacion_4_4$"thyroid.surgery"   == "t")),
                    "I131 treatment" = c(sum(cluster_clasificacion_4_1$"I131.treatment"  == "t"),
                                          sum(cluster_clasificacion_4_2$"I131.treatment"   == "t"),
                                          sum(cluster_clasificacion_4_3$"I131.treatment"   == "t"),
                                          sum(cluster_clasificacion_4_4$"I131.treatment"   == "t")),
                    "query hypothyroid" = c(sum(cluster_clasificacion_4_1$"query.hypothyroid"  == "t"),
                                         sum(cluster_clasificacion_4_2$"query.hypothyroid"   == "t"),
                                         sum(cluster_clasificacion_4_3$"query.hypothyroid"   == "t"),
                                         sum(cluster_clasificacion_4_4$"query.hypothyroid"   == "t")),
                    "lithium" = c(sum(cluster_clasificacion_4_1$"lithium"   == "t"),
                                            sum(cluster_clasificacion_4_2$"lithium"    == "t"),
                                            sum(cluster_clasificacion_4_3$"lithium"   == "t"),
                                            sum(cluster_clasificacion_4_4$"lithium"    == "t")),
                    "goitre" = c(sum(cluster_clasificacion_4_1$"goitre"   == "t"),
                                  sum(cluster_clasificacion_4_2$"goitre"   == "t"),
                                  sum(cluster_clasificacion_4_3$"goitre"   == "t"),
                                  sum(cluster_clasificacion_4_4$"goitre"    == "t")),
                    "tumor" = c(sum(cluster_clasificacion_4_1$"tumor"   == "t"),
                                 sum(cluster_clasificacion_4_2$"tumor"  == "t"),
                                 sum(cluster_clasificacion_4_3$"tumor"   == "t"),
                                 sum(cluster_clasificacion_4_4$"tumor"    == "t")),
                    "hypopituitary" = c(sum(cluster_clasificacion_4_1$"hypopituitary"   == "t"),
                                sum(cluster_clasificacion_4_2$"hypopituitary"  == "t"),
                                sum(cluster_clasificacion_4_3$"hypopituitary"   == "t"),
                                sum(cluster_clasificacion_4_4$"hypopituitary"    == "t")),
                    "psych" = c(sum(cluster_clasificacion_4_1$psych == "t"),
                                sum(cluster_clasificacion_4_2$psych == "t"),
                                sum(cluster_clasificacion_4_3$psych == "t"),
                                sum(cluster_clasificacion_4_4$psych == "t")))



# Transpone todas las columnas menos la primer
datos_4k <- data.frame(t(datos_4k[-1]))
# Añadimos los nombres de las columnas
colnames(datos_4k) <- nombre_4k_cluster



#-------------

levels_cluster6 <- cluster_k6[["data"]][["cluster"]]
cluster_clasificacion_6 <- data.frame(datos_cluster,levels_cluster6)
cluster_clasificacion_6_1 <- cluster_clasificacion_6[cluster_clasificacion_6$levels_cluster6 == 1,]
cluster_clasificacion_6_2 <- cluster_clasificacion_6[cluster_clasificacion_6$levels_cluster6 == 2,]
cluster_clasificacion_6_3 <- cluster_clasificacion_6[cluster_clasificacion_6$levels_cluster6 == 3,]
cluster_clasificacion_6_4 <- cluster_clasificacion_6[cluster_clasificacion_6$levels_cluster6 == 4,]
cluster_clasificacion_6_5 <- cluster_clasificacion_6[cluster_clasificacion_6$levels_cluster6 == 5,]
cluster_clasificacion_6_6 <- cluster_clasificacion_6[cluster_clasificacion_6$levels_cluster6 == 6,]

#Analisis de genero



#RESUMEN DE TODO

#EDAD

Conf3x2_EDAD = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2_EDAD)
hist(cluster_clasificacion_6_1$age, xlab = "EDADES", main = "EDAD EN k=1")
hist(cluster_clasificacion_6_2$age, xlab = "EDADES", main = "EDAD EN k=2")
hist(cluster_clasificacion_6_3$age, xlab = "EDADES", main = "EDAD EN k=3")
hist(cluster_clasificacion_6_4$age, xlab = "EDADES", main = "EDAD EN k=4")
hist(cluster_clasificacion_6_5$age, xlab = "EDADES", main = "EDAD EN k=5")
hist(cluster_clasificacion_6_6$age, xlab = "EDADES", main = "EDAD EN k=6")

#GENERO
tabla6_1 <- table(cluster_clasificacion_6_1$sex)
tabla6_2 <- table(cluster_clasificacion_6_2$sex)
tabla6_3 <- table(cluster_clasificacion_6_3$sex)
tabla6_4 <- table(cluster_clasificacion_6_4$sex)
tabla6_5 <- table(cluster_clasificacion_6_5$sex)
tabla6_6 <- table(cluster_clasificacion_6_6$sex)
Tablas_sex <- rbind(tabla6_1,tabla6_2,tabla6_3,tabla6_4,tabla6_5,tabla6_6)

#TSH
Conf3x2_TSH = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2_TSH)
hist(cluster_clasificacion_6_1$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=1")
hist(cluster_clasificacion_6_2$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=2")
hist(cluster_clasificacion_6_3$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=3")
hist(cluster_clasificacion_6_4$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=4")
hist(cluster_clasificacion_6_5$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=5")
hist(cluster_clasificacion_6_6$TSH, xlab = "Niveles de TSH", main = "TSH en cluster K=6")

#T4
Conf3x2_T4 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2_T4)
hist(cluster_clasificacion_6_1$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=1")
hist(cluster_clasificacion_6_2$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=2")
hist(cluster_clasificacion_6_3$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=3")
hist(cluster_clasificacion_6_4$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=4")
hist(cluster_clasificacion_6_5$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=5")
hist(cluster_clasificacion_6_6$T4U, xlab = "Niveles de T4", main = "T4 en cluster K=6")

#T3
Conf3x2_T4 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2_T4)
hist(cluster_clasificacion_6_1$T3, xlab = "Niveles de T3", main = "T3 en cluster K=1")
hist(cluster_clasificacion_6_2$T3, xlab = "Niveles de T3", main = "T3 en cluster K=2")
hist(cluster_clasificacion_6_3$T3, xlab = "Niveles de T3", main = "T3 en cluster K=3")
hist(cluster_clasificacion_6_4$T3, xlab = "Niveles de T3", main = "T3 en cluster K=4")
hist(cluster_clasificacion_6_5$T3, xlab = "Niveles de T3", main = "T3 en cluster K=5")
hist(cluster_clasificacion_6_6$T3, xlab = "Niveles de T3", main = "T3 en cluster K=6")

nombre_6k_cluster <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")
datos_6k <- data.frame("on thyroxine" = c(sum(cluster_clasificacion_6_1$"on.thyroxine" == "t"),
                                          sum(cluster_clasificacion_6_2$"on.thyroxine" == "t"),
                                          sum(cluster_clasificacion_6_3$"on.thyroxine" == "t"),
                                          sum(cluster_clasificacion_6_4$"on.thyroxine" == "t"),
                                          sum(cluster_clasificacion_6_5$"on.thyroxine" == "t"),
                                          sum(cluster_clasificacion_6_6$"on.thyroxine" == "t")),
                       "on antithyroid medication" = c(sum(cluster_clasificacion_6_1$"on.antithyroid.medication" == "t"),
                                                       sum(cluster_clasificacion_6_2$"on.antithyroid.medication" == "t"),
                                                       sum(cluster_clasificacion_6_3$"on.antithyroid.medication" == "t"),
                                                       sum(cluster_clasificacion_6_4$"on.antithyroid.medication" == "t"),
                                                       sum(cluster_clasificacion_6_5$"on.antithyroid.medication" == "t"),
                                                       sum(cluster_clasificacion_6_6$"on.antithyroid.medication" == "t")),
                       "sick" = c(sum(cluster_clasificacion_6_1$"sick" == "t"),
                                  sum(cluster_clasificacion_6_2$"sick"  == "t"),
                                  sum(cluster_clasificacion_6_3$"sick"  == "t"),
                                  sum(cluster_clasificacion_6_4$"sick"  == "t"),
                                  sum(cluster_clasificacion_6_5$"sick"  == "t"),
                                  sum(cluster_clasificacion_6_6$"sick"  == "t")),
                       "pregnant" = c(sum(cluster_clasificacion_6_1$"pregnant"  == "t"),
                                      sum(cluster_clasificacion_6_2$"pregnant"   == "t"),
                                      sum(cluster_clasificacion_6_3$"pregnant"   == "t"),
                                      sum(cluster_clasificacion_6_4$"pregnant"   == "t"),
                                      sum(cluster_clasificacion_6_5$"pregnant"   == "t"),
                                      sum(cluster_clasificacion_6_6$"pregnant"   == "t")),
                       "thyroid surgery" = c(sum(cluster_clasificacion_6_1$"thyroid.surgery"  == "t"),
                                             sum(cluster_clasificacion_6_2$"thyroid.surgery"   == "t"),
                                             sum(cluster_clasificacion_6_3$"thyroid.surgery"   == "t"),
                                             sum(cluster_clasificacion_6_4$"thyroid.surgery"   == "t"),
                                             sum(cluster_clasificacion_6_5$"thyroid.surgery"   == "t"),
                                             sum(cluster_clasificacion_6_6$"thyroid.surgery"   == "t")),
                       "I131 treatment" = c(sum(cluster_clasificacion_6_1$"I131.treatment"  == "t"),
                                            sum(cluster_clasificacion_6_2$"I131.treatment"   == "t"),
                                            sum(cluster_clasificacion_6_3$"I131.treatment"   == "t"),
                                            sum(cluster_clasificacion_6_4$"I131.treatment"   == "t"),
                                            sum(cluster_clasificacion_6_5$"I131.treatment"   == "t"),
                                            sum(cluster_clasificacion_6_6$"I131.treatment"   == "t")),
                       "query hypothyroid" = c(sum(cluster_clasificacion_6_1$"query.hypothyroid"  == "t"),
                                               sum(cluster_clasificacion_6_2$"query.hypothyroid"   == "t"),
                                               sum(cluster_clasificacion_6_3$"query.hypothyroid"   == "t"),
                                               sum(cluster_clasificacion_6_4$"query.hypothyroid"   == "t"),
                                               sum(cluster_clasificacion_6_5$"query.hypothyroid"   == "t"),
                                               sum(cluster_clasificacion_6_6$"query.hypothyroid"   == "t")),
                       "lithium" = c(sum(cluster_clasificacion_6_1$"lithium"   == "t"),
                                     sum(cluster_clasificacion_6_2$"lithium"    == "t"),
                                     sum(cluster_clasificacion_6_3$"lithium"   == "t"),
                                     sum(cluster_clasificacion_6_4$"lithium"    == "t"),
                                     sum(cluster_clasificacion_6_5$"lithium"    == "t"),
                                     sum(cluster_clasificacion_6_6$"lithium"    == "t")),
                       "goitre" = c(sum(cluster_clasificacion_6_1$"goitre"   == "t"),
                                    sum(cluster_clasificacion_6_2$"goitre"   == "t"),
                                    sum(cluster_clasificacion_6_3$"goitre"   == "t"),
                                    sum(cluster_clasificacion_6_4$"goitre"    == "t"),
                                    sum(cluster_clasificacion_6_5$"goitre"    == "t"),
                                    sum(cluster_clasificacion_6_6$"goitre"    == "t")),
                       "tumor" = c(sum(cluster_clasificacion_6_1$"tumor"   == "t"),
                                   sum(cluster_clasificacion_6_2$"tumor"  == "t"),
                                   sum(cluster_clasificacion_6_3$"tumor"   == "t"),
                                   sum(cluster_clasificacion_6_4$"tumor"    == "t"),
                                   sum(cluster_clasificacion_6_5$"tumor"    == "t"),
                                   sum(cluster_clasificacion_6_6$"tumor"    == "t")),
                       "hypopituitary" = c(sum(cluster_clasificacion_6_1$"hypopituitary"   == "t"),
                                           sum(cluster_clasificacion_6_2$"hypopituitary"  == "t"),
                                           sum(cluster_clasificacion_6_3$"hypopituitary"   == "t"),
                                           sum(cluster_clasificacion_6_4$"hypopituitary"    == "t"),
                                           sum(cluster_clasificacion_6_5$"hypopituitary"    == "t"),
                                           sum(cluster_clasificacion_6_6$"hypopituitary"    == "t")),
                       "psych" = c(sum(cluster_clasificacion_6_1$psych == "t"),
                                   sum(cluster_clasificacion_6_2$psych == "t"),
                                   sum(cluster_clasificacion_6_3$psych == "t"),
                                   sum(cluster_clasificacion_6_4$psych == "t"),
                                   sum(cluster_clasificacion_6_5$psych == "t"),
                                   sum(cluster_clasificacion_6_6$psych == "t")))



# Transpone todas las columnas menos la primer
datos_6k <- data.frame(t(datos_6k[-1]))
# AÃ±adimos los nombres de las columnas
colnames(datos_6k) <- nombre_6k_cluster
#TABLAS
#tabla_2
#tabla_4
#tabla_6




