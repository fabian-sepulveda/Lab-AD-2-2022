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
datos <- datos%>% select(-"class")
datos <- na.omit(datos)




#----------------------------------------------------------------#
#----------------------------------------------------------------#
#TRANSFORMAR TODAS LAS VARIABLES A VALORES NUMERICOS

#Normalizar los datos

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
  
  





#---------------------------------------------#
#Normalizando datos

datos_normalizados <- datos_transformados
datos_normalizados$age <- scale(datos_normalizados$age)
datos_normalizados$FTI <- scale(datos_normalizados$FTI)
datos_normalizados$TT4 <- scale(datos_normalizados$TT4)

#Distancias




gower_dist <- daisy(datos_transformados, metric = "gower", type = list(symm = c(7:37),
                                                                       logratio = c(1:6))) 



#-----------Estimar numero de cluster------------
#wss <- fviz_nbclust(x = datos_normalizados, FUNcluster = pam, method = "wss", diss = gower_dist)
#silueta <- fviz_nbclust(x = datos_normalizados, FUNcluster = pam, method = "silhouette", diss = gower_dist)

#plot(wss)
#plot(silueta)



#otro grafico jj

pam_clusters <- pam(x = datos_normalizados, k = 4)

arbol <- hclust(d = gower_dist,
                method = "ward.D2")


#Grafico mas cuadrado

fviz_cluster(object = pam_clusters,
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "-------",
       subtitle = "Distancia gower, K=4") +
  theme_bw() +
  theme(legend.position = "bottom")

#Dendograma circular

circular <- fviz_dend(arbol, k = 4,
                 rect = TRUE,
                 k_colors = "jco",
                 rect_border = "jco",
                 rect_fill = TRUE,
                 type = "circular")


# Circulitos

g2 <- fviz_cluster(object = pam_clusters, data = datos_normalizados, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")


print(g2)





#-------------------------------------------------------------------------------------------#


