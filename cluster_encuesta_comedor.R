getwd()

setwd("~/Desktop/TP_prof/analisis_encuesta")

rm(list=ls())   #clean memory
library(readxl)
library(stringr)

data <- read_excel("encuesta_comedor.xlsx") #left join (dplyr)


# select base variables
data1 <- data[ , c(1,15,16,17,18,19,27,28,29,30,31,53,54,55,56,57)] #41,48,49 (cafe/buffet)

#unifico las columnas de calidad
data1$`Puntúa la calidad del Menú (Se aprueba con 6)...15` <- paste0(data1$`Puntúa la calidad del Menú (Se aprueba con 6)...15`, data1$`Puntúa la calidad del Menú (Se aprueba con 6)...27`)

#Borro los valores NA, Se hace dos veces por que hay algunas filas que tienen "NANA"
data1$`Puntúa la calidad del Menú (Se aprueba con 6)...15` <- str_replace(data1$`Puntúa la calidad del Menú (Se aprueba con 6)...15` , "NA", "")
data1$`Puntúa la calidad del Menú (Se aprueba con 6)...15` <- str_replace(data1$`Puntúa la calidad del Menú (Se aprueba con 6)...15` , "NA", "")

#unifico las columnas de cantidad
data1$`Puntúa la cantidad del menú (Se aprueba con 6)...16` <- paste0(data1$`Puntúa la cantidad del menú (Se aprueba con 6)...16`, data1$`Puntúa la cantidad del menú (Se aprueba con 6)...28`)

data1$`Puntúa la cantidad del menú (Se aprueba con 6)...16` <- str_replace(data1$`Puntúa la cantidad del menú (Se aprueba con 6)...16` , "NA", "")
data1$`Puntúa la cantidad del menú (Se aprueba con 6)...16` <- str_replace(data1$`Puntúa la cantidad del menú (Se aprueba con 6)...16` , "NA", "")

#unifico las columnas de variedad
data1$`Puntúa la variedad del menú (Se aprueba con 6)...17` <- paste0(data1$`Puntúa la variedad del menú (Se aprueba con 6)...17`, data1$`Puntúa la variedad del menú (Se aprueba con 6)...29`)

data1$`Puntúa la variedad del menú (Se aprueba con 6)...17` <- str_replace(data1$`Puntúa la variedad del menú (Se aprueba con 6)...17` , "NA", "")
data1$`Puntúa la variedad del menú (Se aprueba con 6)...17` <- str_replace(data1$`Puntúa la variedad del menú (Se aprueba con 6)...17` , "NA", "")

#unifico las columnas de relacion/precio calidad
data1$`Relación calidad/precio (Se aprueba con 6)...18` <- paste0(data1$`Relación calidad/precio (Se aprueba con 6)...18`, data1$`Relación calidad/precio (Se aprueba con 6)...30`)

data1$`Relación calidad/precio (Se aprueba con 6)...18` <- str_replace(data1$`Relación calidad/precio (Se aprueba con 6)...18` , "NA", "")
data1$`Relación calidad/precio (Se aprueba con 6)...18` <- str_replace(data1$`Relación calidad/precio (Se aprueba con 6)...18` , "NA", "")

#unifico las columnas de velocidad de atención
data1$`Puntúa la velocidad de atención hoy en día (Se aprueba con 6)...19` <- paste0(data1$`Puntúa la velocidad de atención hoy en día (Se aprueba con 6)...19`, data1$`Puntúa la velocidad de atención hoy en día (Se aprueba con 6)...31`)

data1$`Puntúa la velocidad de atención hoy en día (Se aprueba con 6)...19` <- str_replace(data1$`Puntúa la velocidad de atención hoy en día (Se aprueba con 6)...19` , "NA", "")
data1$`Puntúa la velocidad de atención hoy en día (Se aprueba con 6)...19` <- str_replace(data1$`Puntúa la velocidad de atención hoy en día (Se aprueba con 6)...19` , "NA", "")

#Me quedo solo con las columnas unificadas 
data2 <- data1[ , c(1,2,3,4,5,6,12,13,14,15,16)] #17,18,19 (cafe/buffet)
data2[data2==''] <- NA
#elimino la gente que NO almuerza en el comedor
data3 <- na.omit(data2)
#Elimino la columna de ID
data4 <- data3[ , -1]
data4 <- as.data.frame(sapply(data4, as.numeric))
names(data4) <- c("calidad", "cantidad", "variedad", "precio/calidad", "velocidad", "Iluminacion", "Espacio","ventilacion","Acustica","Limpieza")
datar <- data4
#termino


# COMPONENTES PRINCIPALES ANALISIS

library(psych)
pcarot <- principal(datar, nfactors=5, rotate="varimax",scores=T) 
pcarot  # print results
pcarot$loadings
plot(pcarot$scores)
pcarot$scores
#write.csv(pcarot$scores,file="Z.csv")  # the principal components
#sapply(as.data.frame(pcarot$scores),sd)
#biplot(pcarot)
# save positioning

#write.table(file="Interdep Posit.csv",aggregate(pcarot$scores,by=list(datc$brand),FUN=mean),sep=",",dec=".")

#data5 <- cbind(data3, pcarot$scores)
#data5 <- pcarot$scores

# Hierarchical Clustering - Complete linkage, Ward, Single linkage
d <- dist(data4, method = "euclidean") # distance matrix --> ward
cluh <- hclust(d, method="ward.D") 
plot(cluh) # display dendrogram
groups <- cutree(cluh, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the clusters 
rect.hclust(cluh, k=4, border="red")
# get cluster means 
#write.table(file="cluster_almuerzo_habitat_jerarquico",aggregate(data4,by=list(groups),FUN=mean),sep=",",dec=".")

data5 <- data.frame(data3, groups)
names(data5) <- c("ID","Calidad", "Cantidad", "Variedad", "Precio/Calidad", "Velocidad", "Iluminacion", "Espacio","Ventilacion","Acustica","Limpieza","Groups")

clusters <- df_tabla <- data.frame(table(data5$Groups))
names(clusters) <- c("Cluster", "Tamaño")
clusters

cluster_plot <-ggplot(clusters, aes(x=Cluster, y=Tamaño)) +
  geom_bar(stat="identity")+theme_minimal()

cluster_plot

data6 <- data5[ ,c(1,12)]

#unifico los clusters con la encuesta original
library(dplyr)
data_unif <- left_join(data, data6, "ID")
#Me quedo sólo con los que almuerzan
data_unif <- data_unif[!is.na(data_unif$Groups), ]
data_comportaminto <- data_unif[ ,c(12,34,63)]
#Solo con los que van cada tanto:
data_comp_aveces <- data_comportaminto[!is.na(data_comportaminto$`¿Cuantas veces elegís otro lugar?`), ]

#SIN NUNCA
data_comp_aveces$`¿Cuantas veces elegís otro lugar?` <- factor(data_comp_aveces$`¿Cuantas veces elegís otro lugar?`, 
                                                               ordered = TRUE, 
                                                               levels = c("Menos de 20 %","20 - 40 %","40 - 60 %","60 - 80%","más de 80%"))


tabla <- table(data_comp_aveces$`¿Cuantas veces elegís otro lugar?`,data_comp_aveces$Groups)
tabla

tabla_prop_cluster <- prop.table(tabla, 2)
tabla_prop_cluster

df_tabla_clu <- data.frame(tabla_prop_cluster)
names(df_tabla_clu) <- c("Comportamiento", "Cluster", "Proporción")

tabla_prop_comp <- prop.table(tabla, 1)
tabla_prop_comp

df_tabla <- data.frame(tabla_prop_comp)
names(df_tabla) <- c("Comportamiento", "Cluster", "Proporción")

library(ggplot2)

prop_plot <- ggplot(df_tabla, aes(x=Comportamiento, y=Proporción, fill=Cluster)) +
  geom_bar(stat="identity")+theme_minimal()
prop_plot

prop_clu_plot <- ggplot(df_tabla_clu, aes(x=Cluster, y=Proporción, fill=Comportamiento)) +
  geom_bar(stat="identity")+theme_minimal()
prop_clu_plot


#CON NUNCA
#Los que siempre comen, tienen NUNCA (no condiciono)
data_comportaminto$`¿Cuantas veces elegís otro lugar?`[is.na(data_comportaminto$`¿Cuantas veces elegís otro lugar?`)] <- "Nunca"

data_comportaminto$`¿Cuantas veces elegís otro lugar?` <- factor(data_comportaminto$`¿Cuantas veces elegís otro lugar?`, 
                                                                 ordered = TRUE, 
                                                                 levels = c("Nunca", "Menos de 20 %","20 - 40 %","40 - 60 %","60 - 80%","más de 80%"))

tabla1 <- table(data_comportaminto$`¿Cuantas veces elegís otro lugar?`,data_comportaminto$Groups)
tabla1

tabla1_prop_cluster <- prop.table(tabla1, 2)
tabla1_prop_cluster

df_tabla_clu1 <- data.frame(tabla1_prop_cluster)
names(df_tabla_clu1) <- c("Comportamiento", "Cluster", "Proporción")

tabla1_prop_comp <- prop.table(tabla1, 1)
tabla1_prop_comp

df_tabla1 <- data.frame(tabla1_prop_comp)
names(df_tabla1) <- c("Comportamiento", "Cluster", "Proporción")

prop_plot1 <- ggplot(df_tabla1, aes(x=Comportamiento, y=Proporción, fill=Cluster)) +
  geom_bar(stat="identity")+theme_minimal()
prop_plot1

prop_clu_plot1 <- ggplot(df_tabla_clu1, aes(x=Cluster, y=Proporción, fill=Comportamiento)) +
  geom_bar(stat="identity")+theme_minimal()
prop_clu_plot1

#-------------

data_presup_veces <- data_unif[ ,c(11,12,63)]

data_presup_veces <- data_presup_veces[!is.na(data_presup_veces$`¿Cuantas veces a la semana almorzás dentro de tu horario de cursada?`), ]

tabla_frecuencia <- table(data_presup_veces$`¿Cuantas veces a la semana almorzás dentro de tu horario de cursada?`,data_presup_veces$Groups)
tabla_frecuencia

tabla_prop_frec <- prop.table(tabla_frecuencia, 2)
tabla_prop_frec

df_frec <- data.frame(tabla_prop_frec)
names(df_frec) <- c("Veces_semana", "Cluster", "Proporcion")

prop_plot2 <- ggplot(df_frec, aes(x=Cluster, y=Proporcion, fill=Veces_semana)) +
  geom_bar(stat="identity")+theme_minimal()
prop_plot2

table(data_presup_veces$`¿Cuantas veces a la semana almorzás dentro de tu horario de cursada?`)
(109 + 226*2 + 214*3 +83*4 + 52*5) / (109+226+214+83+52) #promedio de veces que la gente va al comedor

data_presup_veces$`¿Cual es tu presupuesto diario para almorzar?` <- factor(data_presup_veces$`¿Cual es tu presupuesto diario para almorzar?`, 
                                                                 ordered = TRUE, 
                                                                 levels = c("Menos de $ 50","$ 50 - 100","$ 100 - 150", "$ 150 - 200","$ 200 - 250","$ 250 - 300","$ 300 - 350","	$ 350 - 400","Más de $ 400"))
data_presup_veces <- na.omit(data_presup_veces)

tabla_pres <- table(data_presup_veces$`¿Cual es tu presupuesto diario para almorzar?`,data_presup_veces$Groups)

tabla_prop_pres <- prop.table(tabla_pres, 2)
tabla_prop_pres

df_frec <- data.frame(tabla_prop_pres)
names(df_frec) <- c("Presupuesto", "Cluster", "Proporcion")

prop_plot3 <- ggplot(df_frec, aes(x=Cluster, y=Proporcion, fill=Presupuesto)) +
  geom_bar(stat="identity")+theme_minimal()
prop_plot3

table(data_presup_veces$`¿Cual es tu presupuesto diario para almorzar?`)

#------------------------
# Análiis de veganos

competencia <- data_unif[ , c(24,35)]

competencia$`¿Dónde comés? (habitualmente)` <- paste0(competencia$`¿Dónde comés? (habitualmente)`, competencia$`¿Dónde comés cuando no usas el comedor? (habitualmente)`)

competencia$`¿Dónde comés? (habitualmente)` <- str_replace(competencia$`¿Dónde comés? (habitualmente)` , "NA", "")
competencia$`¿Dónde comés? (habitualmente)` <- str_replace(competencia$`¿Dónde comés? (habitualmente)` , "NA", "")

competencia <- competencia[1]

competencia[competencia==''] <- NA

#elimino la gente que NO almuerza en el comedor
competencia <- na.omit(competencia)

library("writexl")

install.packages("writexl")

write.table(file="competencia",competencia)

length(grep("Chino por peso", competencia))

str_count(competencia, "Chino por peso")
#294

str_count(competencia, "Bodegón-Resto")
#80





