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

data4 <- data3[ , -1]
data4 <- as.data.frame(sapply(data4, as.numeric))

names(data4) <- c("Calidad", "Cantidad", "Variedad", "Precio/Calidad", "Velocidad", "Iluminacion", "Espacio","ventilacion","Acustica","Limpieza")


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
write.table(file="cluster_almuerzo_habitat_jerarquico",aggregate(data4,by=list(groups),FUN=mean),sep=",",dec=".")

data5 <- data.frame(data3, groups)
names(data5) <- c("ID","Calidad", "Cantidad", "Variedad", "Precio/Calidad", "Velocidad", "Iluminacion", "Espacio","Ventilacion","Acustica","Limpieza","Groups")

clusters <- table(data5$Groups)
clusters
#La cantidad de clusters es bastante homogenea (149, 199, 173 y 113 = 634)

data6 <- data5[ ,c(1,12)]

#unifico los clusters con la encuesta original
library(dplyr)
data_unif <- left_join(data, data6, "ID")

#Me quedo sólo con los que almuerzan
data_unif <- data_unif[!is.na(data_unif$Groups), ]

data_comportaminto <- data_unif[ ,c(12,34,63)]

#Solo con los que van cada tanto:
data_comp_aveces <- data_comportaminto[!is.na(data_comportaminto$`¿Cuantas veces elegís otro lugar?`), ]

data_comp_aveces$`¿Cuantas veces elegís otro lugar?` <- factor(data_comp_aveces$`¿Cuantas veces elegís otro lugar?`, 
                                                               ordered = TRUE, 
                                                               levels = c("Menos de 20 %","20 - 40 %","40 - 60 %","60 - 80%","más de 80%"))






