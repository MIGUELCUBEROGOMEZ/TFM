
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)
library(openxlsx)
library(skimr)
library(dplyr)

Actigraphdatos1<-read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/Actigraph_usuarios_excel1.xlsx")
Actigraphdatos2<-read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/Actigraph_usuarios_excel2.xlsx")
Actigraphdatos1=as.data.table(Actigraphdatos1)
Actigraphdatos2=as.data.table(Actigraphdatos2)

RRdatos1<-read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/RR_excel1.xlsx")
RRdatos2<-read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/RR_excel2.xlsx")
RRdatos1=as.data.table(RRdatos1)
RRdatos2=as.data.table(RRdatos2)

Actigraphdatos <- rbindlist(list(Actigraphdatos1, Actigraphdatos2), use.names = TRUE, fill = TRUE)
RRdatos <- rbindlist(list(RRdatos1, RRdatos2), use.names = TRUE, fill = TRUE)
Sueñodatos<- read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/sueño_usuarios_excel.xlsx")
Activitydatos<- read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/Activity_usuarios_excel.xlsx")
Cuestionariodatos<- read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/Cuestionario_usuarios_excel.xlsx")
Caracteristicasdatos<- read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/Caracteristicas_usuarios_excel.xlsx")
Salivadatos<- read.xlsx("C:/Users/Miguel/Desktop/Matemáticas/TFM_MiguelCubero/Datos Excel/saliva_usuarios.xlsx")


library(lubridate)

Actigraphdatos <- rbindlist(list(Actigraphdatos1, Actigraphdatos2), use.names = TRUE, fill = TRUE)
Actigraphdatos$time <- as.POSIXct(Actigraphdatos$time, format="%H:%M:%S")
Actigraphdatos$day <- ifelse(Actigraphdatos$day == -29, 2, Actigraphdatos$day)

#Error en las horas de user 8 y user 9

#Actigraphdatos <- Actigraphdatos %>%filter((user %in% c("user_9")))


#Añadimos evolucion por horas
Actigraphdatos <- Actigraphdatos %>%
  mutate(
    Hora = hour(time) + 24*(day-1),
    Tiempo = 60*hour(time) + 24*(day-1)*60+ minute(time)
  )


quantiles_Vector_Magnitude <- quantile(Actigraphdatos$Vector.Magnitude, probs = c(0.05, 0.95), na.rm = TRUE)

# Paso 1: Filtrar los datos para los cuantiles de Vector.Magnitude
Actigraphdatos_filtrados <- Actigraphdatos %>%
  filter(Vector.Magnitude >= quantiles_Vector_Magnitude[1] & 
           Vector.Magnitude <= quantiles_Vector_Magnitude[2])

# Paso 2: Escalar las variables Vector.Magnitude y tilt con min-max
Actigraphdatos_filtrados <- Actigraphdatos_filtrados %>%
  mutate(
    Vector.Magnitude_scaled = (Vector.Magnitude - min(Vector.Magnitude)) / (max(Vector.Magnitude) - min(Vector.Magnitude)),
    HR_scaled= (HR - min(HR)) / (max(HR) - min(HR))
  )


# Paso 3: Calcular el HRMV como el promedio de las variables normalizadas
Actigraphdatos_filtrados <- Actigraphdatos_filtrados %>%
  mutate(HRMV = (Vector.Magnitude_scaled + HR_scaled) / 2,
         Inclinometer.Lying = Inclinometer.Lying+Inclinometer.Off)


# Crear una columna de estado que combine las tres variables de posición
Actigraphdatos_filtrados$estado <- with(Actigraphdatos_filtrados, ifelse(Inclinometer.Sitting == 1, "sentado",
                                                                         ifelse(Inclinometer.Standing == 1, "de_pie",
                                                                                ifelse(Inclinometer.Lying == 1, "tumbado", NA))))

# Crear una columna que indique si hay un cambio de posición
Actigraphdatos_filtrados$cambio_posicion <- c(FALSE, head(Actigraphdatos_filtrados$estado, -1) != tail(Actigraphdatos_filtrados$estado, -1))

# Convertir de lógico a numérico (0 y 1)
Actigraphdatos_filtrados$cambio_posicion <- as.numeric(Actigraphdatos_filtrados$cambio_posicion)


#$#
# Crear columna para el intervalo de 15 minutos
Actigraphdatos_filtrados <- Actigraphdatos_filtrados %>%
  mutate(intervalo_15min = floor_date(time, "15 minutes"))

# Agrupar por usuario e intervalo de 15 minutos y calcular estadísticas
summary_15min <- Actigraphdatos_filtrados %>%
  group_by(user, intervalo_15min) %>%
  summarise(
    HRMV = mean(HRMV, na.rm = TRUE),
    HR_scaled = mean(HR_scaled, na.rm = TRUE),
    Vector_Magnitude_scaled = mean(Vector.Magnitude_scaled, na.rm = TRUE),
    cambios_posicion = sum(cambio_posicion, na.rm = TRUE)
  )


# Agrupar por usuario y hora, y calcular el promedio de HRMV
summary_df <- Actigraphdatos_filtrados %>%
  group_by(user, Hora) %>%
  summarise(HRMV = mean(HRMV, na.rm = TRUE),
            cambios_posicion = sum(cambio_posicion, na.rm = TRUE))


# Agrupar por usuario y Tiempo, y calcular las estadísticas necesarias
summary_minutos <- Actigraphdatos_filtrados %>%
  group_by(user, Tiempo) %>%
  summarise(HRMV = mean(HRMV, na.rm = TRUE),
            HR_scaled = mean(HR_scaled, na.rm = TRUE),
            Vector_Magnitude_scaled = mean(Vector.Magnitude_scaled, na.rm = TRUE),
            cambios_posicion = sum(cambio_posicion, na.rm = TRUE))

# Crear columna para el intervalo de 15 minutos
summary_minutos <- summary_minutos %>%
  mutate(intervalo_15min = floor(Tiempo / 15))

# Agrupar por usuario e intervalo de 15 minutos y calcular estadísticas
summary_15min <- summary_minutos %>%
  group_by(user, intervalo_15min) %>%
  summarise(
    HRMV = mean(HRMV, na.rm = TRUE),
    HR_scaled = mean(HR_scaled, na.rm = TRUE),
    Vector_Magnitude_scaled = mean(Vector_Magnitude_scaled, na.rm = TRUE),
    cambios_posicion = sum(cambios_posicion, na.rm = TRUE)
  )


summary_15min <- summary_15min %>%
  mutate(cambios_posicion_modificado = ifelse(cambios_posicion == 0, 0.5, cambios_posicion),
         WMFC = HRMV * cambios_posicion_modificado,
         hora_in=intervalo_15min*15/60)


#Corregimos para 6,7,19
summary_15min <- summary_15min %>%
  mutate(hora_in = ifelse(user %in% c("user_6", "user_7", "user_19"), hora_in - 1, hora_in))


min_max_df <- summary_15min %>%
  group_by(user) %>%
  summarise(
    min_intervalo = min(hora_in, na.rm = TRUE),
    max_intervalo = max(hora_in, na.rm = TRUE)
  )


cota_inicio<-max(min_max_df$min_intervalo)
cota_fin<-min(min_max_df$max_intervalo)

summary_15min_acotado <- summary_15min %>%
  filter(hora_in >= cota_inicio & hora_in <= cota_fin)



# Gráfico para un usuario
summary_15minutos_user1 <- summary_15min_acotado %>%
  filter(user=="user_19")

#Gráfico para un usuario
ggplot(summary_15minutos_user1, aes(x = hora_in, y = WMFC, color = user)) +
  geom_line(size = 1.2) +
  labs(title = paste("Promedio de WMFC por Minuto para el Usuario 1"),
       x = "Hora",
       y = "Actividad Posición Movimientos Promedio (WMFC)") +
  theme_minimal() +
  theme(legend.title = element_blank())


# Gráfico para todos los usuarios
ggplot(summary_15min_acotado, aes(x = hora_in, y = WMFC, color = user, group = user)) +
  geom_line(size=1.2) +
  labs(title = "Promedio de WMFC por Minuto para Cada Usuario",
       x = "Hora",
       y = "Actividad Posición Movimientos Promedio (WMFC)") +
  theme_minimal() +
  theme(legend.title = element_blank())






##################################################
            #CLUSTERING FMM#
##################################################

library(FMM)

usuarios <- unique(summary_15min_acotado$user)
coeficientes_todos_usuarios <- data.frame()

for (usuario in usuarios) {
  # Filtrar los datos para el usuario actual
  datos_usuario <- summary_15min_acotado %>%
    filter(user == usuario)
  
  # Ajustar el modelo FMM
  resultado_fmm <- fitFMM(vData = datos_usuario$WMFC)
  
  # Obtener los coeficientes
  coeficientes <- coef(resultado_fmm)
  
  # Combinar los coeficientes en una sola lista
  todos_coeficientes <- c(coeficientes[[1]], coeficientes[[2]])
  
  # Acceder al valor de R2
  r2_value <- resultado_fmm@R2
  
  # Convertir a un dataframe temporal y agregar una columna para el usuario y R2
  df_coeficientes <- as.data.frame(t(todos_coeficientes))
  df_coeficientes$user <- usuario
  df_coeficientes$R2 <- r2_value
  
  # Agregar los coeficientes del usuario actual al dataframe de todos los coeficientes
  coeficientes_todos_usuarios <- bind_rows(coeficientes_todos_usuarios, df_coeficientes)
}

# Imprimir el dataframe con los coeficientes de todos los usuarios
print(coeficientes_todos_usuarios)



####################
  #Clustering#
####################


library(readr)
library(ggplot2)
library(cluster)


datos_fmm <- coeficientes_todos_usuarios %>%
  mutate(
    alpha = as.numeric(alpha),
    beta = as.numeric(beta),
    alpha_sin = sin(alpha),
    alpha_cos = cos(alpha),
    beta_sin = sin(beta),
    beta_cos = cos(beta)
  ) %>%
  select(-alpha, -beta)
  

# Eliminar la columna de usuarios para el clustering
datos_cluster_fmm <-  datos_fmm %>%
  select(-user )

datos_cluster_fmm <- as.data.frame(lapply(datos_cluster_fmm, as.numeric))


# Normalizar los datos usando scale()
datos_scaled_fmm <- scale(datos_cluster_fmm)


##### MÉTODO DEL CODO #########
# Función para calcular el total within sum of squares
wss <- function(k) {
  kmeans(datos_scaled_fmm, k, nstart = 10)$tot.withinss
}

# Rangos de K a evaluar
k.values <- 1:10

# Calcular el total within sum of squares para cada K
wss_values <- sapply(k.values, wss)

# Graficar el método del codo
plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de clústeres K",
     ylab = "Total within sum of squares")


##### MÉTODO DE SILUETA #########
# Determinar el rango de K a evaluar
n_clust <- 2:10

# Inicializar una lista para guardar los resultados de silueta
sil_width <- vector("list", length(n_clust))

# Calcular el coeficiente de silueta para cada valor de K
for (i in seq_along(n_clust)) {
  set.seed(1)  # Para reproducibilidad
  kmeans_result <- kmeans(datos_scaled_fmm, centers = n_clust[i], nstart = 10)
  sil <- silhouette(kmeans_result$cluster, dist(datos_scaled_fmm))
  sil_width[[i]] <- mean(sil[, 3])  # La columna 3 contiene los valores de silueta
}

# Convertir la lista de resultados en un dataframe
sil_df <- data.frame(Clusters = n_clust, Silhouette_Width = unlist(sil_width))

# Graficar el ancho del coeficiente de silueta
ggplot(sil_df, aes(x = Clusters, y = Silhouette_Width)) +
  geom_line(size = 1.5) +  
  geom_point(color = "red") +
  geom_text(aes(label = round(Silhouette_Width, 3)), nudge_y = -0.025, color = "red", size = 4) +  
  labs(title = "Valores del Ancho del Coeficiente de Silueta",
       x = "Número de clusters",
       y = "Ancho del Coeficiente de Silueta") +
  scale_x_continuous(breaks = seq(2, 10, by = 1), labels = abs(seq(2, 10, by = 1))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),      
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank(),    
        panel.background = element_rect(colour = "black", size = 2),
        plot.title = element_text(hjust = 0.5))



# Elegir el número óptimo de clústeres
k_optimo <- 5  # Este valor se elige en base al método del codo

# Aplicar K-means clustering
set.seed(1)  # Para reproducibilidad
kmeans_result <- kmeans(datos_scaled_fmm, centers = k_optimo)


# Añadir el clúster asignado a los datos originales
datos_fmm$Cluster <- as.factor(kmeans_result$cluster)


# Realizar una PCA para reducir la dimensionalidad a 2D
pca_result <- prcomp(datos_scaled_fmm, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)

summary(pca_result)
explained_var <- 100*(pca_result$sdev)^2/sum((pca_result$sdev)^2)

# Calcular las proporciones para los dos primeros componentes
pc1_var <- explained_var[1]
pc2_var <- explained_var[2]

# Añadir la asignación de clústeres al dataframe de PCA
pca_data$Cluster <- datos_fmm$Cluster

dev.off()
# Graficar los clústeres con ggplot2
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 4) +
  labs(
    title = "Clustering del modelo FMM con k-medias y k=5",
    x = paste("Primera Componente Principal (", round(pc1_var, 1), "%)", sep = ""),
    y = paste("Segunda Componente Principal (", round(pc2_var, 1), "%)", sep = "")
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12)
  )
library(factoextra)

fviz_cluster(kmeans_result, data = datos_scaled_fmm, geom = "point")




# Cálculo de los ángulos alpha y beta en radianes
datos_fmm$alpha <- atan2(datos_fmm$alpha_sin, datos_fmm$alpha_cos)
datos_fmm$beta <- atan2(datos_fmm$beta_sin, datos_fmm$beta_cos)

#Solo para la tabla del TFM
datos_TFM<- datos_fmm %>% 
  select(-alpha_sin, -alpha_cos, -beta_sin, -beta_cos)

# Agrupar por Cluster y calcular el promedio de cada columna
datos_promedios <- datos_fmm %>% 
  select(-user,-alpha_sin, -alpha_cos, -beta_sin, -beta_cos)

datos_promedios <- as.data.frame(lapply(datos_promedios, as.numeric))
datos_promedios <- datos_promedios %>%
  group_by(Cluster) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

# Crear data frames separados para cada clúster
datos_cluster_1 <- datos_promedios %>% filter(Cluster == 1)
datos_cluster_2 <- datos_promedios %>% filter(Cluster == 2)
datos_cluster_3 <- datos_promedios %>% filter(Cluster == 3)
datos_cluster_4 <- datos_promedios %>% filter(Cluster == 4)
datos_cluster_5 <- datos_promedios %>% filter(Cluster == 5)


par(mfrow=c(3,2))

generateFMM(datos_cluster_1$V1,datos_cluster_1$A,datos_cluster_1$alpha,datos_cluster_1$beta,datos_cluster_1$omega)
generateFMM(datos_cluster_2$V1,datos_cluster_2$A,datos_cluster_2$alpha,datos_cluster_2$beta,datos_cluster_2$omega)
generateFMM(datos_cluster_3$V1,datos_cluster_3$A,datos_cluster_3$alpha,datos_cluster_3$beta,datos_cluster_3$omega)
generateFMM(datos_cluster_4$V1,datos_cluster_4$A,datos_cluster_4$alpha,datos_cluster_4$beta,datos_cluster_4$omega)
generateFMM(datos_cluster_5$V1,datos_cluster_5$A,datos_cluster_5$alpha,datos_cluster_5$beta,datos_cluster_5$omega)

dev.off()
par(mfrow=c(3,2))
##### Cluster 1
summary_minutos_user2 <- summary_15min_acotado %>%
  filter(user=="user_2")


resultado_fmm_2 <- fitFMM(vData = summary_minutos_user2$WMFC)

# Mostrar los resultados
print(resultado_fmm_2)

plotFMM(resultado_fmm_2)


##### Cluster 2
summary_minutos_user13 <- summary_15min_acotado %>%
  filter(user=="user_13")


resultado_fmm_13 <- fitFMM(vData = summary_minutos_user13$WMFC,)

# Mostrar los resultados
print(resultado_fmm_13)

plotFMM(resultado_fmm_13)



##### Cluster 3
summary_minutos_user1 <- summary_15min_acotado %>%
  filter(user=="user_1")


resultado_fmm_1 <- fitFMM(vData = summary_minutos_user1$WMFC,)

# Mostrar los resultados
print(resultado_fmm_1)

plotFMM(resultado_fmm_1)

##### Cluster 4
summary_minutos_user18 <- summary_15min_acotado %>%
  filter(user=="user_18")


resultado_fmm_18 <- fitFMM(vData = summary_minutos_user18$WMFC,)

# Mostrar los resultados
print(resultado_fmm_18)

plotFMM(resultado_fmm_18)


##### Cluster 5
summary_minutos_user16 <- summary_15min_acotado %>%
  filter(user=="user_16")


resultado_fmm_16 <- fitFMM(vData = summary_minutos_user16$WMFC,)

# Mostrar los resultados
print(resultado_fmm_16)

plotFMM(resultado_fmm_16)


#PLOT DE DOS EN DOS

par(mfrow=c(1,2))


generateFMM(datos_cluster_1$V1,datos_cluster_1$A,datos_cluster_1$alpha,datos_cluster_1$beta,datos_cluster_1$omega)
plotFMM(resultado_fmm_2)

generateFMM(datos_cluster_2$V1,datos_cluster_2$A,datos_cluster_2$alpha,datos_cluster_2$beta,datos_cluster_2$omega)
plotFMM(resultado_fmm_13)

generateFMM(datos_cluster_3$V1,datos_cluster_3$A,datos_cluster_3$alpha,datos_cluster_3$beta,datos_cluster_3$omega)
plotFMM(resultado_fmm_1)

generateFMM(datos_cluster_4$V1,datos_cluster_4$A,datos_cluster_4$alpha,datos_cluster_4$beta,datos_cluster_4$omega)
plotFMM(resultado_fmm_18)

generateFMM(datos_cluster_5$V1,datos_cluster_5$A,datos_cluster_5$alpha,datos_cluster_5$beta,datos_cluster_5$omega)
plotFMM(resultado_fmm_16)


#############################################################
# COMO OBTENEMOS MAYOR COEFICIENTE DE SILUETA:
# Cargar paquetes necesarios
library(cluster)   # Para silhouette
library(mclust)    # Para Gaussian Mixture Models (GMM)

# Definir el rango de clusters
n_clust <- 2:10

# Definir los métodos de clustering jerárquico a evaluar
hierarchical_methods <- c("ward.D2", "complete", "average", "single")

# Inicializar listas para guardar los resultados de silueta para cada método
sil_width_kmeans <- vector("list", length(n_clust))
sil_width_hclust <- vector("list", length(n_clust) * length(hierarchical_methods))
sil_width_gmm <- vector("list", length(n_clust))

# Índice para resultados jerárquicos
index_hclust <- 1

# Calcular el coeficiente de silueta para K-means, diferentes métodos de clustering jerárquico y GMM
for (i in seq_along(n_clust)) {
  # K-means
  set.seed(1)
  kmeans_result <- kmeans(datos_scaled_fmm, centers = n_clust[i], nstart = 10)
  sil <- silhouette(kmeans_result$cluster, dist(datos_scaled_fmm))
  sil_width_kmeans[[i]] <- mean(sil[, 3])
  
  # Clustering Jerárquico con diferentes métodos
  for (method in hierarchical_methods) {
    hclust_result <- cutree(hclust(dist(datos_scaled_fmm), method = method), k = n_clust[i])
    sil <- silhouette(hclust_result, dist(datos_scaled_fmm))
    sil_width_hclust[[index_hclust]] <- list(Method = method, Clusters = n_clust[i], Silhouette_Width = mean(sil[, 3]))
    index_hclust <- index_hclust + 1
  }
  
  # Gaussian Mixture Models (GMM)
  gmm_result <- Mclust(datos_scaled_fmm, G = n_clust[i])
  sil <- silhouette(gmm_result$classification, dist(datos_scaled_fmm))
  sil_width_gmm[[i]] <- mean(sil[, 3])
}

# Convertir las listas de resultados en dataframes
sil_df_kmeans <- data.frame(Clusters = n_clust, Silhouette_Width = unlist(sil_width_kmeans), Method = "K-means")
sil_df_hclust <- do.call(rbind, lapply(sil_width_hclust, as.data.frame))
sil_df_gmm <- data.frame(Clusters = n_clust, Silhouette_Width = unlist(sil_width_gmm), Method = "GMM")

# Combinar todos los resultados en un solo dataframe
sil_df <- rbind(sil_df_kmeans, sil_df_hclust, sil_df_gmm)

# Mostrar el dataframe de resultados
print(sil_df)



#############################################################
# CLUSTERING
# Definir el número óptimo de clústeres
k_optimo <- 2  # Este valor se elige en base al método del codo

# Realizar el clustering jerárquico con el método "single"
hclust_result <- hclust(dist(datos_scaled_fmm), method = "single")

# Cortar el dendrograma para obtener 5 clústeres
clusters <- cutree(hclust_result, k = k_optimo)

# Añadir el clúster asignado a los datos originales
datos_fmm$Cluster <- as.factor(clusters)

# Realizar una PCA para reducir la dimensionalidad a 2D
pca_result <- prcomp(datos_scaled_fmm, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)

# Calcular la varianza explicada para los dos primeros componentes
explained_var <- 100 * (pca_result$sdev)^2 / sum((pca_result$sdev)^2)

# Calcular las proporciones para los dos primeros componentes
pc1_var <- explained_var[1]
pc2_var <- explained_var[2]

# Añadir la asignación de clústeres al dataframe de PCA
pca_data$Cluster <- datos_fmm$Cluster

# Graficar los clústeres con ggplot2
library(ggplot2)
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 4) +
  labs(
    title = "Clustering del modelo FMM con método jerárquico 'single' y k=2",
    x = paste("Primera Componente Principal (", round(pc1_var, 1), "%)", sep = ""),
    y = paste("Segunda Componente Principal (", round(pc2_var, 1), "%)", sep = "")
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12)
  )

# Cálculo de los ángulos alpha y beta en radianes
datos_fmm$alpha <- atan2(datos_fmm$alpha_sin, datos_fmm$alpha_cos)
datos_fmm$beta <- atan2(datos_fmm$beta_sin, datos_fmm$beta_cos)

#Solo para la tabla del TFM
datos_TFM<- datos_fmm %>% 
  select(-alpha_sin, -alpha_cos, -beta_sin, -beta_cos)

# Agrupar por Cluster y calcular el promedio de cada columna
datos_promedios <- datos_fmm %>% 
  select(-user,-alpha_sin, -alpha_cos, -beta_sin, -beta_cos)

datos_promedios <- as.data.frame(lapply(datos_promedios, as.numeric))
datos_promedios <- datos_promedios %>%
  group_by(Cluster) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))



# Crear data frames separados para cada clúster
datos_cluster_1 <- datos_promedios %>% filter(Cluster == 1)
datos_cluster_2 <- datos_promedios %>% filter(Cluster == 2)


par(mfrow=c(1,2))

generateFMM(datos_cluster_1$V1,datos_cluster_1$A,datos_cluster_1$alpha,datos_cluster_1$beta,datos_cluster_1$omega)
generateFMM(datos_cluster_2$V1,datos_cluster_2$A,datos_cluster_2$alpha,datos_cluster_2$beta,datos_cluster_2$omega)


##### Cluster 1
summary_minutos_user2 <- summary_15min_acotado %>%
  filter(user=="user_11")


resultado_fmm_2 <- fitFMM(vData = summary_minutos_user2$WMFC)

# Mostrar los resultados
print(resultado_fmm_2)

plotFMM(resultado_fmm_2)


##### Cluster 2
summary_minutos_user13 <- summary_15min_acotado %>%
  filter(user=="user_13")


resultado_fmm_13 <- fitFMM(vData = summary_minutos_user13$WMFC,)

# Mostrar los resultados
print(resultado_fmm_13)

plotFMM(resultado_fmm_13)



#############################################################
# Compara cosinor
############################################################

# Definir el número óptimo de clústeres
k_optimo <- 2  # Este valor se elige en base al método del codo

# Realizar el clustering jerárquico con el método "single"
hclust_result <- hclust(dist(datos_scaled_fmm), method = "single")

# Cortar el dendrograma para obtener 5 clústeres
clusters <- cutree(hclust_result, k = k_optimo)

# Añadir el clúster asignado a los datos originales
datos_fmm$Cluster <- as.factor(clusters)

# Realizar una PCA para reducir la dimensionalidad a 2D
pca_result <- prcomp(datos_scaled_fmm, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)

# Calcular la varianza explicada para los dos primeros componentes
explained_var <- 100 * (pca_result$sdev)^2 / sum((pca_result$sdev)^2)

# Calcular las proporciones para los dos primeros componentes
pc1_var <- explained_var[1]
pc2_var <- explained_var[2]

# Añadir la asignación de clústeres al dataframe de PCA
pca_data$Cluster <- datos_fmm$Cluster

# Graficar los clústeres con ggplot2
library(ggplot2)
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 4) +
  labs(
    title = "Clustering del modelo FMM con método jerárquico 'single' y k=5",
    x = paste("Primera Componente Principal (", round(pc1_var, 1), "%)", sep = ""),
    y = paste("Segunda Componente Principal (", round(pc2_var, 1), "%)", sep = "")
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12)
  )

datos_TFM<- datos_fmm %>% 
  select(-alpha_sin, -alpha_cos, -beta_sin, -beta_cos)





















#############################################################
# Compara cosinor
############################################################


# Agrupar por usuario y hora, y calcular el promedio de HRMV
summary_df <- Actigraphdatos_filtrados %>%
  group_by(user, Hora) %>%
  summarise(HRMV = mean(HRMV, na.rm = TRUE),
            cambios_posicion = sum(cambio_posicion, na.rm = TRUE),
            cambios_posicion_modificado = ifelse(cambios_posicion == 0, 0.5, cambios_posicion),
            WMFC = HRMV * cambios_posicion_modificado,)


library(cosinor2)

summary_df_user9 <- summary_15min_acotado %>%
  filter(user=="user_9")


# Modificar hora e intervalo
summary_df_user9$hora_in <- summary_df_user9$hora_in-min(summary_df_user9$hora_in)
summary_df_user9$intervalo_15min <- summary_df_user9$intervalo_15min-min(summary_df_user9$intervalo_15min)

# Ajustar el modelo cosinor
resultado_cosinor <- cosinor.lm(WMFC ~ time(hora_in),period = 24, data = summary_df_user9)

resultado_cosinor2 <- cosinor.lm(WMFC ~ time(intervalo_15min),period = 96, data = summary_df_user9)

# Mostrar los resultados
summary(resultado_cosinor)

library(ggplot2)

# Gráfico original para user_9
gg_original <- ggplot(summary_df_user9, aes(x = intervalo_15min, y = WMFC)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Promedio de wMvFC por Hora para user_9 vs Predicción cosinor",
       x = "Intervalos de 15 minutos",
       y = "wMvFC") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Gráfico de predicción cosinor en rojo
gg_cosinor <- ggplot_cosinor.lm(resultado_cosinor2) +
  geom_line(aes(color = "Cosinor"), size = 1.2) +
  scale_color_manual(values = c("Cosinor" = "red"))

# Extraer datos del gráfico cosinor
cosinor_data <- ggplot_build(gg_cosinor)$data[[1]]

# Combinar los gráficos
combined_plot <- gg_original +
  geom_line(data = cosinor_data, aes(x, y, color = "Cosinor"), size = 1.2) +
  scale_color_manual(values = c("Datos" = "blue", "Cosinor" = "red")) +
  labs(color = "Modelo") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(size = 14),  # Aumentar tamaño del texto de los ejes
    axis.title = element_text(size = 16)  # Aumentar tamaño del título de los ejes
  )

# Mostrar el gráfico combinado
print(combined_plot)


###########################

# Para el usuario 9 vemos como cosinor no detecta la tentencia ascendente
summary_comp_9<- summary_15min_acotado %>%
  filter(user=="user_9")


resultado_fmm__comp9 <- fitFMM(vData = summary_comp_9$WMFC,)

# Mostrar los resultados
print(resultado_fmm__comp9)

plotFMM(resultado_fmm__comp9)


############################################################
# Para el usuario 16 vemos como cosinor no puede alcanzar la subida

summary_df_user16 <- summary_15min_acotado %>%
  filter(user=="user_16")

summary_df_user16$intervalo_15min <- summary_df_user16$intervalo_15min-min(summary_df_user16$intervalo_15min)

# Ajustar el modelo cosinor

resultado_cosinor3 <- cosinor.lm(WMFC ~ time(intervalo_15min),period = 96, data = summary_df_user16)

# Mostrar los resultados
summary(resultado_cosinor)

library(ggplot2)


# Gráfico original para user_9
gg_original <- ggplot(summary_df_user16, aes(x = intervalo_15min, y = WMFC)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Promedio de wMvFC por Hora para user_16 vs Predicción cosinor",
       x = "Intervalos de 15 minutos",
       y = "wMvFC") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Gráfico de predicción cosinor en rojo
gg_cosinor <- ggplot_cosinor.lm(resultado_cosinor2) +
  geom_line(aes(color = "Cosinor"), size = 1.2) +
  scale_color_manual(values = c("Cosinor" = "red"))

# Extraer datos del gráfico cosinor
cosinor_data <- ggplot_build(gg_cosinor)$data[[1]]

# Combinar los gráficos
combined_plot <- gg_original +
  geom_line(data = cosinor_data, aes(x, y, color = "Cosinor"), size = 1.2) +
  scale_color_manual(values = c("Datos" = "blue", "Cosinor" = "red")) +
  labs(color = "Modelo") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(size = 14),  # Aumentar tamaño del texto de los ejes
    axis.title = element_text(size = 16)  # Aumentar tamaño del título de los ejes
  )

# Mostrar el gráfico combinado
print(combined_plot)



#FMM
summary_comp_16<- summary_15min_acotado %>%
  filter(user=="user_16")


resultado_fmm__comp16 <- fitFMM(vData = summary_comp_16$WMFC,)

# Mostrar los resultados
print(resultado_fmm__comp16)

plotFMM(resultado_fmm__comp16)
############################################################