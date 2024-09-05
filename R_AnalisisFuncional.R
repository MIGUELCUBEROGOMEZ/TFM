
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)
library(openxlsx)
library(skimr)
library(dplyr)
library(dtwclust)
library(proxy)
library(lubridate)
library(plotly)

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





Actigraphdatos <- rbindlist(list(Actigraphdatos1, Actigraphdatos2), use.names = TRUE, fill = TRUE)
Actigraphdatos$time <- as.POSIXct(Actigraphdatos$time, format="%H:%M:%S")
Actigraphdatos$day <- ifelse(Actigraphdatos$day == -29, 2, Actigraphdatos$day)

#Error en las horas de user 8 y user 9

#Actigraphdatos <- Actigraphdatos %>%filter((user %in% c("user_14")))


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


min_max_df <- summary_15min %>%
  group_by(user) %>%
  summarise(
    min_intervalo = min(hora_in, na.rm = TRUE),
    max_intervalo = max(hora_in, na.rm = TRUE)
  )

cota_inicio<-max(min_max_df$min_intervalo)
cota_fin<-min(min_max_df$max_intervalo)

summary_15min_acotado <- summary_15min %>%
  filter(intervalo_15min >= cota_inicio & intervalo_15min <= cota_fin)



# Gráfico para un usuario
summary_15minutos_user1 <- summary_15min %>%
  filter(user=="user_3")

library(gridExtra)
library(ggplot2)

x_inicio<-24.5
x_fin<-29.75

# Crear los gráficos y guardarlos en variables
# Definir los gráficos con ajuste de tamaño de texto en el eje X
# Definir los gráficos sin leyenda de 'user'
grafico1 <- ggplot(summary_15minutos_user1, aes(x = hora_in, y = WMFC)) +
  geom_line(size = 1.2, color = "brown2") +  # Definir color directamente en geom_line
  labs(title = "Evolución de wMvFC",
       x = "Hora",
       y = "wMvFC") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),   # Aumentar tamaño del texto en el eje X
        axis.text.y = element_text(size = 14),   # Aumentar tamaño del texto en el eje Y
        axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
        axis.title.y = element_text(size = 16)) + # Aumentar tamaño del título del eje Y
  annotate("rect", xmin = x_inicio, xmax = x_fin, ymin = 0, ymax = 60, fill = "lightblue", alpha = 0.3)

grafico2 <- ggplot(summary_15minutos_user1, aes(x = hora_in, y = HR_scaled)) +
  geom_line(size = 1.2, color = "brown2") +  # Definir color directamente en geom_line
  labs(title = "Evolución de la Frecuencia Cardiaca escalada",
       x = "Hora",
       y = "Frecuencia Cardiaca") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),   # Aumentar tamaño del texto en el eje X
        axis.text.y = element_text(size = 14),   # Aumentar tamaño del texto en el eje Y
        axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
        axis.title.y = element_text(size = 16)) + # Aumentar tamaño del título del eje Y
  annotate("rect", xmin = x_inicio, xmax = x_fin, ymin = 0.2, ymax = 0.55, fill = "lightblue", alpha = 0.3)

grafico3 <- ggplot(summary_15minutos_user1, aes(x = hora_in, y = Vector_Magnitude_scaled)) +
  geom_line(size = 1.2, color = "brown2") +  # Definir color directamente en geom_line
  labs(title = "Evolución del movimiento de la muñeca escalado",
       x = "Hora",
       y = "Movimiento de la muñeca") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),   # Aumentar tamaño del texto en el eje X
        axis.text.y = element_text(size = 14),   # Aumentar tamaño del texto en el eje Y
        axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
        axis.title.y = element_text(size = 16)) + # Aumentar tamaño del título del eje Y
  annotate("rect", xmin = x_inicio, xmax = x_fin, ymin = 0, ymax = 0.6, fill = "lightblue", alpha = 0.3)

grafico4 <- ggplot(summary_15minutos_user1, aes(x = hora_in, y = cambios_posicion)) +
  geom_line(size = 1.2, color = "brown2") +  # Definir color directamente en geom_line
  labs(title = "Evolución de los cambios de posición",
       x = "Hora",
       y = "Cambios de posición") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),   # Aumentar tamaño del texto en el eje X
        axis.text.y = element_text(size = 14),   # Aumentar tamaño del texto en el eje Y
        axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
        axis.title.y = element_text(size = 16)) + # Aumentar tamaño del título del eje Y
  annotate("rect", xmin = x_inicio, xmax = x_fin, ymin = 0, ymax = 200, fill = "lightblue", alpha = 0.3)

# Mostrar los gráficos en una disposición de 2x2
grid.arrange(grafico1, grafico2, grafico3, grafico4, ncol = 2, nrow = 2)



expected_intervalos <- seq(41, 130)

# Encontrar valores faltantes por usuario
missing_values <- summary_15min_acotado %>%
  group_by(user) %>%
  summarise(
    missing_intervalos = list(expected_intervalos[!(expected_intervalos %in% intervalo_15min)]),
    .groups = 'drop'
  ) %>%
  filter(lengths(missing_intervalos) > 0)  # Filtrar usuarios con valores faltantes

print(missing_values)





#############################################
          # ANALISIS FUNCIONAL #
#############################################

summary_15min_prueba <- summary_15min %>%
  filter(hora_in < 34)

# Crear la lista donde cada elemento es un vector correspondiente a un usuario
lista_vectores_hora <- split(summary_15min_prueba$hora_in, summary_15min_prueba$user)
lista_vectores_WMFC <- split(summary_15min_prueba$WMFC, summary_15min_prueba$user)


# Mostrar la lista
#print(lista_vectores_hora)
#print(lista_vectores_WMFC)

library("fdapace")

#nRefGRID aumenta los valores de la hora es decir supone que tenemos 100
opciones <- list(dataType = 'Sparse')  # Indica que los datos son de tipo disperso

modeloTFM <- FPCA(lista_vectores_WMFC,lista_vectores_hora,optns = opciones)



plot(modeloTFM$workGrid, modeloTFM$mu, type="l", col="blue",
     xlab="Time",
     ylab="mu(t)",lwd=6, cex.axis=2, cex.lab=1.5)


for(j in 1:22){
  points(lista_vectores_hora[[j]],lista_vectores_WMFC[[j]],col="red",lwd=3)
}
dev.off()

par(mfrow=c(2,2))
plot(modeloTFM$workGrid, modeloTFM$phi[,1], type="l", col="blue",
     xlab="Time",
     ylab="First FPC",lwd=3, cex.axis=2, cex.lab=1.5)
plot(modeloTFM$workGrid, modeloTFM$phi[,2], type="l", col="blue",
     xlab="Time",
     ylab="Second FPC",lwd=3, cex.axis=2, cex.lab=1.5)
plot(modeloTFM$workGrid, modeloTFM$phi[,3], type="l", col="blue",
     xlab="Time",
     ylab="Third FPC",lwd=3, cex.axis=2, cex.lab=1.5)
plot(modeloTFM$workGrid, modeloTFM$phi[,4], type="l", col="blue",
     xlab="Time",
     ylab="Fourth FPC",lwd=3, cex.axis=2, cex.lab=1.5)

modeloTFM$cumFVE

library("fields")

par(mfrow=c(1,1))

image.plot(modeloTFM$workGrid, modeloTFM$workGrid, modeloTFM$fittedCov,
           xlab="Time",
           ylab="Time",lwd=3, cex.axis=2, cex.lab=1.5)


plot(modeloTFM)

eigenfunctions <- modeloTFM$phi

#Comprobar que el producto es 0
sum(eigenfunctions[,1]*eigenfunctions[,2])

# Crear el gráfico con el tamaño de línea aumentado
plot(modeloTFM$workGrid, eigenfunctions[, 1], type = "l", col = "blue", lwd = 4,  # Aumentar grosor de línea
     main = "Autofunciones", xlab = "Tiempo", ylab = "Valor", ylim = c(-0.5, 0.5))

# Agregar la segunda línea con el tamaño de línea aumentado
lines(modeloTFM$workGrid, eigenfunctions[, 2], col = "red", lwd = 4)  # Aumentar grosor de línea
lines(modeloTFM$workGrid, eigenfunctions[, 3], col = "darkgreen", lwd = 4)  # Aumentar grosor de línea

# Agregar leyenda al gráfico
legend("topright", legend = c("phi_1", "phi_2", "phi_3"), col = c("blue", "red","darkgreen"), lty = 1, lwd = 2)  # Ajustar grosor de línea en la leyenda



##########################################################
#Reconstrucción de la variable inicial para un usuario en concreto:#
##########################################################

# 1. Selecciona el índice del usuario para el que deseas reconstruir la variable inicial
i <- 4  # Cambia '1' por el índice del usuario de interés

# 2. Extrae los elementos necesarios del modelo FPCA
xi_scores <- modeloTFM$xiEst[i, ]  # Scores del usuario i
mu <- modeloTFM$mu  # Función media
phi <- modeloTFM$phi  # Autofunciones
workGrid <- modeloTFM$workGrid  # Grid de evaluación

# 3. Realiza la reconstrucción de la variable inicial
# Calcular la suma ponderada de las autofunciones
reconstruction <- mu + phi %*% xi_scores

# 4. Crear un data frame para facilitar la visualización
df_reconstruction_user_i <- data.frame(Time = workGrid, Reconstruction = reconstruction)

# 5. Visualiza la reconstrucción
library(ggplot2)
# Supongamos que df_reconstruction_user_i y summary_15minutos_user1 tienen columnas con los mismos nombres para el eje x
df_combined <- data.frame(
  Time = c(df_reconstruction_user_i$Time, summary_15minutos_user1$hora_in),
  Value = c(df_reconstruction_user_i$Reconstruction, summary_15minutos_user1$WMFC),
  Source = c(rep("Reconstruction", nrow(df_reconstruction_user_i)), rep("Original", nrow(summary_15minutos_user1)))
)

# Grafico con las dos líneas superpuestas

ggplot(df_combined, aes(x = Time, y = Value)) +
  geom_line(data = df_combined[df_combined$Source == "Original",], size = 1, color = "black") + 
  geom_line(data = df_combined[df_combined$Source == "Reconstruction",], size = 1, color = "red") +
  labs(title = paste("Reconstrucción de los datos para el Usuario", i),
       x = "Tiempo",
       y = "Valor") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  # Añadir leyenda manualmente usando annotate
  annotate("text", x = Inf, y = Inf, label = "Datos", hjust = 1.1, vjust = 2, size = 5, color = "black") +
  annotate("text", x = Inf, y = Inf, label = "ACPF", hjust = 1.1, vjust = 4, size = 5, color = "red")
#############################
#Clustering usando DTW Y HCLUST#
#############################

#Pimero reconstruimos todos los usuarios y los agregamos por columnas

n<- length(modeloTFM$xiEst[,1 ])
mu <- modeloTFM$mu  # Función media
phi <- modeloTFM$phi  # Autofunciones
workGrid <- modeloTFM$workGrid  # Grid de evaluación
df_reconstruction <- data.frame(matrix(ncol = n, nrow = length(workGrid)))
colnames(df_reconstruction) <- names(lista_vectores_WMFC)

for (i in 1:n) {
  # Extrae los elementos necesarios del modelo FPCA
  xi_scores <- modeloTFM$xiEst[i, ]  # Scores del usuario i
  # Calcular la suma ponderada de las autofunciones
  reconstruction <- mu + phi %*% xi_scores
  df_reconstruction[, i] <- reconstruction
}

set.seed(123)  # Semilla para reproducibilidad

series_list <- as.list(df_reconstruction)
# Cada elemento de `series_list` es un vector que representa una serie temporal
# Normaliza las series temporales (opcional pero recomendado)
normalized_series <- lapply(series_list, scale)

# Calcula la matriz de distancias usando DTW
dist_matrix <- proxy::dist(normalized_series, method = "dtw")

hc_clust <- hclust(dist_matrix, method = 'ward.D2')

# Cortar el dendrograma en 3 grupos
clusters <- cutree(hc_clust, k = 3)

# Plotear el dendrograma
plot(hc_clust)

# Dibujar rectángulos alrededor de los 3 clusters
rect.hclust(hc_clust, k = 3, border = "red")


df_clustering <- as.data.frame(t(df_reconstruction))

df_clustering$Cluster <- as.factor(clusters)

## Gráfico


# Crear el data frame base para ggplot
df_clustering$Usuario <- rownames(df_clustering)  # Añadir los nombres de usuarios como una columna

# Crear un gráfico base vacío
# Crear el gráfico base
p <- ggplot() +
  theme_minimal() +
  labs(
    title = "Series Temporales de Usuarios Agrupadas por Clusters",
    x = "Tiempo",
    y = "Valor",
    color = "Cluster"
  ) +
  theme(
    axis.title.x = element_text(size = 14), # Tamaño del título del eje x
    axis.title.y = element_text(size = 14), # Tamaño del título del eje y
    axis.text.x = element_text(size = 12),  # Tamaño del texto de las etiquetas del eje x
    axis.text.y = element_text(size = 12)   # Tamaño del texto de las etiquetas del eje y
  )

# Agregar líneas para cada usuario
for (i in 1:nrow(df_clustering)) {
  p <- p + geom_line(data = data.frame(Tiempo = workGrid, 
                                       Valor = as.numeric(df_clustering[i, grepl("V", names(df_clustering))]), 
                                       Cluster = factor(df_clustering$Cluster[i])),
                     aes(x = Tiempo, y = Valor, color = Cluster), size = 1)
}

# Convertir el gráfico ggplot2 a plotly para hacerlo interactivo
p_interactivo <- ggplotly(p)

# Mostrar el gráfico interactivo
p_interactivo


##############################################v
## Prueba diferentes clusters y metodos
################################
# Cargar paquetes necesarios
library(cluster)
library(factoextra)
library(proxy)
library(dtwclust)  
library(mclust)    

# Definir el rango de clusters

# Convertir las series temporales en una lista adecuada para dtwclust
series_list <- as.list(df_reconstruction)
# Cada elemento de `series_list` es un vector que representa una serie temporal
# Normaliza las series temporales (opcional pero recomendado)
normalized_series <- lapply(series_list, scale)

# Definir el rango de clusters
n_clust <- 2:10

# Definir los métodos de clustering jerárquico a evaluar
hierarchical_methods <- c("ward.D2", "complete", "average", "single")

# Inicializar listas para guardar los resultados de silueta
sil_width_hclust <- vector("list", length(n_clust) * length(hierarchical_methods))
index_hclust <- 1

# Calcular el coeficiente de silueta para cada método de clustering jerárquico
for (i in seq_along(n_clust)) {
  for (method in hierarchical_methods) {
    # Calcular la matriz de distancias usando DTW
    dist_matrix <- proxy::dist(normalized_series, method = "dtw")
    
    # Clustering Jerárquico
    hc_clust <- hclust(dist_matrix, method = method)
    
    # Cortar el dendrograma en el número de clusters deseado
    clusters <- cutree(hc_clust, k = n_clust[i])
    
    # Calcular el coeficiente de silueta
    sil <- silhouette(clusters, dist_matrix)
    sil_width_hclust[[index_hclust]] <- list(Method = method, Clusters = n_clust[i], Silhouette_Width = mean(sil[, 3]))
    index_hclust <- index_hclust + 1
  }
}

# Convertir las listas de resultados en un data frame
sil_df_hclust <- do.call(rbind, lapply(sil_width_hclust, as.data.frame))

# Encontrar el mejor método basado en el coeficiente de silueta
best_method <- sil_df_hclust[which.max(sil_df_hclust$Silhouette_Width),]

# Imprimir el mejor método
print(best_method)

# Usar el mejor método para clustering
best_method_name <- as.character(best_method$Method)
best_k <- best_method$Clusters

# Recalcular la matriz de distancias usando DTW
dist_matrix <- proxy::dist(normalized_series, method = "dtw")

# Clustering Jerárquico con el mejor método
hc_clust <- hclust(dist_matrix, method = best_method_name)

# Cortar el dendrograma en el número óptimo de grupos
clusters <- cutree(hc_clust, k = best_k)

# Convertir los resultados de clustering a un data frame
df_clustering <- as.data.frame(t(df_reconstruction))
df_clustering$Cluster <- as.factor(clusters)

# Crear el gráfico base para ggplot
df_clustering$Usuario <- rownames(df_clustering)  # Añadir los nombres de usuarios como una columna

# Crear un gráfico base vacío
p <- ggplot() +
  theme_minimal() +
  labs(
    title = "Series Temporales de Usuarios Agrupadas por Clusters",
    x = "Tiempo",
    y = "Valor",
    color = "Cluster"
  )

# Agregar una línea para cada usuario
for (i in 1:nrow(df_clustering)) {
  p <- p + geom_line(data = data.frame(Tiempo = seq_len(ncol(df_reconstruction)), 
                                       Valor = as.numeric(df_clustering[i, grepl("V", names(df_clustering))]), 
                                       Cluster = factor(df_clustering$Cluster[i])),
                     aes(x = Tiempo, y = Valor, color = Cluster), size = 1)
}

# Convertir el gráfico ggplot2 a plotly para hacerlo interactivo
p_interactivo <- ggplotly(p)

# Mostrar el gráfico interactivo
p_interactivo
