
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)
library(openxlsx)
library(skimr)
library(dplyr)
library(corrplot)

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



##########################################
              #Saliva#
##########################################

# Gráfico para cortisol
cortisol_plot <- ggplot(Salivadatos, aes(x = SAMPLES, y = Cortisol.NORM, fill = SAMPLES)) +
  geom_boxplot() +
  labs(title = "Cortisol antes y después de dormir",
       x = "Muestra",
       y = "Cortisol") +
  theme_minimal() +
  theme(legend.position = "none")  # Eliminar leyenda

# Gráfico para melatonina
melatonina_plot <- ggplot(Salivadatos, aes(x = SAMPLES, y = Melatonin.NORM, fill = SAMPLES)) +
  geom_boxplot() +
  labs(title = "Melatonina antes y después de dormir",
       x = "Muestra",
       y = "Melatonina") +
  theme_minimal() +
  theme(legend.position = "none")  # Eliminar leyenda


# Mostrar los gráficos
print(cortisol_plot)
print(melatonina_plot)


# Crear un data frame para "before sleep"
df_before_sleep <- Salivadatos %>%
  filter(SAMPLES == "before sleep")

# Crear un data frame para "wake up"
df_wake_up <- Salivadatos %>%
  filter(SAMPLES == "wake up")

# Unir los data frames por usuario
df_combined <- df_before_sleep %>%
  rename(Cortisol.NORM_before_sleep = Cortisol.NORM, Melatonin.NORM_before_sleep = Melatonin.NORM) %>%
  inner_join(df_wake_up %>%
               rename(Cortisol.NORM_wake_up = Cortisol.NORM, Melatonin.NORM_wake_up = Melatonin.NORM), 
             by = "user")

cortisol_total <- c(df_combined$Cortisol.NORM_wake_up,df_combined$Cortisol.NORM_before_sleep)
melatonina_total <- c(df_combined$Melatonin.NORM_wake_up,df_combined$Melatonin.NORM_before_sleep)

#Diferencia por usuario
df_datos_normalizados <- df_combined %>%
  mutate(
    Cortisol_normalizado = (Cortisol.NORM_wake_up - Cortisol.NORM_before_sleep)/(max(cortisol_total)-min(cortisol_total)),
    Melatonin_normalizado = (Melatonin.NORM_before_sleep - Melatonin.NORM_wake_up  )/(max(melatonina_total)-min(melatonina_total))
  ) %>%
  select(user, Cortisol_normalizado, Melatonin_normalizado)


##########################################
          #Cuestionarios#
##########################################

df_cuestionario_simplifcado <- Cuestionariodatos %>%
  select(user,MEQ,Pittsburgh,Daily_stress)

df_cuestionario_saliva <- df_cuestionario_simplifcado %>%
  inner_join(df_datos_normalizados,by = "user")



df_numeric <- df_cuestionario_saliva %>%
  select_if(is.numeric)

cor_matrix <- cor(df_numeric, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black", 
         addCoef.col = "black")




##########################################
    ############ TM, VM #############
##########################################



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
quantiles_HR <- quantile(Actigraphdatos$HR, probs = c(0.05, 0.95), na.rm = TRUE)

# Paso 1: Filtrar los datos para los cuantiles de Vector.Magnitude
Actigraphdatos_filtrados <- Actigraphdatos %>%
  filter(Vector.Magnitude >= quantiles_Vector_Magnitude[1] & 
           Vector.Magnitude <= quantiles_Vector_Magnitude[2]&
           HR >= quantiles_HR[1] & 
           HR <= quantiles_HR[2])

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
  group_by(user,Tiempo) %>%
  summarise(HRMV = mean(HRMV, na.rm = TRUE),
            HR = mean(HR, na.rm = TRUE),
            HR_scaled = mean(HR_scaled, na.rm = TRUE),
            Vector.Magnitude = mean(Vector.Magnitude, na.rm = TRUE),
            Vector_Magnitude_scaled = mean(Vector.Magnitude_scaled, na.rm = TRUE),
            cambios_posicion = sum(cambio_posicion, na.rm = TRUE),
            cambios_posicion_modificado = ifelse(cambios_posicion == 0, 0.5, cambios_posicion),
            WMFC = HRMV * cambios_posicion_modificado,
            intervalo_15min = floor(Tiempo / 15)) %>%
  distinct()



# Agrupar por usuario e intervalo de 15 minutos y calcular estadísticas
summary_15min <- summary_minutos %>%
  group_by(user, intervalo_15min) %>%
  summarise(
    HR = mean(HR, na.rm = TRUE),
    HRMV = mean(HRMV, na.rm = TRUE),
    HR_scaled = mean(HR_scaled, na.rm = TRUE),
    Vector.Magnitude = mean(Vector.Magnitude, na.rm = TRUE),
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



######################################
    #Calculo indices#
#####################################


#Usamos el método de la ventana adyacente para reducir de
# en 5h de 329199 a 2202 sumas
#en 10h de 480398 a 1604 sumas




# Crear vectores vacíos para guardar los resultados
user_vector <- c()
acumulado_5h_vector <- c()
start_time_5h_vector <- c()
acumulado_10h_vector <- c()
start_time_10h_vector <- c()

# Función para calcular acumulados de 5 y 10 horas utilizando ventana deslizante
calcular_acumulado_ventana_horas <- function(df) {
  n <- nrow(df)
  
  if (n < 300) return()  # Si hay menos de 300 filas, devolver sin cambios
  
  # Calcular la suma inicial para la primera ventana de 5 horas (300 minutos)
  suma_5h <- sum(df$HR[1:300], na.rm = TRUE)/300
  # Inicializar la suma para la ventana de 10 horas (600 minutos) si es posible
  if (n >= 600) {
    suma_10h <- sum(df$HR[1:600], na.rm = TRUE)/600
  } else {
    suma_10h <- NA
  }
  
  # Añadir la primera fila de resultados a los vectores
  user_vector <<- c(user_vector, df$user[1])
  acumulado_5h_vector <<- c(acumulado_5h_vector, suma_5h)
  start_time_5h_vector <<- c(start_time_5h_vector, df$Tiempo[1])
  acumulado_10h_vector <<- c(acumulado_10h_vector, suma_10h)
  start_time_10h_vector <<- c(start_time_10h_vector, df$Tiempo[1])
  
  # Calcular el resto de las sumas utilizando la técnica de ventana deslizante
  for (i in 2:(n-299)) {
    suma_5h <- suma_5h + (df$HR[i+299] - df$HR[i-1])/300
    start_time_5h <- df$Tiempo[i]

    if (i <= (n-599)) {
      suma_10h <- suma_10h + (df$HR[i+599]  - df$HR[i-1])/600
      start_time_10h <- df$Tiempo[i]
    } else {
      suma_10h <- NA
      start_time_10h <- NA
    }
    
    # Añadir los resultados actuales a los vectores
    user_vector <<- c(user_vector, df$user[i])
    acumulado_5h_vector <<- c(acumulado_5h_vector, suma_5h)
    start_time_5h_vector <<- c(start_time_5h_vector, start_time_5h)
    acumulado_10h_vector <<- c(acumulado_10h_vector, suma_10h)
    start_time_10h_vector <<- c(start_time_10h_vector, start_time_10h)
  }
}

# Aplicar la función por cada usuario en summary_minutos (agregado por minutos)
usuarios_minutos <- unique(summary_minutos$user)
for (user in usuarios_minutos) {
  df_user_min <- summary_minutos %>% filter(user == !!user)
  calcular_acumulado_ventana_horas(df_user_min)
}

# Crear el dataframe final con los resultados acumulados
resultados_acumulados <- data.frame(
  user = user_vector, 
  acumulado_5h = acumulado_5h_vector, 
  start_time_5h = start_time_5h_vector, 
  acumulado_10h = acumulado_10h_vector, 
  start_time_10h = start_time_10h_vector,
  stringsAsFactors = FALSE
)

# Ver los resultados
print(resultados_acumulados)

# Resumir los resultados en horas
summary_resultados_acumulados <- resultados_acumulados %>%
  group_by(user) %>%
  summarise(
    max_acumulado_5h = max(acumulado_5h, na.rm = TRUE),
    max_start_time_5h = (150+start_time_5h[which.max(acumulado_5h)])/60,
    min_acumulado_5h = min(acumulado_5h, na.rm = TRUE),
    min_start_time_5h = (150+start_time_5h[which.min(acumulado_5h)])/60,
    max_acumulado_10h = max(acumulado_10h, na.rm = TRUE),
    max_start_time_10h = (300+start_time_10h[which.max(acumulado_10h)])/60,
    min_acumulado_10h = min(acumulado_10h, na.rm = TRUE),
    min_start_time_10h = (300+start_time_10h[which.min(acumulado_10h)])/60,
    AA = max_acumulado_10h - min_acumulado_5h,
    RA = (max_acumulado_10h - min_acumulado_5h)/(max_acumulado_10h + min_acumulado_5h)
  )

# Ver los resultados resumidos
print(summary_resultados_acumulados)

resultadostfm <- summary_resultados_acumulados %>%
  select(user,RA,AA)
##################################
      #Buscamos relaciones#
##################################
df_awakening<- Sueñodatos %>%
  group_by(user)%>%
  summarise(Number.of.Awakenings = sum(Number.of.Awakenings),
            Total.Sleep.Time..TST. = sum(Total.Sleep.Time..TST.),
            Total.Minutes.in.Bed = sum(Total.Minutes.in.Bed) )


#Actigraphdatos <- Actigraphdatos %>%filter((user %in% c("user_9")))
df_cuestionario_saliva_indices<- data.frame()
df_cuestionario_saliva_indices <- df_cuestionario_saliva %>%
  inner_join(summary_resultados_acumulados,by = "user")
 
df_cuestionario_saliva_indices <- df_cuestionario_saliva_indices %>% filter(user != "user_11")

df_cuestionario_saliva_indices_awakening <- df_cuestionario_saliva_indices %>%
  inner_join(df_awakening,by = "user")

df_numeric2 <- df_cuestionario_saliva_indices_awakening %>% 
  select_if(is.numeric)

cor_matrix2 <- cor(df_numeric2, use = "complete.obs")
par(mfrow=c(1,1))

corrplot(cor_matrix2, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black", 
         addCoef.col = "black")


#Ajustamos las regresiones R2=0.71

par(mfrow=c(2,2))

datameq <- df_numeric2 %>%
  select(MEQ,Melatonin_normalizado,max_start_time_10h, min_start_time_10h)

lmMEQ <- lm(MEQ ~. ,data=df_numeric2)
summary(lmMEQ)
plot(lmMEQ)

#Ajustamos las regresiones R2=0.58
lmPittsburgh <- lm(Pittsburgh ~. ,data=df_numeric2)
summary(lmPittsburgh)
plot(lmPittsburgh)


########################################
    # Indice de Circadianidad #
########################################

library("stats")

# Función para calcular el Índice de Circadianidad (CI) para cada usuario
calcular_ci_por_usuario <- function(df) {
  
  # Obtener una lista de usuarios únicos
  usuarios <- unique(df$user)
  
  # Crear una lista para almacenar los resultados
  resultados <- list()
  
  # Recorrer cada usuario
  for (usuario in usuarios) {
    
    # Filtrar los datos para el usuario actual
    user_data <- subset(df, user == usuario)
    
    # Extraer los valores de frecuencia cardiaca
    frecuencia_cardiaca <- user_data$HR
    
    # Comprobar si hay suficiente datos para aplicar FFT
    if (length(frecuencia_cardiaca) < 2) {
      warning(paste("Datos insuficientes para aplicar FFT para el usuario", usuario))
      next
    }
    
    # Aplicar la Transformada Rápida de Fourier (FFT)
    fft_result <- fft(frecuencia_cardiaca)
    
    # Calcular la potencia de cada armónica
    power_spectrum <- Mod(fft_result)^2
    power_spectrum <- power_spectrum / sum(power_spectrum)
    
    # Número de armónicas a considerar
    num_harmonics <- 24
    num_harmonics <- min(num_harmonics, length(power_spectrum) / 2)
    
    # Potencia de la primera armónica
    P1 <- power_spectrum[2]  # La primera armónica está en el índice 2
    
    # Suma de las potencias de las primeras 12 armónicas
    P_sum <- sum(power_spectrum[2:(num_harmonics + 1)])
    
    # Calcular el Índice de Circadianidad (CI)
    CI <- P1 / P_sum
    
    # Almacenar el resultado en la lista
    resultados[[as.character(usuario)]] <- CI
  }
  
  # Convertir la lista en un data frame
  resultados_df <- data.frame(user = names(resultados), CI = unlist(resultados))
  
  return(resultados_df)
}

resultados_ci <- calcular_ci_por_usuario(summary_minutos)
 

 print(resultados_ci)
 
 ################################
 # Función para calcular el Índice de Variabilidad (IV) para cada usuario
 calcular_iv_por_usuario <- function(df) {
   
   # Obtener una lista de usuarios únicos
   usuarios <- unique(df$user)
   
   # Crear una lista para almacenar los resultados
   resultados <- list()
   
   # Recorrer cada usuario
   for (usuario in usuarios) {
     
     # Filtrar los datos para el usuario actual
     user_data <- subset(df, user == usuario)
     
     # Extraer los valores de la frecuencia cardíaca
     frecuencia_cardiaca <- user_data$HR
     
     # Comprobar si hay suficientes datos para el cálculo
     if (length(frecuencia_cardiaca) < 2) {
       warning(paste("Datos insuficientes para calcular IV para el usuario", usuario))
       next
     }
     
     # Calcular la diferencia sucesiva
     diff_frecuencia <- diff(frecuencia_cardiaca)
     
     # Calcular la suma de los cuadrados de las diferencias sucesivas
     sum_diff_squared <- sum(diff_frecuencia^2)
     
     # Calcular la media de la frecuencia cardíaca
     mean_frecuencia <- mean(frecuencia_cardiaca)
     
     # Calcular la suma de los cuadrados de las diferencias respecto a la media
     sum_squared_diff_mean <- sum((frecuencia_cardiaca - mean_frecuencia)^2)
     
     # Calcular el Índice de Variabilidad (IV)
     n <- length(frecuencia_cardiaca)
     IV <- (n * sum_diff_squared) / ((n - 1) * sum_squared_diff_mean)
     
     # Almacenar el resultado en la lista
     resultados[[as.character(usuario)]] <- IV
   }
   
   # Convertir la lista en un data frame
   resultados_df <- data.frame(user = names(resultados), IV = unlist(resultados))
   
   return(resultados_df)
 }
 
 # Supongamos que 'summary_minutos' es tu dataframe y contiene la columna 'HR'
 resultados_iv <- calcular_iv_por_usuario(summary_minutos)
 
 # Imprimir los resultados
 print(resultados_iv)

 
 
 resultados_ci_VAR_caracteristicas <- resultados_ci %>%
   inner_join(Caracteristicasdatos, by = "user")%>%
   inner_join(resultados_iv, by = "user")%>%
   mutate(Height=Height/100)
 
 
 df_numeric3 <- resultados_ci_VAR_caracteristicas %>%
   select_if(is.numeric)
 
 
 mean_age <- df_numeric3 %>%
   filter(user != "user_18") %>%
   pull(Age) %>%
   mean(na.rm = TRUE)
 
 
 df_numeric3 <- df_numeric3 %>%
   mutate(Age = ifelse(Age != 0,Age, mean_age ))
 df_numeric3 <- df_numeric3[c("CI", "IV", "Weight", "Height", "Age")]
 
 cor_matrix <- cor(df_numeric3, use = "complete.obs")
 
 
 par(mfrow=c(1,1))
 
 corrplot(cor_matrix, method = "color", type = "upper", 
          tl.cex = 0.8, tl.col = "black", 
          addCoef.col = "black")
 
 
 
 resultados_ci_VAR_caracteristicas <- resultados_ci_VAR_caracteristicas%>%
   mutate(Age = ifelse(Age != 0,Age, mean_age ),
          IMC= Weight/(Height)^2)
 

 
 df_numeric4 <- resultados_ci_VAR_caracteristicas %>%
   select_if(is.numeric)
 
 cor_matrix2 <- cor(df_numeric4, use = "complete.obs")
 
 
 par(mfrow=c(1,1))
 
 corrplot(cor_matrix2, method = "color", type = "upper", 
          tl.cex = 0.8, tl.col = "black", 
          addCoef.col = "black")
 
 
 
 
 model<-lm(IMC ~ CI + IV  + Age , data = resultados_ci_VAR_caracteristicas)
 
 par(mfrow=c(2,2))
 plot(model)
 summary(model)
 
 
 
  
 # Gráfico para  user10
 summary_15minutos_user10 <- summary_minutos %>%
   filter(user %in% c( "user_10")) %>%
   mutate(hora_in = intervalo_15min * 15 / 60)
 
 #Gráfico para un usuario
 ggplot(summary_15minutos_user10, aes(x = hora_in, y = HR, color = user)) +
   geom_line(size = 1.2) +
   labs(title = paste("Promedio de FC por Minuto para el Usuario 10"),
        x = "Hora",
        y = "Frecuencia cardiaca") +
   theme_minimal() +
   theme(legend.title = element_blank())+
   theme(axis.text.x = element_text(size = 14),   # Aumentar tamaño del texto en el eje X
         axis.text.y = element_text(size = 14),   # Aumentar tamaño del texto en el eje Y
         axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
         axis.title.y = element_text(size = 16)) 
 
 # Gráfico para  user13
 summary_15minutos_user13 <- summary_minutos %>%
   filter(user %in% c( "user_13")) %>%
   mutate(hora_in = intervalo_15min * 15 / 60)
 
 #Gráfico para un usuario
 ggplot(summary_15minutos_user13, aes(x = hora_in, y = HR, color = user)) +
   geom_line(size = 1.2) +
   labs(title = paste("Promedio de FC por Minuto para el Usuario 13"),
        x = "Hora",
        y = "Frecuencia cardiaca") +
   theme_minimal() +
   theme(legend.title = element_blank())+
   theme(axis.text.x = element_text(size = 14),   # Aumentar tamaño del texto en el eje X
         axis.text.y = element_text(size = 14),   # Aumentar tamaño del texto en el eje Y
         axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
         axis.title.y = element_text(size = 16)) 
 
 
 # Gráfico para  user2
 summary_15minutos_user2 <- summary_minutos %>%
   filter(user %in% c( "user_2")) %>%
   mutate(hora_in = intervalo_15min * 15 / 60)
 
 #Gráfico para un usuario
 ggplot(summary_15minutos_user2, aes(x = hora_in, y = HR, color = user)) +
   geom_line(size = 1.2) +
   labs(title = paste("Promedio de FC por Minuto para el Usuario 2"),
        x = "Hora",
        y = "Frecuencia cardiaca") +
   theme_minimal() +
   theme(legend.title = element_blank())+
   theme(axis.text.x = element_text(size = 14),   # Aumentar tamaño del texto en el eje X
         axis.text.y = element_text(size = 14),   # Aumentar tamaño del texto en el eje Y
         axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
         axis.title.y = element_text(size = 16)) 
 
 # Gráfico para  user19
 summary_15minutos_user19 <- summary_minutos %>%
   filter(user %in% c( "user_19")) %>%
   mutate(hora_in = intervalo_15min * 15 / 60)
 
 #Gráfico para un usuario
 ggplot(summary_15minutos_user19, aes(x = hora_in, y = HR, color = user)) +
   geom_line(size = 1.2) +
   labs(title = paste("Promedio de FC por Minuto para el Usuario 19"),
        x = "Hora",
        y = "Frecuencia cardiaca") +
   theme_minimal() +
   theme(legend.title = element_blank())+
   theme(axis.text.x = element_text(size = 14),   # Aumentar tamaño del texto en el eje X
         axis.text.y = element_text(size = 14),   # Aumentar tamaño del texto en el eje Y
         axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
         axis.title.y = element_text(size = 16)) 
 
 
 
 
 #tiene que ver con la varianza?
 summary_var <- summary_minutos %>%
   group_by(user) %>%
   summarise(
     Var_HR = var(HR)
   )


 
library(plotly)

resultados_ci_VAR_caracteristicas <- resultados_ci_VAR_caracteristicas%>%
  mutate(Age = ifelse(Age != 0,Age, mean_age ),
         IMC= Weight/(Height)^2)

print(cor(resultados_ci_VAR_caracteristicas$IMC,resultados_ci$ci))
  
 # Crear un gráfico con ggplot2
p<-ggplot(resultados_ci_VAR_caracteristicas, aes(x = CI, y = IMC)) +
   geom_point(size = 3) +  # Añadir puntos
   geom_smooth(method = "lm", color = "blue", size = 1.5) +  # Añadir línea de regresión
   labs(title = "Relación entre CI e IMC", x = "CI", y = "IMC") +
   theme_minimal() +
   theme(
     axis.title.x = element_text(size = 14),  # Tamaño del texto del eje x
     axis.title.y = element_text(size = 14),  # Tamaño del texto del eje y
     axis.text.x = element_text(size = 12),    # Tamaño del texto de las etiquetas del eje x
     axis.text.y = element_text(size = 12)     # Tamaño del texto de las etiquetas del eje y
   )
p
 # Convertir el gráfico a interactivo con plotly
# p_interactivo <- ggplotly(p, tooltip = "text")
 
 # Mostrar el gráfico interactivo
 #p_interactivo
 
 
model<-lm(IMC ~ CI+ IV , data = resultados_ci_VAR_caracteristicas)

par(mfrow=c(2,2))
plot(model)
summary(model)

 ######################################## 
 #Un usuario en concreto
 
 # Filtrar datos para un único usuario
 user_data <- subset(summary_minutos, user == "user_10")

 # Extraer los valores de frecuencia cardiaca
 frecuencia_cardiaca <- user_data$HR
 
 # Aplicar la Transformada Rápida de Fourier (FFT)
 fft_result <- fft(frecuencia_cardiaca)
 
 # Calcular la potencia de cada armónica
 power_spectrum <- Mod(fft_result)^2
 power_spectrum <- power_spectrum / sum(power_spectrum)
 
 # Calcular el Índice de Circadianidad (CI)
 num_harmonics <- 24
 num_harmonics <- min(num_harmonics, length(power_spectrum) / 2)
 P1 <- power_spectrum[2]
 P_sum <- sum(power_spectrum[2:(num_harmonics + 1)])
 CI <- P1 / P_sum
 
 # Mostrar el resultado
 print(CI)
 
 ########################################
 #Graficos
 par(mfrow=c(2,2))
 
 # Filtrar los datos para un único usuario
 user_data <- subset(summary_15min, user == "user_10")

 # Extraer los valores de frecuencia cardiaca
 frecuencia_cardiaca <- user_data$HR
 
 # Graficar la serie temporal original
 plot(user_data$intervalo_15min, user_data$HR, type = 'l',
      xlab = 'Tiempo', ylab = 'Frecuencia Cardiaca',
      main = 'Serie Temporal de Frecuencia Cardiaca')
 
 # Aplicar FFT y calcular el espectro de potencia
 fft_result <- fft(frecuencia_cardiaca)
 magnitude_spectrum <- Mod(fft_result)
 power_spectrum <- magnitude_spectrum^2
 normalized_power_spectrum <- power_spectrum / sum(power_spectrum)
 
 # Graficar el espectro de potencia
 freqs <- seq(0, length(normalized_power_spectrum) - 1) / length(normalized_power_spectrum)
 plot(freqs[1:(length(freqs)/2)], normalized_power_spectrum[1:(length(normalized_power_spectrum)/2)],
      type = 'h', lwd = 2, col = 'blue',
      xlab = 'Frecuencia', ylab = 'Potencia Normalizada',
      main = 'Espectro de Potencia')
 
 # Número de armónicas a considerar
 num_harmonics <- min(12, length(normalized_power_spectrum) / 2)
 harmonic_indices <- 2:(num_harmonics + 1)
 harmonic_powers <- normalized_power_spectrum[harmonic_indices]
 
 # Graficar las potencias de las primeras armónicas
 barplot(harmonic_powers, names.arg = harmonic_indices - 1,
         xlab = 'Número de Armónica', ylab = 'Potencia Normalizada',
         main = 'Potencias de las Primeras Armónicas',
         col = 'lightblue', border = 'blue')
 
 # Graficar la magnitud del FFT
 plot(freqs[1:(length(freqs)/2)], magnitude_spectrum[1:(length(magnitude_spectrum)/2)],
      type = 'h', lwd = 2, col = 'red',
      xlab = 'Frecuencia', ylab = 'Magnitud',
      main = 'Magnitud del FFT')

 
 
 
############################################
# Relación entre melatonina y RA y AA
 
 ggplot(df_numeric2, aes(x = Melatonin_normalizado, y = AA)) +
   geom_point(size = 3) +  # Añadir puntos
   geom_smooth(method = "lm", color = "blue", size = 1.5) +  # Añadir línea de regresión
   labs(title = "Relación entre Melatonina normalizada y AA", 
        x = "Melatonina normalizada", 
        y = "AA") +
   theme_minimal() +
   theme(
     axis.title.x = element_text(size = 14),  # Tamaño del texto del eje x
     axis.title.y = element_text(size = 14),  # Tamaño del texto del eje y
     axis.text.x = element_text(size = 12),   # Tamaño del texto de las etiquetas del eje x
     axis.text.y = element_text(size = 12)    # Tamaño del texto de las etiquetas del eje y
   )
 
 ggplot(df_numeric2, aes(x = Melatonin_normalizado, y = RA)) +
   geom_point(size = 3) +  # Añadir puntos
   geom_smooth(method = "lm", color = "blue", size = 1.5) +  # Añadir línea de regresión
   labs(title = "Relación entre Melatonina Normalizada y RA", 
        x = "Melatonina Normalizada", 
        y = "RA") +
   theme_minimal() +
   theme(
     axis.title.x = element_text(size = 14),  # Tamaño del texto del eje x
     axis.title.y = element_text(size = 14),  # Tamaño del texto del eje y
     axis.text.x = element_text(size = 12),   # Tamaño del texto de las etiquetas del eje x
     axis.text.y = element_text(size = 12)    # Tamaño del texto de las etiquetas del eje y
   )
 
 
 
 
