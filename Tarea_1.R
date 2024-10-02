
# Cargar los datos
datos <- read.csv("dynamic_pricing.csv")

# Ver la estructura del dataframe
str(datos)

# Desviación estándar de todas las columnas numéricas
sapply(datos, var, na.rm = TRUE)
# sapply(datos, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA)

# ***** Análisis Unidimensional ***********
# ***** Ejercicio 1
# Crear diccionario del dataframe
diccionario <- data.frame(
  Nombre_variable = names(datos), # Obtener los nombres de las columnas
    Descripcion_variable = c("Indica el número de pasajeros",
                               "Indica el número de conductores",
                               "Indica categoría de ubicación",
                               "Indica estado de lealtad del cliente",
                               "Indica número de viajes anteriores",
                               "Indica calificaciones promedio",
                               "Indica hora de reserva",
                               "Indica tipo de vehículo",
                               "Indica duración esperada del viaje",
                               "Indica costo histórico de los viajes"),
  Clasificacion_variable = sapply(datos, class), # Obtener el tipo de la variables
  row.names = NULL # Evitar que se ponga una columna adicional con los nombres
)

# ****** Ejercicio 2
# ****** Tablas de distribución de frecuencias variables categóricas
tb_location_category <- table(datos$Location_Category)
tb_Customer_Loyalty_status <- table(datos$Customer_Loyalty_Status)
tb_Time_of_Booking <- table(datos$Time_of_Booking)
tb_hehicle_type <- table(datos$Vehicle_Type)

# ******* Gráficas de distribución de frecuencias
# variable Location_Category
ggplot(datos, aes(x = Location_Category))+
  geom_bar(fill = "skyblue")+
  ggtitle("Distribución de variable Location_Category")+
  xlab("Location_Category")+
  ylab("Frecuencia")

# Variable Customer_Loyalty_Status
ggplot(datos, aes(x = Customer_Loyalty_Status))+
  geom_bar(fill = "lightgreen")+
  ggtitle("Distribución de variable Customer_Loyalty_Status")+
  xlab("Customer_Loyalty_Status")+
  ylab("Frecuencia")

# Variable Time_of_Booking
ggplot(datos, aes(x = Time_of_Booking))+
  geom_bar(fill = "orange")+
  ggtitle("Distribución de variable Time_of_Booking")+
  xlab("Time_of_Booking")+
  ylab("Frecuencia")

# Variable Vehicle_Type
ggplot(datos, aes(x = Vehicle_Type))+
  geom_bar(fill = "blue")+
  ggtitle("Distribución de variable Vehicle_Type")+
  xlab("Vehicle_Type")+
  ylab("Frecuencia")

# ******* Ejercicio 3
# ********* Medidas resumen variables numéricas
# Calcular las medidas de resumen para las variables numéricas
riders <- summary(datos$Number_of_Riders)
ratings <- summary(datos$Average_Ratings)
duration <- summary(datos$Expected_Ride_Duration)
cost <- summary(datos$Historical_Cost_of_Ride)

# ***** Ejercicio 4
# ******** Gráficas de distribución de las variables (Boxplot)
# Variable Number_of_Riders
ggplot(datos, aes(x = "", y = Number_of_Riders))+
  geom_boxplot(fill = "lightblue")+
  ggtitle("Distribución de variable NUmber_of_Riders")+
  ylab("Number_of_Riders")

# Variable Average_Ratings
ggplot(datos, aes(x = "", y = Average_Ratings)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplot de Average Ratings") +
  ylab("Average_Ratings")

# Variable Expected_Ride_Duration
ggplot(datos, aes(x = "", y = Expected_Ride_Duration)) +
  geom_boxplot(fill = "salmon") +
  ggtitle("Boxplot de Expected Ride Duration") +
  ylab("Expected_Ride_Duration")

# Variable Historical_Cost_of_Rid
ggplot(datos, aes(x = "", y = Historical_Cost_of_Ride)) +
  geom_boxplot(fill = "purple") +
  ggtitle("Boxplot de Historical Cost of Ride") +
  ylab("Historical_Cost_of_Rid")


# ********* Análisis Bidimensional e Inferencial.
# ***** Ejercicio 5a
# correlación dos a dos entre
# Scatterplot: Número de viajeros vs. Costo histórico
ggplot(datos, aes(x = Number_of_Riders, y = Historical_Cost_of_Ride)) +
  geom_point(color = "blue") +
  ggtitle("Scatterplot: Number_of_Riders vs. Historical_Cost_of_Ride") +
  xlab("Number_of_Riders") +
  ylab("Historical_Cost_of_Ride")

# Scatterplot: Average_Ratings vs. Historical_Cost_of_Ride
ggplot(datos, aes(x = Average_Ratings, y = Historical_Cost_of_Ride)) +
  geom_point(color = "green") +
  ggtitle("Scatterplot: Average_Ratings vs. Historical_Cost_of_Ride") +
  xlab("Average_Ratings") +
  ylab("Historical_Cost_of_Ride")

# Scatterplot: Expected_Ride_Duration vs. Historical_Cost_of_Ride
ggplot(datos, aes(x = Expected_Ride_Duration, y = Historical_Cost_of_Ride)) +
  geom_point(color = "orange") +
  ggtitle("Scatterplot: Expected_Ride_Duration vs. Historical_Cost_of_Ride") +
  xlab("Expected_Ride_Duration (min)") +
  ylab("Historical_Cost_of_Ride")

# ****** Ejercicio 5b
# ****** Calcular la correlacion de pearson
# obtener las variables numéricas
var_num <- datos %>%
  select_if(is.numeric)

# Aplicar la correlación de Pearson a las variables numéricas 
tb_correlacion <- cor(var_num, method = 'pearson')

# ****** Calcular la correlacion de pearson entre:
# Correlación Número de viajeros vs Costo histórico
cor_riders_cost <- cor(datos$Number_of_Riders, 
                       datos$Historical_Cost_of_Ride, 
                       method = "pearson")

cor_test_riders_cost <- cor.test(datos$Number_of_Riders, 
                                 datos$Historical_Cost_of_Ride,
                                 method = "pearson")

# Correlación Calificación vs Costo histórico
cor_ratings_cost <- cor(datos$Average_Ratings, 
                        datos$Historical_Cost_of_Ride, 
                        method = "pearson")

cor_test_ratings_cost <- cor.test(datos$Average_Ratings, 
                                  datos$Historical_Cost_of_Ride,
                                  method = "pearson")

# Correlación Duración esperada vs Costo histórico
cor_duration_cost <- cor(datos$Expected_Ride_Duration, 
                         datos$Historical_Cost_of_Ride, 
                         method = "pearson")

cor_test_duration_cost <- cor.test(datos$Expected_Ride_Duration, 
                                   datos$Historical_Cost_of_Ride,
                                   method = "pearson")

# Tabla de Resultados del test
resultados_test <- data.frame(
  Variables_evaluada = c("número de viajeros vs costo histórico", 
                         "calificación vs costo histórico", 
                         "expectativa de duración del viaje vs costo histórico"),
  coef_correlacion = c(cor_riders_cost,
                       cor_ratings_cost, 
                       cor_duration_cost
                       ),
  p_valor = c(cor_test_riders_cost$p.value, 
               cor_test_ratings_cost$p.value, 
               cor_test_duration_cost$p.value)
)

# ***** Ejercicio 6a 
# Tabla de contingencia 1: Customer Loyalty Status vs Time of Booking
tabla_fidelidad_reserva <- table(datos$Customer_Loyalty_Status, datos$Time_of_Booking)
tabla_fidelidad_reserva

# Tabla de contingencia 2: Customer Loyalty Status vs Vehicle Type
tabla_fidelidad_vehiculo <- table(datos$Customer_Loyalty_Status, datos$Vehicle_Type)
tabla_fidelidad_vehiculo

# Tabla de contingencia 3: Time of Booking vs Vehicle Type
tabla_reserva_vehiculo <- table(datos$Time_of_Booking, datos$Vehicle_Type)
tabla_reserva_vehiculo


# ***** Ejercicio 6b
# Prueba Chi-Cuadrado: Customer Loyalty Status vs Time of Booking
chi_fidelidad_reserva <- chisq.test(tabla_fidelidad_reserva)

# Prueba Chi-Cuadrado: Customer Loyalty Status vs Vehicle Type
chi_fidelidad_vehiculo <- chisq.test(tabla_fidelidad_vehiculo)

# Prueba Chi-Cuadrado: Time of Booking vs Vehicle Type
chi_reserva_vehiculo <- chisq.test(tabla_reserva_vehiculo)

# Crear tabla con el resultado de test chi-squared
resultados_chi <- data.frame(
  Tabla = c("tabla_fidelidad_reserva",
            "tabla_fidelidad_vehiculo",
            "tabla_reserva_vehiculo"),
  p_value = c(chi_fidelidad_reserva$p.value,
                 chi_fidelidad_vehiculo$p.value,
                 chi_reserva_vehiculo$p.value),
  chi_squared = c(chi_fidelidad_reserva$statistic,
              chi_fidelidad_vehiculo$statistic,
              chi_reserva_vehiculo$statistic),
  freedom_degrees = c(chi_fidelidad_reserva$parameter,
                      chi_fidelidad_vehiculo$parameter,
                      chi_reserva_vehiculo$parameter)
)

# ****** Ejercicio 7a
# graficos de boxplot
# Boxplot: Número de Viajeros vs Estado de Fidelidad
ggplot(datos, aes(x = Customer_Loyalty_Status, y = Number_of_Riders)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Número de Viajeros según el Estado de Fidelidad", 
       x = "Estado de Fidelidad", 
       y = "Número de Viajeros")

# Boxplot: Calificación vs Estado de Fidelidad
ggplot(datos, aes(x = Customer_Loyalty_Status, y = Average_Ratings)) +
  geom_boxplot(fill = "coral") +
  labs(title = "Calificación según el Estado de Fidelidad", 
       x = "Estado de Fidelidad", 
       y = "Calificación Promedio")

# Boxplot: Expectativa de Duración del Viaje vs Estado de Fidelidad
ggplot(datos, aes(x = Customer_Loyalty_Status, y = Expected_Ride_Duration)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Expectativa de Duración del Viaje según el Estado de Fidelidad", 
       x = "Estado de Fidelidad", 
       y = "Duración Esperada del Viaje (min)")

# ******* Ejercicio 7b

# ANOVA 1: Número de Viajeros vs Estado de Fidelidad
# Para esta prueba se toma la variable numérica com dependiente y el factor como independiente
anova_riders <- aov(Number_of_Riders ~ Customer_Loyalty_Status, 
                    data = datos)

summary(anova_riders)

# ----------------------------------------------------------------------------- 
#El mismo proceso pero convirtiendo a factor la variable Customer_Loyalty_Status, aunque esto lo hace la función aov
datos_aov <- datos %>%
  mutate(Customer_Loyalty_Status = as.factor(Customer_Loyalty_Status))

class(datos_aov$Customer_Loyalty_Status)

anova_riders_2 <- aov(Number_of_Riders ~ Customer_Loyalty_Status, 
                    data = datos_aov)

summary(anova_riders_2)
# ----
# ANOVA 2: Calificación vs Estado de Fidelidad
anova_ratings <- aov(Average_Ratings ~ Customer_Loyalty_Status, 
                     data = datos)
summary(anova_ratings)

# ANOVA 3: Expectativa de Duración del Viaje vs Estado de Fidelidad
anova_duration <- aov(Expected_Ride_Duration ~ Customer_Loyalty_Status, 
                      data = datos)
summary(anova_duration)


# Obtener el p-value de cada prueba ANOVA
p_value_annova_riders <- summary(anova_riders) 
p_value_riders <- p_value_annova_riders[[1]][["Pr(>F)"]][1]

p_value_annova_ratings <- summary(anova_ratings) 
p_value_ratings <- p_value_annova_ratings[[1]][["Pr(>F)"]][1]

p_value_annova_durations <- summary(anova_duration) 
p_value_duration <- p_value_annova_durations[[1]][["Pr(>F)"]][1]


# Crear tabla con el resultado de ANOVA
resultados_anova <- data.frame(
  Relacion = c("Number_of_Riders ~ Customer_Loyalty_Status",
            "Average_Ratings ~ Customer_Loyalty_Status",
            "Expected_Ride_Duration ~ Customer_Loyalty_Status"),
  p_value = c(p_value_riders,
              p_value_ratings,
              p_value_duration)
)







