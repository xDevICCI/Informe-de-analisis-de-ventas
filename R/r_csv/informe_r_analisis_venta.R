# Importación de librerías
library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
# Visualizar la matriz de correlación usando ggplot2
library(corrplot)
# Cargar funciones personalizadas
source("./global/funciones_r/funciones_eda/funciones_eda.R")
source("./global/funciones_r/funciones_graficos/funciones_grafico.R")
source("./global/funciones_r/funciones_limpieza/limpieza.R")

# Preprocesamiento de Datos
datos <- read_csv("./data/5m_sales.csv")
if (!file.exists("./data/5m_sales.csv")) {
  stop("El archivo no existe.")
}

# Mostrar datos previos del dataset
head(datos)
str(datos)

# Quitar espacios de los nombres de las columnas
datos <- quitar_espacios_nombres(datos)
head(datos)
str(datos)

# Validar si existen valores nulos o vacíos
validar_datos(datos)

# Inspección de datos
glimpse(datos)

skim(datos)

# Resumen numérico de las variables Sales y Profit
summary(datos$Total_Cost)
resumen <- resumen_numerico(datos)
print(resumen)

# Estadísticas descriptivas por grupo (Region)
estadisticas <- estadisticas_por_grupo(datos, "Region")
print(estadisticas)

# Comprobar y limpiar valores faltantes
na_count <- sapply(datos, function(x) sum(is.na(x)))
na_count

# Comprobar valores faltantes en columnas seleccionadas
sum(is.na(datos))
sum(is.na(datos$Units_Sold))
sum(is.na(datos$Unit_Price))
sum(is.na(datos$Total_Profit))

# Limpiar datos nulos
datos_limpios <- limpiar_datos_nulos(datos)
print(datos_limpios)

# Visualizaciones
crear_grafico_dinamico(data = datos_limpios, tipo_grafico = "histograma", x_var = "Total_Profit", title = "Histograma de Profit", x_label = "Profit", y_label = "Frecuencia", bins = 30)
crear_grafico_dinamico(data = datos_limpios, tipo_grafico = "boxplot", x_var = "Total_Cost", title = "Boxplot de Total Cost", x_label = "Total Cost", y_label = "Valores")
crear_grafico_dinamico(data = datos_limpios, tipo_grafico = "boxplot", x_var = "Region", y_var = "Total_Revenue", title = "Boxplot de Total Revenue por Region", x_label = "Region", y_label = "Total Revenue")
crear_grafico_dinamico(data = datos_limpios, tipo_grafico = "line", date_var = "Order_Date", units_var = "Units_Sold", title = "Total Units Sold por Mes", x_label = "Month", y_label = "Total Units Sold")

# Resumen por grupo usando dplyr
resumen_por_region <- datos %>%
  group_by(Region) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd, median = median)))
print(resumen_por_region)


# Calcular la matriz de correlación
correlaciones <- cor(datos %>% select_if(is.numeric))

corrplot(correlaciones, method = "circle")



#Modelado de datos
# Comprobar que no hayan valores NA en las columnas que vamos a usar para modelar
data <- datos %>%
  filter(!is.na(Total_Revenue), !is.na(Units_Sold), !is.na(Unit_Price))

# Regresión Lineal: Total Revenue como función de Units Sold y Unit Price
linear_model <- lm(Total_Revenue ~ Units_Sold + Unit_Price, data = data)
summary(linear_model)
par(mfrow=c(2,2))
plot(linear_model)

# Diagnósticos visuales del modelo
par(mfrow = c(2, 2))  # Configurar el área de la gráfica para mostrar 4 gráficos en una cuadrícula de 2x2

# Gráfico de Residuos vs Valores Ajustados
plot(linear_model$fitted.values, residuals(linear_model),
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Residuos vs Valores Ajustados")

# Gráfico Q-Q de los residuos para verificar la normalidad
qqnorm(residuals(linear_model))
qqline(residuals(linear_model), col = "red")

# Gráfico de Escala-Localización (Scale-Location Plot)
plot(linear_model$fitted.values, sqrt(abs(residuals(linear_model))),
     xlab = "Valores Ajustados", ylab = "Raíz Cuadrada de los Residuos Absolutos",
     main = "Escala-Localización")

# Gráfico de Distancias de Cook para identificar influencias atípicas
plot(linear_model, which = 4, main = "Distancias de Cook")








#REGRESIÓN LOGÍSTICA
library(caret)

# Crear una variable categórica para la regresión logística
median_revenue <- median(data$Total_Revenue, na.rm = TRUE)
data$High_Revenue <- ifelse(data$Total_Revenue >= median_revenue, 1, 0)  # 1 si los ingresos son altos, 0 si son bajos

# Limpiar y preparar los datos
data <- data %>%
  filter(!is.na(Total_Revenue)) %>%
  mutate(
    Order_Priority = as.factor(Order_Priority),
    Sales_Channel = as.factor(Sales_Channel)
  )

# Establecer semilla para reproducibilidad
set.seed(123)

# Dividir los datos en conjuntos de entrenamiento y prueba
training_indices <- createDataPartition(data$High_Revenue, p = 0.8, list = FALSE)
train_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

# Modelo de regresión logística
logit_model <- glm(High_Revenue ~ Units_Sold + Unit_Price + Order_Priority + Sales_Channel, family = binomial(), data = train_data)

# Ver el resumen del modelo
summary(logit_model)

# Predicciones sobre el conjunto de prueba
probabilities <- predict(logit_model, test_data, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)

# Generar matriz de confusión para evaluar el modelo
confusionMatrix(as.factor(predicted_classes), as.factor(test_data$High_Revenue))
