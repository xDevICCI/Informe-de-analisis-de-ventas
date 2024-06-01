# importación de librerias
library(tidyverse)
library(ggplot2)

source("./global/funciones_eda/funciones_eda.R")
source("./global/funciones_graficos/funciones_graficos.R")
source("./global/funciones_limpieza/limpieza.R")



# 2. Preprocesamiento de Datos: Descripción detallada de los pasos tomados para limpiar y preparar los datos para el análisis utilizando Tidyverse.
datos <- read_csv("./data/5m_sales.csv") # Usar read_delim con el delimitador especificado

#mostrar datos previos del dataset
head(datos)

#ver estructura de los datos (tipo de variables)
str(datos)

# Quitar espacios de los nombres de las columnas (reemplazar los pacios por guion bajo)
datos <- quitar_espacios_nombres(datos)
head(datos)
str(datos)

# 2.1. Base de datos reducida

#se debe validar si existen valores nulos o vacíos
validar_datos(datos)

# Inspeccionamos las primeras filas de los datos
head(datos)


# Opcion 1: Resumen de estructura de datos (imprime algunos datos y tipos de datos)
glimpse(datos)

# Opcion 2: Inspeccionar (imprime la estructura y los tipos de datos)
str(datos)

# Opcion 3: Tambien en forma opcional y para mejorar visualización de los datos anteriores, se hace uso de la funcion validar y verificar
# 1. Validar y verificar los datos

# 3. Análisis Exploratorio de Datos: Implementación de códigos en R para explorar los datos. Esto puede incluir visualizaciones de datos, medidas de tendencia central, etc. importación de datos

# 3.1. Resumen numérico de las variables Sales y Profit
resumen <- resumen_numerico(datos)
print(resumen)


# 3.1. Estadísticas descriptivas por grupo (State)
estadisticas <- estadisticas_por_grupo(datos, "Region")
print(estadisticas)


# 3.3. resumen general/total estadísticos de todos los datos presentes en el dataset
summary(datos)




# 3.3. Comprobar si hay valores faltantes en las columnas importantes
na_count <- sapply(datos, function(x) sum(is.na(x)))
na_count

# 3.4. Comprobar si hay valores faltantes en las columnas importantes seleccionadas que exige el informe.
sum(is.na(datos))
sum(is.na(datos$Units_Sold))
sum(is.na(datos$Unit_Price))
sum(is.na(datos$Total_Profit))

# 3.5. Ya realizado la comprobación de las variables importanes seleccionadas, tambien se hace uso de función para tomar todas las variables y limpiar NA del objeto DF
datos_limpios <- limpiar_datos_nulos(datos)
print(datos_limpios)

# 3.4. Histograma de Profit
crear_grafico(data = datos_limpios, tipo = "histograma", x_var = "Total_Profit", title = "Histograma de Profit", x_label = "Profit", y_label = "Frecuencia", bins = 30)

# 3.5. Boxplot de Cost
crear_grafico(data = datos_limpios, tipo = "boxplot", y_var = "Total_Cost", title = "Boxplot de Sales", y_label = "Sales")

# 3.6. Boxplot de Total Pr por Market
crear_grafico(data = datos_limpios, tipo = "boxplot", x_var = "Region", y_var = "Total_Revenue", title = "Boxplot de Total Expenses por Market", x_label = "Market", y_label = "Total Expenses")
