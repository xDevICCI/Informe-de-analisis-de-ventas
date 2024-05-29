# importación de librerias
library(tidyverse)
library(ggplot2)

source("./global/funciones_eda/funciones_eda.R")
source("./global/funciones_graficos/funciones_graficos.R")
source("./global/funciones_limpieza/limpieza.R")

#0 Definición del problema/objetivo de investigación (ficticia).

# 0.1 La empresa "IquiqueMiami LTDA." ha observado fluctuaciones significativas en sus ingresos durante 
#     el último año fiscal. A pesar de tener una amplia variedad de productos y presencia en varias estados 
#     del país de Estados Unidos, la empresa ha enfrentado desafíos debido a la alta competitividad en el
#     mercado y cambios en las preferencias de los consumidores. La dirección ha identificado que una 
#     estrategia de precios ineficaz podría ser una de las causas principales de estos resultados 
#     inconsistentes.


# 1. Introducción: Descripción del conjunto de datos seleccionado y los objetivos del análisis (ficticia).

#     En los últimos años, el movimiento económico de empresas pequeñas enfrenta desafíos constantes 
#     debido a la competencia, las cambiantes preferencias de los consumidores o clientes, y la rápida 
#     evaluación de la tecnología. “Iquique Miami LTDA.”, una empresa en el sector minorista, se ha 
#     comprometido a mejorar sus ventas y la satisfacción del cliente mediante la implementación de un 
#     enfoque basado en datos.


# 1.1. Diccionario de datos

#       Area Code: Store's Code;
#       State: Store's State;
#       Market: Store's Region;
#       Market Size: Store's Size;
#       Profit: Profits in Dollars ($);
#       Margin: Profit + Total Expenses ($) OR Sales - COGS ($);
#       Sales: Values Acquired in Sales ($);
#       COGS: Cost of Goods Sold ($);
#       Total Expenses: Total Expenses to get the Product to Sell ($);
#       Marketing: Expenses in Marketing ($);
#       Inventory: Inventory Value of the Product in the Sale Moment ($);
#       Budget Profit: Expected Profit ($);
#       Budget COGS: Expected COGS ($);
#       Budget Margin: Expected Profit + Expected Total Expenses ($) OR Expected Sales - Expected COGS ($);
#       Budget Sales: Expected Value Acquired in Sales ($);
#       ProductID: Product ID;
#       Date: Sale Date;
#       Product Type: Product Category;
#       Product: Product Description;
#       Type: Type;



#####1.2. DATOS CUANTITATIVOS

####
#         Area code: Entero
#         Profit: Entero
#         Margin: Entero
#         Sales: Entero
#         COGS: Entero
#         Total Expenses: Entero
#         Marketing: Entero
#         Inventory: Entero
#         Budget Profit: Entero
#         Budget COGS: Entero
#         Budget Margin: Entero
#         Budget Sales: Entero
#         ProductId: Entero
###


#####1.3. DATOS CUALITATIVOS

####
#State: Cadena
#Market: Cadena
#Market Size: Cadena
#Product Type: Cadena
#Product: Cadena
####



# 2. Preprocesamiento de Datos: Descripción detallada de los pasos tomados para limpiar y preparar los datos para el análisis utilizando Tidyverse.
datos <- read_delim("./data/sales.csv", delim = ",") # Usar read_delim con el delimitador especificado

# Quitar espacios de los nombres de las columnas (reemplazar los pacios por guion bajo)
datos <- quitar_espacios_nombres(datos)


# 2.1. Base de datos reducida
datos_reducidos <- base_datos_reducida(datos, "Date", c("State", "Market", "Market_Size"))
print(datos_reducidos)

# Inspeccionamos las primeras filas de los datos
head(datos_reducidos)


# Opcion 1: Resumen de estructura de datos (imprime algunos datos y tipos de datos)
glimpse(datos_reducidos)

# Opcion 2: Inspeccionar (imprime la estructura y los tipos de datos)
str(datos_reducidos)

# Opcion 3: Tambien en forma opcional y para mejorar visualización de los datos anteriores, se hace uso de la funcion validar y verificar
# 1. Validar y verificar los datos
validar_datos(datos_reducidos)

# 3. Análisis Exploratorio de Datos: Implementación de códigos en R para explorar los datos. Esto puede incluir visualizaciones de datos, medidas de tendencia central, etc. importación de datos

# 3.1. Resumen numérico de las variables Sales y Profit
resumen <- resumen_numerico(datos_reducidos)
print(resumen)


# 3.1. Estadísticas descriptivas por grupo (State)
estadisticas <- estadisticas_por_grupo(datos, "State")
print(estadisticas)


# 3.3. resumen general/total estadísticos de todos los datos presentes en el dataset
summary(datos_reducidos)




# 3.3. Comprobar si hay valores faltantes en las columnas importantes
na_count <- sapply(datos_reducidos, function(x) sum(is.na(x)))
na_count

# 3.4. Comprobar si hay valores faltantes en las columnas importantes seleccionadas que exige el informe.
sum(is.na(datos_reducidos))
sum(is.na(datos_reducidos$Profit))
sum(is.na(datos_reducidos$Sales))
sum(is.na(datos_reducidos$COGS))

# 3.5. Ya realizado la comprobación de las variables importanes seleccionadas, tambien se hace uso de función para tomar todas las variables y limpiar NA del objeto DF

datos_limpios <- limpiar_datos_nulos(datos)
print(datos_limpios)

# 3.4. Histograma de Profit
crear_grafico(data = datos_limpios, tipo = "histograma", x_var = "Profit", title = "Histograma de Profit", x_label = "Profit", y_label = "Frecuencia", bins = 30)

# 3.5. Boxplot de Sales
crear_grafico(data = datos_limpios, tipo = "boxplot", y_var = "Sales", title = "Boxplot de Sales", y_label = "Sales")

# 3.6. Boxplot de Total Expenses por Market
crear_grafico(data = datos_limpios, tipo = "boxplot", x_var = "Market", y_var = "Total_Expenses", title = "Boxplot de Total Expenses por Market", x_label = "Market", y_label = "Total Expenses")
