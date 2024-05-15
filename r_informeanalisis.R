# importación de librerias
library(tidyverse)
library(ggplot2)

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
datos <- read_csv("sales_oficial.csv")

# Inspeccionamos las primeras filas de los datos
head(datos)


# Resumen de estructura de datos (imprime algunos datos y tipos de datos)
glimpse(datos)

# Inspeccionar y limpiar los datos si es necesario (imprime la estructura y los tipos de datos)
str(datos)

# 3. Análisis Exploratorio de Datos: Implementación de códigos en R para explorar los datos. Esto puede incluir visualizaciones de datos, medidas de tendencia central, etc. importación de datos

# 3.1. resumen general/total estadísticos de todos los datos presentes en el dataset
summary(datos)


# 3.2. Comprobar si hay valores faltantes en las columnas importantes
na_count <- sapply(datos, function(x) sum(is.na(x)))
na_count

# 3.3. Comprobar si hay valores faltantes en las columnas importantes seleccionadas que exige el informe.
sum(is.na(datos))
sum(is.na(datos$Profit))
sum(is.na(datos$Sales))
sum(is.na(datos$COGS))


# 3.4. Histograma de Profit
ggplot(datos, aes(x = Profit)) +
  geom_histogram(bins = 30, fill = "blue") +
  ggtitle("Histograma de Profit")

# 3.5. Boxplot de Sales
ggplot(datos, aes(y = Sales)) +
  geom_boxplot(fill = "green") +
  ggtitle("Boxplot de Sales")

# 4 Preprocesamiento de Datos (formateo de solamente fecha, ya que la fecha incluye horas, minutos y segundos)
datos <- datos %>%
  mutate(fecha = as.Date(fecha, format = "%d-%m-%Y")) %>% # Convertir la columna 'fecha' al formato de fecha (no incluye hh:mm:ss)
  filter(!is.na(precio)) %>% # Eliminar filas donde el precio es NA
  arrange(desc(precio)) # Ordenar los datos por precio de forma descendente



# 4. Modelado de Datos: Aplicación de técnicas de modelado de datos y algoritmos de aprendizaje automático, si corresponde.





#5. Interpretación de Resultados: Discusión de los resultados del análisis y su relevancia para los objetivos establecidos en la introducción.





