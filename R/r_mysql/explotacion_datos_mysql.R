install.packages("DBI")
install.packages("RMySQL")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")

library(DBI)
library(RMySQL)
library(ggplot2)
library(dplyr)
library(forecast)
library(leaflet)
library(lubridate)

source("./global/funciones_mysql/funciones_r_mysql.R")



# Conectar a la base de datos MySQL
con <- conectar_db("sales", "127.0.0.1", 3306, "root", "", "/var/run/mysqld/mysqld.sock")


# Extraer datos de las tablas
sales_data <- extraer_datos(con, "SELECT * FROM sales")
countries_data <- extraer_datos(con, "SELECT * FROM countries")
regions_data <- extraer_datos(con, "SELECT * FROM regions")
items_data <- extraer_datos(con, "SELECT * FROM items")
orders_data <- extraer_datos(con, "SELECT * FROM orders")


# Unir los datos de ventas con los nombres de países
sales_data <- sales_data %>% 
  left_join(countries_data, by = c("country_id" = "id"))

# Unir los datos de ventas con todas las tablas relacionadas
sales_data <- procesar_datos(sales_data, countries_data, regions_data, items_data, orders_data)


# Crear tabla resumen
resumen <- crear_tabla_resumen(sales_data)
resumen


#Análisis Temporal:  ventas diarias
# Extraer datos de ventas diarias
ventas_diarias <- dbGetQuery(con, 
                            "SELECT o.order_date, SUM(s.total_revenue) AS daily_revenue 
                             FROM sales s
                             inner join orders o ON
                             o.id = s.order_id
                             GROUP BY o.order_date")


ventas_diarias$order_date <- as.Date(ventas_diarias$order_date)

# Crear una serie temporal
ts_data <- ts(ventas_diarias$daily_revenue, start = c(2020, 1), frequency = 365)

# Visualizar la serie temporal
autoplot(ts_data) + labs(title = "Ingresos Diarios", x = "Fecha", y = "Ingresos Diarios")

# Ajustar un modelo ARIMA
# El modelo ARIMA (AutoRegressive Integrated Moving Average) es una técnica estadística utilizada para analizar 
# y predecir series temporales. Es uno de los métodos más populares y ampliamente utilizados en el análisis de 
# series temporales debido a su flexibilidad y capacidad para manejar una amplia gama de patrones de datos
fit <- auto.arima(ts_data)
forecasted <- forecast(fit, h = 30)

# Visualizar el pronóstico
autoplot(forecasted) + labs(title = "Pronóstico de Ingresos Diarios", x = "Fecha", y = "Ingresos Diarios")




# Crear gráficos
crear_grafico(sales_data, "barras", "total_cost", "total_revenue")
crear_grafico(sales_data, "dispersión", "unit_price", "units_sold")
crear_grafico(sales_data, "histograma", "total_profit", binwidth = 10000)

# Crear un gráfico de líneas de ingresos totales por fecha de pedido (SE VE HORRIBLE CON FORMATO DATE)
crear_grafico(sales_data, "lineas", "order_date", "total_revenue")

#Crear un gráfico de lineas Crear un gráfico de líneas de ingresos totales por fecha de pedido (SOLUCIÓN)
# Convertir order_date a Date
sales_data$order_date <- as.Date(sales_data$order_date)

# Agregar columnas de año y mes
sales_data <- sales_data %>%mutate(year = year(order_date),month = floor_date(order_date, "month"))

# Agrupar por mes y sumar los ingresos totales
monthly_data <- sales_data %>%
  group_by(month) %>%
  summarise(total_revenue = sum(total_revenue))


# Agrupar por año y sumar los ingresos totales
yearly_data <- sales_data %>%
  group_by(year) %>%
  summarise(total_revenue = sum(total_revenue))


# Crear un gráfico de líneas de ingresos totales por mes
crear_grafico(monthly_data, "lineas", "month", "total_revenue")

# Crear un gráfico de líneas de ingresos totales por año
crear_grafico(yearly_data, "lineas", "year", "total_revenue")


# Cerrar la conexión a la base de datos
dbDisconnect(con)
