# Instalar paquetes necesarios
install.packages("DBI")
install.packages("RMySQL")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")

# Cargar librerías
library(DBI)
library(RMySQL)
library(ggplot2)
library(dplyr)
library(forecast)
library(leaflet)
library(lubridate)

# Cargar funciones externas
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

# Análisis Temporal: ventas diarias
# Extraer datos de ventas diarias
ventas_diarias <- dbGetQuery(con, 
                             "SELECT o.order_date, SUM(s.total_revenue) AS daily_revenue 
                             FROM sales s
                             INNER JOIN orders o ON o.id = s.order_id
                             GROUP BY o.order_date")

ventas_diarias$order_date <- as.Date(ventas_diarias$order_date)

analisis_complejo(sales_data, orders_data, items_data, countries_data, 100, "total_revenue")

# Crear una serie temporal
ts_data <- ts(ventas_diarias$daily_revenue, start = c(2020, 1), frequency = 365)

# Visualizar la serie temporal
autoplot(ts_data) + labs(title = "Ingresos Diarios", x = "Fecha", y = "Ingresos Diarios")

# Ajustar un modelo ARIMA
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

# Crear un gráfico de líneas de ingresos totales por fecha de pedido (SOLUCIÓN)
sales_data$order_date <- as.Date(sales_data$order_date)
sales_data <- sales_data %>%
  mutate(year = year(order_date), month = floor_date(order_date, "month"))

monthly_data <- sales_data %>%
  group_by(month) %>%
  summarise(total_revenue = sum(total_revenue))

yearly_data <- sales_data %>%
  group_by(year) %>%
  summarise(total_revenue = sum(total_revenue))

# Consulta con INNER JOIN
query_inner_join <- "
SELECT sales.*, orders.order_date, countries.name AS country_name, items.item_type, regions.name AS region_name
FROM sales
INNER JOIN orders ON sales.order_id = orders.id
INNER JOIN countries ON sales.country_id = countries.id
INNER JOIN items ON sales.item_id = items.id
INNER JOIN regions ON countries.region_id = regions.id;
"
data_inner_join <- extraer_datos(con, query_inner_join)
data_inner_join$order_date <- as.Date(data_inner_join$order_date)
monthly_data_inner <- data_inner_join %>%
  mutate(month = floor_date(order_date, "month")) %>%
  group_by(month) %>%
  summarise(total_units_sold = sum(units_sold, na.rm = TRUE), 
            total_cost = sum(total_cost, na.rm = TRUE))

# Realizar INNER JOIN
data_inner_join <- sales_data %>%
  inner_join(orders_data, by = c("order_id" = "id")) %>%
  inner_join(countries_data, by = c("country_id" = "id")) %>%
  inner_join(items_data, by = c("item_id" = "id")) %>%
  inner_join(regions_data, by = c("region_id" = "id"))
data_inner_join$order_date <- as.Date(data_inner_join$order_date)
monthly_data_inner <- data_inner_join %>%
  mutate(month = floor_date(order_date, "month")) %>%
  group_by(month) %>%
  summarise(total_units_sold = sum(units_sold, na.rm = TRUE), 
            total_cost = sum(total_cost, na.rm = TRUE))

# Crear gráfico de líneas de unidades vendidas totales por mes usando INNER JOIN
crear_grafico(monthly_data_inner, "lineas", "month", "total_units_sold")

# Crear gráfico de líneas de costo total por mes usando INNER JOIN
crear_grafico(monthly_data_inner, "barras", "month", "total_cost")

# Realizar LEFT JOIN
data_left_join <- sales_data %>%
  left_join(orders_data, by = c("order_id" = "id")) %>%
  left_join(countries_data, by = c("country_id" = "id")) %>%
  left_join(items_data, by = c("item_id" = "id")) %>%
  left_join(regions_data, by = c("region_id" = "id"))
data_left_join$order_date <- as.Date(data_left_join$order_date)
annual_data_left <- data_left_join %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarise(total_units_sold = sum(units_sold, na.rm = TRUE), 
            total_cost = sum(total_cost, na.rm = TRUE))

# Crear gráfico de líneas de unidades vendidas totales por año usando LEFT JOIN
crear_grafico(annual_data_left, "lineas", "year", "total_units_sold")

# Crear gráfico de líneas de costo total por año usando LEFT JOIN
crear_grafico(annual_data_left, "lineas", "year", "total_cost")

# Realizar RIGHT JOIN
data_right_join <- sales_data %>%
  right_join(orders_data, by = c("order_id" = "id")) %>%
  right_join(countries_data, by = c("country_id" = "id")) %>%
  right_join(items_data, by = c("item_id" = "id")) %>%
  right_join(regions_data, by = c("region_id" = "id"))
data_right_join$order_date <- as.Date(data_right_join$order_date)
annual_data_right <- data_right_join %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarise(total_units_sold = sum(units_sold, na.rm = TRUE), 
            total_cost = sum(total_cost, na.rm = TRUE))

# Crear gráfico de líneas de unidades vendidas totales por año usando RIGHT JOIN
crear_grafico(annual_data_right, "lineas", "year", "total_units_sold")

# Crear gráfico de líneas de costo total por año usando RIGHT JOIN
crear_grafico(annual_data_right, "lineas", "year", "total_cost")

# Crear un gráfico de líneas de ingresos totales por mes
crear_grafico(monthly_data, "lineas", "month", "total_revenue")

# Crear un gráfico de líneas de ingresos totales por año
crear_grafico(yearly_data, "lineas", "year", "total_revenue")

# Cerrar la conexión a la base de datos
dbDisconnect(con)
