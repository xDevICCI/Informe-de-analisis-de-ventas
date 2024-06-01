install.packages("DBI")
install.packages("RMySQL")
install.packages("ggplot2")
install.packages("dplyr")

library(DBI)
library(RMySQL)
library(ggplot2)
library(dplyr)


# Conectar a la base de datos MySQL
con <- conectar_db("sales", "127.0.0.1", 3306, "root", "", "/var/run/mysqld/mysqld.sock")


# Extraer datos de la tabla `sales`
sales_data <- dbGetQuery(con, "SELECT * FROM sales")

# Extraer datos de la tabla `countries` para obtener nombres de países
countries_data <- dbGetQuery(con, "SELECT * FROM countries")

# Unir los datos de ventas con los nombres de países
sales_data <- sales_data %>% 
  left_join(countries_data, by = c("country_id" = "id"))

# Crear tabla resumen
resumen <- crear_tabla_resumen(sales_data)
resumen

# Crear gráficos
crear_grafico(sales_data, "barras", "name", "total_revenue")
crear_grafico(sales_data, "dispersión", "unit_price", "units_sold")
crear_grafico(sales_data, "histograma", "total_profit", binwidth = 10000)




# Cerrar la conexión a la base de datos
dbDisconnect(con)
