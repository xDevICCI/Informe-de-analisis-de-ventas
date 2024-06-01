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



# Gráfico de barras de ingresos totales por país
ggplot(sales_data, aes(x = name, y = total_revenue)) +
  geom_bar(stat = "identity") +
  labs(title = "Ingresos Totales por País", x = "País", y = "Ingresos Totales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Gráfico de dispersión de precio unitario vs. unidades vendidas
ggplot(sales_data, aes(x = unit_price, y = units_sold)) +
  geom_point() +
  labs(title = "Precio Unitario vs. Unidades Vendidas", x = "Precio Unitario", y = "Unidades Vendidas")

# Histograma de beneficios totales
ggplot(sales_data, aes(x = total_profit)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black") +
  labs(title = "Distribución de Beneficios Totales", x = "Beneficios Totales", y = "Frecuencia")



# Cerrar la conexión a la base de datos
dbDisconnect(con)
