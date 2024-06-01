
library(DBI)
library(RMySQL)
library(ggplot2)
library(dplyr)


conectar_db <- function(dbname, host, port, user, password, unix_socket) {
  con <- dbConnect(RMySQL::MySQL(), 
                   dbname = dbname, 
                   host = host, 
                   port = port, 
                   user = user, 
                   password = password,
                   unix.socket = unix_socket)
  return(con)
}



extraer_datos <- function(con, query) {
  datos <- dbGetQuery(con, query)
  return(datos)
}

procesar_datos <- function(sales_data, countries_data) {
  sales_data <- sales_data %>% 
    left_join(countries_data, by = c("country_id" = "id"))
  return(sales_data)
}


analizar_datos <- function(data) {
  summary_stats <- data %>%
    summarise(total_revenue = sum(total_revenue),
              total_cost = sum(total_cost),
              total_profit = sum(total_profit),
              average_unit_price = mean(unit_price),
              average_units_sold = mean(units_sold))
  return(summary_stats)
}


crear_grafico_ingresos <- function(data) {
  p <- ggplot(data, aes(x = name, y = total_revenue)) +
    geom_bar(stat = "identity") +
    labs(title = "Ingresos Totales por País", x = "País", y = "Ingresos Totales") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}


analisis_ventas <- function(dbname, host, port, user, password, unix_socket) {
  # Conectar a la base de datos
  con <- conectar_db(dbname, host, port, user, password, unix_socket)
  
  # Extraer datos
  sales_data <- extraer_datos(con, "SELECT * FROM sales")
  countries_data <- extraer_datos(con, "SELECT * FROM countries")
  
  # Procesar datos
  sales_data <- procesar_datos(sales_data, countries_data)
  
  # Analizar datos
  resumen <- analizar_datos(sales_data)
  print(resumen)
  
  # Crear gráfico
  grafico <- crear_grafico_ingresos(sales_data)
  print(grafico)
  
  # Cerrar conexión
  dbDisconnect(con)
  
  # Devolver el resumen y el gráfico
  return(list(resumen = resumen, grafico = grafico))
}

