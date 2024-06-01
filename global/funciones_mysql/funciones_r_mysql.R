
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

# Función para procesar datos (unir todas las tablas)
procesar_datos <- function(sales_data, countries_data, regions_data, items_data, orders_data) {
  sales_data <- sales_data %>%
    left_join(orders_data, by = c("order_id" = "id")) %>%
    left_join(countries_data, by = c("country_id" = "id")) %>%
    left_join(regions_data, by = c("region_id" = "id")) %>%
    left_join(items_data, by = c("item_id" = "id"))
  return(sales_data)
}


# Función para calcular la moda
calcular_moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Función para crear una tabla resumen de estadísticas
crear_tabla_resumen <- function(data) {
  resumen <- data %>%
    summarise(across(where(is.numeric), list(
      media = ~ mean(.x, na.rm = TRUE),
      mediana = ~ median(.x, na.rm = TRUE),
      varianza = ~ var(.x, na.rm = TRUE),
      desviacion_estandar = ~ sd(.x, na.rm = TRUE),
      moda = ~ calcular_moda(.x)
    )))
  
  return(resumen)
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

# Función para crear diferentes tipos de gráficos
crear_grafico <- function(data, tipo, x, y = NULL, binwidth = NULL) {
  if (tipo == "barras") {
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Gráfico de barras de", y, "por", x), x = x, y = y) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else if (tipo == "dispersión") {
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_point() +
      labs(title = paste("Gráfico de dispersión de", x, "vs.", y), x = x, y = y)
  } else if (tipo == "histograma") {
    p <- ggplot(data, aes_string(x = x)) +
      geom_histogram(binwidth = binwidth, fill = "blue", color = "black") +
      labs(title = paste("Histograma de", x), x = x, y = "Frecuencia")
  } else if (tipo == "lineas") {
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_line() +
      labs(title = paste("Gráfico de líneas de", y, "por", x), x = x, y = y)
  } else {
    stop("Tipo de gráfico no soportado.")
  }
  print(p)
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

