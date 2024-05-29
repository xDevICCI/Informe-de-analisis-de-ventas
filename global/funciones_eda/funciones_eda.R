# Librerías necesarias
library(tidyverse)

# Función para calcular resúmenes numéricos de variables numéricas automáticamente
resumen_numerico <- function(data) {
  # Seleccionar solo las columnas numéricas
  data_selected <- data %>% select_if(is.numeric)
  
  # Calcular el resumen numérico para cada variable
  resumen <- data_selected %>% 
    summarise(across(everything(), list(
      Min = ~ min(.),
      Max = ~ max(.),
      Media = ~ mean(.),
      Mediana = ~ median(.),
      Desviacion_Estandar = ~ sd(.)
    ), .names = "{col}_{fn}"))
  
  return(resumen)
}


# Función para crear una base de datos reducida
base_datos_reducida <- function(data, date_var = "Date", group_vars = NULL) {
  # Asegurarse de que la variable Date esté en formato de fecha (FORMATO DD-MM-YYYY)
  data <- data %>% mutate(across(all_of(date_var), as.Date, format = "%Y-%m-%d"))
  
  # Filtrar solo las columnas que existen en el data frame
  valid_group_vars <- group_vars[group_vars %in% colnames(data)]
  if (length(valid_group_vars) == 0) {
    valid_group_vars <- NULL
  }
  
  # Agrupar por Date y variables cualitativas válidas, y sumar las variables cuantitativas
  data_reducida <- data %>%
    group_by(across(all_of(c(date_var, valid_group_vars)))) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') %>%
    arrange(across(all_of(date_var)))
  
  return(data_reducida)
}

# Función para calcular estadísticas descriptivas por grupo
estadisticas_por_grupo <- function(data, group_var) {
  # Seleccionar solo las columnas numéricas
  numeric_vars <- names(select_if(data, is.numeric))
  
  # Agrupar por la variable especificada y calcular resúmenes numéricos para las variables cuantitativas
  estadisticas <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(across(all_of(numeric_vars), list(
      Min = ~ min(.),
      Max = ~ max(.),
      Media = ~ mean(.),
      Mediana = ~ median(.),
      Desviacion_Estandar = ~ sd(.)
    ), .names = "{col}_{fn}"), .groups = 'drop')
  
  return(estadisticas)
}


# Función para validar y verificar los datos
validar_datos <- function(data) {
  # Verificar los nombres de las columnas
  print("Nombres de las columnas:")
  print(colnames(data))
  
  # Verificar el tipo de datos de cada columna
  print("Tipos de datos de cada columna:")
  print(str(data))
  
  # Verificar valores únicos en variables cualitativas
  print("Valores únicos en variables cualitativas:")
  cualitativas <- names(select_if(data, is.character))
  for (var in cualitativas) {
    print(paste("Valores únicos en", var, ":"))
    print(unique(data[[var]]))
  }
  
  # Verificar si hay valores faltantes
  print("Conteo de valores faltantes en cada columna:")
  na_count <- sapply(data, function(x) sum(is.na(x)))
  print(na_count)
}

