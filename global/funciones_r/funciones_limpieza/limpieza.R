# Función para limpiar datos nulos
limpiar_datos_nulos <- function(data) {
  # Reemplazar valores nulos en variables numéricas por la media de la columna
  data <- data %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # Reemplazar valores nulos en variables cualitativas por "Desconocido"
  data <- data %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "Desconocido", .)))
  
  return(data)
}


# Función para quitar espacios de los nombres de las columnas
quitar_espacios_nombres <- function(df) {
  colnames(df) <- gsub(" ", "_", colnames(df))
  return(df)
}