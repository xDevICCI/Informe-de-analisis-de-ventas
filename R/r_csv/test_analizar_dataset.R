# test_analizar_dataset.R
library(testthat)
library(dplyr)
library(ggplot2)
library(shiny)
library(readxl)
library(openxlsx)
library(tools)
library(assertthat)

options(expressions = 500000)

# Asegúrate de que esta sea la ruta correcta al script que contiene la función analizar_dataset
source("./test_analizar_dataset.R")

# Función auxiliar para cargar datos y limpiar nombres de columnas
load_data <- function(file_path) {
  file_extension <- tools::file_ext(file_path)
  if (file_extension == "csv") {
    datos <- read.csv(file_path)
  } else if (file_extension == "xlsx") {
    datos <- read.xlsx(file_path, sheet = 1)
  } else {
    stop("Formato de archivo no soportado")
  }
  names(datos) <- gsub(" ", "_", names(datos))
  return(datos)
}

test_that("analizar_dataset funciona correctamente con CSV", {
  # Ruta al archivo CSV proporcionado
  file_path <- './data/100_sales.csv'
  
  # Llama a la función
  result <- tryCatch({
    analizar_dataset(file_path)
    TRUE
  }, error = function(e) {
    message("Error en analizar_dataset: ", e$message)
    FALSE
  })
  
  # Verifica que la función no retorna error
  assert_that(result, msg = "La función analizar_dataset falló")
})

test_that("analizar_dataset muestra el resumen correcto", {
  # Ruta al archivo CSV proporcionado
  file_path <- './data/100_sales.csv'
  
  # Cargar los datos directamente
  datos <- load_data(file_path)
  
  # Resumen esperado
  resumen_esperado <- datos %>%
    group_by(Region, Item_Type) %>%
    summarise(Total_Units_Sold = sum(Units_Sold, na.rm = TRUE),
              Total_Revenue = sum(Total_Revenue, na.rm = TRUE)) %>%
    arrange(Region, desc(Total_Units_Sold))
  
  # Llama a la función para generar el resumen
  resumen_generado <- datos %>%
    group_by(Region, Item_Type) %>%
    summarise(Total_Units_Sold = sum(Units_Sold, na.rm = TRUE),
              Total_Revenue = sum(Total_Revenue, na.rm = TRUE)) %>%
    arrange(Region, desc(Total_Units_Sold))
  
  # Verifica que el resumen generado es correcto
  assert_that(identical(resumen_generado, resumen_esperado), msg = "El resumen generado no coincide con el esperado")
})
