# install.packages("readxl")
# install.packages("openxlsx")

library(dplyr)
library(ggplot2)
library(shiny)
library(readxl)
library(openxlsx)


#Explicación:
#  1.	Carga y Preprocesamiento:
#     - Cargar los datos desde un archivo CSV y ajustar los nombres de las columnas para eliminar espacios.
#     - Verificar y mostrar la cantidad de valores nulos.
#.    - Tomar una muestra aleatoria de 100,000 registros si el dataset contiene más de 100,000 filas para reducir 
#.    el uso de memoria.
#. 2.	Análisis y Resumen:
#.    - Agrupar los datos por región y tipo de producto, y calcular la suma de las unidades vendidas (Total_Units_Sold) y 
#.    - los ingresos totales (Total_Revenue) para cada combinación.
#     - Ordenar los resultados por región y unidades vendidas en orden descendente.
#  3.	Aplicación Shiny:
#     - Crear una interfaz interactiva que permita al usuario seleccionar una región y un tipo de producto.
#	    - Añadir un panel de pestañas (tabsetPanel) para incluir múltiples gráficos y visualizaciones:
#	    - Gráfico de barras de ventas por tipo de producto en la región seleccionada.
#	    - Histograma de ingresos totales.
#	    - Gráfico de dispersión de precio unitario vs. unidades vendidas.
#	    - Gráfico de líneas de ingresos totales a lo largo del tiempo.
#	    - Recomendación basada en el tipo de producto más vendido en la región seleccionada.




#****Explicación ¿Para qué sirve?###########
#    permite analizar las ventas y realizar recomendaciones de productos basadas en las ventas regionales, 
#.   con múltiples gráficos interactivos que facilitan la exploración y comprensión de los datos, todo mientras 
#.   se mantiene un uso eficiente de la memoria.
#*****##########################
#*
#*
analizar_dataset <- function(file_path) {
  
  # Cargar el dataset
  # Detectar el tipo de archivo y cargar el dataset
  file_extension <- tools::file_ext(file_path)
  
  if (file_extension == "csv") {
    datos <- read.csv(file_path)
  } else if (file_extension == "xlsx") {
    datos <- read.xlsx(file_path, sheet = 1)
  } else {
    stop("Tipo de archivo no soportado. Por favor, cargue un archivo CSV o XLSX.")
  }
  
  # Quitar espacios de los nombres de las columnas
  names(datos) <- gsub(" ", "_", names(datos))
  
  # Verificar si hay valores nulos
  print("Valores nulos en el dataset:")
  print(colSums(is.na(datos)))
  
  # Tomar una muestra aleatoria de 100,000 registros para reducir el uso de memoria
  set.seed(123)
  if (nrow(datos) > 100000) {
    datos <- datos %>% sample_n(100000)
  }
  
  # Mostrar las primeras filas del dataset
  print("Primeras filas del dataset:")
  print(head(datos))
  
  # Descripción estadística del dataset
  print("Descripción estadística del dataset:")
  print(summary(datos))
  
  # Resumen por región y tipo de producto
  resumen_region_producto <- datos %>%
    group_by(Region, Item.Type) %>%
    summarise(Total_Units_Sold = sum(Units.Sold, na.rm = TRUE),
              Total.Revenue = sum(Total.Revenue, na.rm = TRUE)) %>%
    arrange(Region, desc(Total_Units_Sold))
  print("Resumen por región y tipo de producto:")
  print(resumen_region_producto)
  
  # Crear la aplicación Shiny
  app <- shinyApp(
    ui = fluidPage(
      titlePanel("Sistema de Recomendación de Productos"),
      sidebarLayout(
        sidebarPanel(
          selectInput("region", "Seleccionar Región:", choices = unique(datos$Region)),
          selectInput("producto", "Seleccionar Producto:", choices = unique(datos$Item.Type))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Gráfico de Barras", plotOutput("barPlot")),
            tabPanel("Histograma de Ventas", plotOutput("histPlot")),
            tabPanel("Dispersión Precio vs Unidades", plotOutput("scatterPlot")),
            tabPanel("Ingresos Totales", plotOutput("linePlot")),
            tabPanel("Recomendación", verbatimTextOutput("recommendation"))
          )
        )
      )
    ),
    server = function(input, output) {
      
      output$classInfo <- renderPrint({
        class(app)  # Mostrar la clase del objeto app
      })
      
      output$barPlot <- renderPlot({
        datos_filtrados <- resumen_region_producto %>% filter(Region == input$region)
        ggplot(datos_filtrados, aes(x = reorder(Item.Type, -Total_Units_Sold), y = Total_Units_Sold, fill = Item.Type)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Ventas por Tipo de Producto en", input$region),
               x = "Tipo de Producto", y = "Unidades Vendidas") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      output$histPlot <- renderPlot({
        ggplot(datos, aes(x = Total.Revenue)) +
          geom_histogram(bins = 30, fill = 'blue', alpha = 0.7) +
          labs(title = "Histograma de Ingresos Totales", x = "Ingresos Totales", y = "Frecuencia")
      })
      
      output$scatterPlot <- renderPlot({
        ggplot(datos, aes(x = Unit.Price, y = Units.Sold)) +
          geom_point(alpha = 0.5) +
          labs(title = "Dispersión Precio vs Unidades Vendidas", x = "Precio Unitario", y = "Unidades Vendidas")
      })
      
      output$linePlot <- renderPlot({
        datos %>%
          mutate(Order.Date = as.Date(Order.Date, format = "%m/%d/%Y")) %>%
          group_by(Order.Date) %>%
          summarise(Total.Revenue = sum(Total.Revenue, na.rm = TRUE)) %>%
          ggplot(aes(x = Order.Date, y = Total.Revenue)) +
          geom_line(color = 'blue') +
          labs(title = "Ingresos Totales a lo Largo del Tiempo", x = "Fecha de Orden", y = "Ingresos Totales")
      })
      
      output$recommendation <- renderPrint({
        recomendacion <- resumen_region_producto %>% 
          filter(Region == input$region, Item.Type == input$producto) %>% 
          top_n(1, wt = Total_Units_Sold)
        print(paste("En la región", input$region, "se recomienda el producto", input$producto, "con", recomendacion$Total_Units_Sold, "unidades vendidas."))
      })
    }
  )
  
  runApp(app)  # Ejecutar la aplicación Shiny
}

# Llamar a la función con la ruta del archivo
analizar_dataset('./data/5m_sales.csv')



#install.packages("rsconnect")



library(rsconnect)


rsconnect::setAccountInfo(name='victorgodoyroaudla',
                          token='secret',
                          secret='secret')



rsconnect::deployApp()
