# Instalar las librerías si no están ya instaladas
install.packages("DBI")
install.packages("RMySQL")
install.packages("readr")



# Cargar las librerías
library(DBI)
library(RMySQL)
library(readr)

# Conectar a la base de datos MySQL
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "sales", 
                 host = "127.0.0.1", 
                 port = 3306, 
                 user = "root", 
                 password = "")

# Leer el archivo CSV
data <- read_csv("./data/5m_sales.csv")

data <- data.frame(
  region = data$Region,
  country = data$Country,
  item_type = data$`Item Type`,
  sales_channel = data$`Sales Channel`,
  order_priority = data$`Order Priority`,
  order_date = as.Date(data$`Order Date`, format="%m/%d/%Y"),
  order_id = data$`Order ID`,
  ship_date = as.Date(data$`Ship Date`, format="%m/%d/%Y"),
  units_sold = data$`Units Sold`,
  unit_price = data$`Unit Price`,
  unit_cost = data$`Unit Cost`,
  total_revenue = data$`Total Revenue`,
  total_cost = data$`Total Cost`,
  total_profit = data$`Total Profit`
)

# Insertar datos en la tabla `regions`
regions <- unique(data.frame(name = data$region))
dbWriteTable(con, "regions", regions, append = TRUE, row.names = FALSE)

# Obtener el id de las regiones
regions_ids <- dbReadTable(con, "regions")

# Insertar datos en la tabla `countries`
data$region_id <- sapply(data$region, function(x) regions_ids$id[regions_ids$name == x])
countries <- unique(data.frame(name = data$country, region_id = data$region_id))
dbWriteTable(con, "countries", countries, append = TRUE, row.names = FALSE)

# Obtener el id de los países
countries_ids <- dbReadTable(con, "countries")

# Insertar datos en la tabla `items`
items <- unique(data.frame(item_type = data$item_type))
dbWriteTable(con, "items", items, append = TRUE, row.names = FALSE)

# Obtener el id de los tipos de artículos
items_ids <- dbReadTable(con, "items")

# Insertar datos en la tabla `orders`
orders <- unique(data.frame(id = data$order_id, order_date = data$order_date, ship_date = data$ship_date, order_priority = data$order_priority))
dbWriteTable(con, "orders", orders, append = TRUE, row.names = FALSE)

# Insertar datos en la tabla `sales`
data$country_id <- sapply(data$country, function(x) countries_ids$id[countries_ids$name == x])
data$item_id <- sapply(data$item_type, function(x) items_ids$id[items_ids$item_type == x])
sales <- data.frame(
  order_id = data$order_id,
  country_id = data$country_id,
  item_id = data$item_id,
  sales_channel = data$sales_channel,
  units_sold = data$units_sold,
  unit_price = data$unit_price,
  unit_cost = data$unit_cost,
  total_revenue = data$total_revenue,
  total_cost = data$total_cost,
  total_profit = data$total_profit
)
dbWriteTable(con, "sales", sales, append = TRUE, row.names = FALSE)



# Cerrar la conexión a la base de datos
dbDisconnect(con)
