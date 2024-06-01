crear_grafico <- function(data, tipo, x_var = NULL, y_var = NULL, fill_var = NULL, title = NULL, x_label = NULL, y_label = NULL, bins = 30) {
  p <- ggplot(data)
  
  if (tipo == "histograma") {
    p <- p + geom_histogram(aes_string(x = x_var, fill = fill_var), bins = bins)
  } else if (tipo == "boxplot") {
    if (!is.null(x_var) && x_var != "") {
      p <- p + geom_boxplot(aes_string(x = x_var, y = y_var, fill = fill_var))
    } else {
      p <- p + geom_boxplot(aes_string(y = y_var, fill = fill_var))
    }
  } else if (tipo == "puntos") {
    p <- p + geom_point(aes_string(x = x_var, y = y_var, color = fill_var))
  } else if (tipo == "barras") {
    p <- p + geom_bar(aes_string(x = x_var, fill = fill_var), stat = "identity")
  } else if (tipo == "lineas") {
    p <- p + geom_line(aes_string(x = x_var, y = y_var, color = fill_var))
  } else {
    stop("Tipo de gráfico no soportado.")
  }
  
  p <- p + ggtitle(title) + xlab(x_label) + ylab(y_label)
  
  print(p)
}

