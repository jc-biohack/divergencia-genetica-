#Divergencia Genética

#cargar librerías
library(readxl)
library(dplyr)
library(tibble)
library(reshape2)

#Establecer el archivo de entrada
archivo <- "divergencia.xlsx"

#Indicar los nombres excatos de las hojas en Excel
hojas <- c("its", "psbA", "concatenado")

#Función principal
analizar_locus <- function(hoja, titulo){
  
#Leer matriz de distancias
  mat <- read_excel(archivo, sheet = hoja)
  
#Primera columna como nombres de fila
  mat <- mat %>% column_to_rownames(var = colnames(mat)[1])
  mat <- as.matrix(mat)
  
#Convertir a formato largo
  dist <- melt(mat)
  colnames(dist) <- c("muestra1", "muestra2", "distancia")
  
#Forzar tipos correctos
  dist$muestra1 <- as.character(dist$muestra1)
  dist$muestra2 <- as.character(dist$muestra2)
  dist$distancia <- as.numeric(dist$distancia)
  
#Limpiar datos
  dist <- dist %>%
    filter(!is.na(distancia),
           muestra1 != muestra2)
  
#Clasificación intra / inter
  
  dist$tipo <- ifelse(
    substr(dist$muestra1, 1, 3) == substr(dist$muestra2, 1, 3),
    "Intraespecífica",
    "Interespecífica"
  )
  
#Eliminar duplicados
  dist <- dist %>%
    mutate(par = paste(
      pmin(muestra1, muestra2),
      pmax(muestra1, muestra2),
      sep = "_"
    )) %>%
    distinct(par, .keep_all = TRUE)
  

#HISTOGRAMA (Barcode gap)

  png(paste0(hoja, "_histograma.png"),
      width = 1800, height = 1400, res = 300)
  
  par(mar = c(5,5,4,2))
  
  max_val <- max(dist$distancia, na.rm = TRUE)
  
  hist(dist$distancia[dist$tipo == "Interespecífica"],
       breaks = seq(0, max_val, length.out = 40),
       col = "gold",
       border = "black",
       xlab = "Distancia genética (K2P)",
       ylab = "Número de comparaciones",
       main = titulo)
  
  hist(dist$distancia[dist$tipo == "Intraespecífica"],
       breaks = seq(0, max_val, length.out = 40),
       col = "red",
       border = "black",
       add = TRUE)
  
  legend("topright",
         legend = c("Interespecífica", "Intraespecífica"),
         fill = c("gold", "red"),
         bty = "n")
  
  dev.off()
  

  #  PRUEBA DE WILCOXON

  test <- wilcox.test(distancia ~ tipo, data = dist)
  
  cat("\n  \n")
  cat("Locus:", hoja, "\n")
  print(test)
  
  return(dist)
}

#Ejecutar automaticamente
resultados <- lapply(hojas, function(h) {
  analizar_locus(
    hoja   = h,
    titulo = paste("Divergencia genética –", h)
  )
})

names(resultados) <- hojas

