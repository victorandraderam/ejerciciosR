
library(readxl)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(fs)
library(purrr)


load_excel_files <- function(relative_path) {

  excel_files <- dir_ls(relative_path, recurse = TRUE, glob = "*.xlsx")
 
  combined_data <- excel_files %>%
    map_dfr(~ {
      file_name <- path_file(.x)
      
      # Como los archivos tienen nombre estandarizado, sacamos de cada uno el número de institución (ex código SBIF) y la fecha del reporte en YYYYMM
      institucion <- as.integer(str_sub(file_name, 1, 3))
      fecha <- as.integer(str_sub(file_name, 5, 10))
      
      # Leer el archivo Excel usando un rango específico de celdas
      data <- read_excel(.x, range = "B10:E16", col_names = FALSE)
      
      # Renombrar las columnas relevantes
      colnames(data) <- c("CARTERA", "CÓDIGOS","COLOC_TOTAL", "CART_VENCIDA")
      
      # filtramos las columnas que nos interesan (sacamos los de códigos contables) y agregamos INS_COD y Periodo, que obtuvimos desde el nombre de archivo
      data <- data %>%
        select(CARTERA, COLOC_TOTAL, CART_VENCIDA) %>%
        mutate(
          COLOC_TOTAL = as.numeric(COLOC_TOTAL),
          CART_VENCIDA = as.numeric(CART_VENCIDA),
          INS_COD = institucion,
          PERIODO = fecha
        )
      
      return(data)
    })
  
  return(combined_data)
}

# Hicimos un unzip de T8.zip y lo copiamos en T8 dentro del directorio de trabajo, lo que nos permite usar rutas relativas
relative_path <- "./T8"

# Llamar a la función y guardar los datos combinados
datos <- load_excel_files(relative_path)

# Visualizar los primeros registros del data frame combinado
head(datos)
