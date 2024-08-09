
library(readxl)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(fs)
library(purrr)
# Pregunta 1. Extraer la información de cada archivo y guardarlo en un dataframe con las siguientes columnas: Ins_Cod, Periodo, Cartera, Coloc_Total, y Cart_vencida

carga_archivos <- function(ruta) {

  excel_files <- dir_ls(ruta, recurse = TRUE, glob = "*.xlsx")
 
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
ruta <- "./T8"

# Usamos la función y guardamos los datos combinados
datos <- carga_archivos(ruta)

# exploramos los datos y verificamos que se cumple lo solicitado
head(datos)
# Pregunta 2. Calcular para cada periodo, tipo de cartera y monto, el sistema bancario (esdecir, la suma de todos los bancos), asígnele el código de institución 999 y añádalo al dataframe
# Creamos una data sumaria del sistema bancario
sistema_bancario <- datos %>%
  group_by(PERIODO, CARTERA) %>%
  summarise(
    COLOC_TOTAL = sum(COLOC_TOTAL),
    CART_VENCIDA = sum(CART_VENCIDA),
    .groups = 'drop'
  ) %>%
  mutate(INS_COD = 999) %>%
  select(INS_COD, PERIODO, CARTERA, COLOC_TOTAL, CART_VENCIDA) %>%
  drop_na() 
data_finalizada <- bind_rows(datos, sistema_bancario)
# Pregunta 3.Calcular el indicador de cartera vencida, para cada cartera i
data_finalizada <- data_finalizada %>%
  mutate(INDICADOR_CARTERA_VENCIDA = 100 * CART_VENCIDA / COLOC_TOTAL)
head(data_finalizada)
# Pregunta 4. Exporte el dataframe resultante a un archivo CSV delimitado por punto y coma
# Se usa write_csv2 del paquete readr que por defecto usa ";"
write_csv2(data_finalizada, "resultado_final.csv")
# Pregunta 5 El archivo InsCod.txt contiene una tabla con el código de la institución y su nombre, cárguelo en un dataframe y crúcelo con la información del T8 procesada.
# Cargamos el txt y despues usamos left_join

ins_cod_txt<- read_delim("./InsCod.txt", delim = "\t")

data_finalizada <- data_finalizada %>%
  left_join(ins_cod_txt, by = "INS_COD")
# Pregunta 6 a. ¿En qué periodo el indicador de cartera vencida del total de colocaciones del sistema bancario alcanza su máximo? ¿Cuál es ese valor?
max_indicador <- data_finalizada %>%
  filter(INS_COD == 999, CARTERA == "TOTAL:") %>%
  arrange(desc(INDICADOR_CARTERA_VENCIDA)) %>%
  slice(1)
print(max_indicador)
# Es el periodo 202405, de las colocaciones con un factor 1.07
# Pregunta 6 b. ¿En qué periodo el indicador de cartera vencida del total de colocaciones del sistema bancario alcanza su mínimo? ¿Cuál es ese valor?
min_indicador <- data_finalizada %>%
  filter(INS_COD == 999, CARTERA == "TOTAL:") %>%
  arrange(INDICADOR_CARTERA_VENCIDA) %>%
  slice(1)
print(min_indicador)
# Es el periodo 202305, de las colocaciones con un indicador 0.834
# Pregunta 7 Considerando la información de mayo 2024 (202405)
# a. ¿Qué banco tiene la mayor participación en cuanto a colocaciones totales? ¿y por carteras?
max_participacion_cartera <- data_finalizada %>%
  filter(PERIODO == 202405, CARTERA == "TOTAL:", INS_COD != 999) %>%
  group_by(CARTERA) %>%
  arrange(desc(COLOC_TOTAL)) %>%
  slice(1) %>%
  ungroup()
print(max_participacion_cartera)
# Total es Santander con 40756745
max_participacion_cartera_com <- data_finalizada %>%
  filter(PERIODO == 202405, CARTERA == "Colocaciones comerciales:", INS_COD != 999) %>%
  group_by(CARTERA) %>%
  arrange(desc(COLOC_TOTAL)) %>%
  slice(1) %>%
  ungroup()
print(max_participacion_cartera_com)
# Comercial es Itaú
max_participacion_cartera_vivienda <- data_finalizada %>%
  filter(PERIODO == 202405, CARTERA == "Colocaciones para vivienda:", INS_COD != 999) %>%
  group_by(CARTERA) %>%
  arrange(desc(COLOC_TOTAL)) %>%
  slice(1) %>%
  ungroup()
print(max_participacion_cartera_vivienda)
# Vivienda es Itaú
max_participacion_cartera_consumo <- data_finalizada %>%
  filter(PERIODO == 202405, CARTERA == "Colocaciones de consumo:", INS_COD != 999) %>%
  group_by(CARTERA) %>%
  arrange(desc(COLOC_TOTAL)) %>%
  slice(1) %>%
  ungroup()
print(max_participacion_cartera_consumo)
# Consumo es Itaú
# ii. ¿Qué bancos tienen un indicador de cartera vencida mayor al del Sistema Bancario?
cartera_total <- data_finalizada %>%
  filter(CARTERA == "TOTAL:")
indicador_SB <- cartera_total %>%
  filter(INS_COD == 999) %>%
  summarise(INDICADOR_MAX = max(INDICADOR_CARTERA_VENCIDA, na.rm = TRUE)) %>%
  pull(INDICADOR_MAX)
instituciones_mayor_indicador <- cartera_total %>%
  filter(INS_COD != 999, PERIODO ==202405, INDICADOR_CARTERA_VENCIDA > indicador_SB)
print(instituciones_mayor_indicador)
# Son las instituciones, BECH, Santander, Itaú, Security, Ripley, y CCB
# iii. ¿Qué bancos tienen un indicador de cartera vencida menor al del Sistema Bancario?
instituciones_menor_indicador <- cartera_total %>%
  filter(INS_COD != 999, PERIODO ==202405, INDICADOR_CARTERA_VENCIDA < indicador_SB)
print(instituciones_menor_indicador)
# Son el Chile, Internacional  Scotiabank, BCI, BICE, HSBC, JP Morgan, Falabella, Consorcio, BTG y Bank of China.
