library(readxl)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(fs)
library(purrr)
cargar_archivos_excel <- function(archivoscomb) {
  archivos_excel <- dir_ls(archivoscomb, recurse = T, glob = "*.xlsx")
  datos_combinados <- archivos_excel %>%
    map_dfr(~ read_excel(.x))
  
  return(datos_combinados)
}  
archivoscomb <- "./T8"

fulldata <- cargar_archivos_excel(archivoscomb)
