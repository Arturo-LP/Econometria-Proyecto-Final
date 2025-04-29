# Directorio de trabajo
setwd("C:/Users/alope/OneDrive/Documentos/R Projects/CIDE ME - Econometria/Proyecto Final/Bases de datos/INPC Estatal")

library(tidyverse)
library(readr)
library(janitor)
library(dplyr)
library(tidyr)

# Lista de archivos del INPC por estado
archivos_csv <- list.files(pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

#########################
# Funciones y variables #
#########################

# Diccionario de nombres en sincronía con el archivo de ENIGH
nombres_subindices <- c(
  "inpc_general",
  "inpc_alimentos",
  "inpc_vesti_calz",
  "inpc_vivienda",
  "inpc_limpieza",
  "inpc_salud",
  "inpc_cuid_personal",
  "inpc_transporte",
  "inpc_educ_espa",
  "inpc_otros_gas"
)

# Diccionario de códigos a nombres de estado
codigo_a_nombre <- c(
  "01" = "Aguascalientes", "02" = "Baja California", "03" = "Baja California Sur",
  "04" = "Campeche", "05" = "Coahuila de Zaragoza", "06" = "Colima", "07" = "Chiapas",
  "08" = "Chihuahua", "09" = "Ciudad de México", "10" = "Durango",
  "11" = "Guanajuato", "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco",
  "15" = "Estado de México", "16" = "Michoacán de Ocampo", "17" = "Morelos", "18" = "Nayarit",
  "19" = "Nuevo León", "20" = "Oaxaca", "21" = "Puebla", "22" = "Querétaro",
  "23" = "Quintana Roo", "24" = "San Luis Potosí", "25" = "Sinaloa", "26" = "Sonora",
  "27" = "Tabasco", "28" = "Tamaulipas", "29" = "Tlaxcala", "30" = "Veracruz de Ignacio de la Llave",
  "31" = "Yucatán", "32" = "Zacatecas"
)

# Función para leer y limpiar cada archivo
leer_inpc_estado <- function(archivo) {
  # Extraer código y nombre del estado
  codigo <- str_extract(basename(archivo), "(?<=INPC_)[0-9]{2}")
  edo <- codigo_a_nombre[codigo]
  
  # Encabezado en sincronía con el script de ENIGH
  encabezado_limpio <- c("fecha", nombres_subindices)
  
  # Leer datos desde fila 21 (índices válidos, jul-18 en adelante)
  datos <- read_csv(
    archivo,
    skip = 20,
    col_names = encabezado_limpio,
    locale = locale(encoding = "Latin1"),
    show_col_types = FALSE,
    col_types = cols(
      fecha = col_character(), .default = col_double()
    )
  )
  
  # Diccionario para convertir abreviaturas de meses en español a números
  # Esto elimina los errores de codificación y parsing
  meses_es_a_num <- c(
    "Ene" = "01", "Feb" = "02", "Mar" = "03", "Abr" = "04", 
    "May" = "05", "Jun" = "06", "Jul" = "07", "Ago" = "08", 
    "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dic" = "12"
  )
  
  # Función para convertir formatos "Jul 2018" a "2018-07-01"
  convertir_fecha <- function(fecha_texto) {
    partes <- str_split(fecha_texto, " ", simplify = TRUE)
    mes_abr <- partes[, 1]
    anio <- partes[, 2]
    mes_num <- meses_es_a_num[mes_abr]
    fecha_iso <- paste0(anio, "-", mes_num, "-01") # Primer día del mes
    return(as.Date(fecha_iso))
  }
  
  # Aplicar transformaciones
  datos_procesados <- datos %>%
    mutate(
      fecha = convertir_fecha(fecha),
      edo = edo
    ) %>%
    pivot_longer(
      cols = starts_with("inpc_"),
      names_to = "rubro",
      values_to = "inpc"
    ) %>%
    filter(!is.na(inpc)) %>%
    select(fecha, edo, rubro, inpc)
  
  return(datos_procesados)
}


#############
# Ejecución #
#############


# Data frame
inpc_estatal <- map_dfr(archivos_csv, leer_inpc_estado)

# Liberar memoria
rm(archivos_csv, codigo_a_nombre)
gc() # Forzar garbage collection

############################
# Indicadores de Inflación #
############################

# Fechas de interés
fecha_base <- as.Date("2018-07-01")
fecha_2020 <- as.Date("2020-07-01")
fecha_2022 <- as.Date("2022-07-01")

# Data fram filtrado
inpc_filtrado <- inpc_estatal %>%
  filter(fecha %in% c(fecha_base, fecha_2020, fecha_2022))

# Convertir a formato ancho
inpc_ancho <- inpc_filtrado %>%
  mutate(
    periodo = format(fecha, "%Y_%m")
  ) %>%
  select(edo, rubro, periodo, inpc) %>%
  pivot_wider(
    names_from = periodo,
    values_from = inpc,
    names_prefix = "inpc_"
  )

### Indicadores a calcular:
# Inflación acumulada hasta 2020 y 2022
# Tasa de Inflación Media Anual Compuesta (TMAC)

inflacion <- inpc_ancho %>%
  mutate(
    # Inflación simple entre períodos específicos
    inflacion_2018_2020 = (inpc_2020_07 / inpc_2018_07) - 1, # inflación acumulada
    inflacion_2018_2022 = (inpc_2022_07 / inpc_2018_07) - 1, # inflación acumulada
    
    # TMAC: Tasa Media Anual Compuesta
    # Fórmula: (Valor final / Valor inicial)^(1/n) - 1
    # donde n es el número de años
    
    # Para el período 2018-2020 (2 años)
    tmac_2018_2020 = (inpc_2020_07 / inpc_2018_07)^(1/2) - 1,
    
    # Para el período 2018-2022 (4 años)
    tmac_2018_2022 = (inpc_2022_07 / inpc_2018_07)^(1/4) - 1
  ) %>%
  select(edo, rubro, inflacion_2018_2020, inflacion_2018_2022, tmac_2018_2020, tmac_2018_2022)


# Guardar en la memoria
saveRDS(inflacion, "inflacion.rds")
saveRDS(inpc_estatal, "inpc_estatal.rds")


############
# Análisis #
############

library(dplyr)
library(tidyr)
library(ggplot2)


# --- Periodo: 2018-2020 ---

# Gráficos de Inflación Acumulada entre 2018 y 2020
for (r in nombres_subindices) {
  inflacion %>%
    filter(rubro == r) %>%
    ggplot(aes(x = reorder(edo, inflacion_2018_2020), y = inflacion_2018_2020 * 100)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Inflación Acumulada 2018–2020:", r),
      x = "Estado",
      y = "Inflación acumulada"
    ) +
    theme_minimal(base_size = 13) -> p_acumulada
  
  print(p_acumulada)
}

# Gráficos de Tasa de Inflación Anual Compuesta entre 2018 y 2020
for (r in nombres_subindices) {
  inflacion %>%
    filter(rubro == r) %>%
    ggplot(aes(x = reorder(edo, tmac_2018_2020), y = tmac_2018_2020 * 100)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Tasa de Inflación Anual Compuesta 2018–2020:", r),
      x = "Estado",
      y = "Tasa de Inflación Anual Compuesta"
    ) +
    theme_minimal(base_size = 13) -> p_compuesta
  
  print(p_compuesta)
}


# --- Periodo: 2018-2022 ---

# Gráficos de Inflación Acumulada entre 2018-2022
for (r in nombres_subindices) {
  inflacion %>%
    filter(rubro == r) %>%
    ggplot(aes(x = reorder(edo, inflacion_2018_2022), y = inflacion_2018_2022 * 100)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Inflación Acumulada 2018–2022:", r),
      x = "Estado",
      y = "Inflación acumulada"
    ) +
    theme_minimal(base_size = 13) -> p_acumulada
  
  print(p_acumulada)
}

# Gráficos de Tasa de Inflación Anual Compuesta entre 2018-2022
for (r in nombres_subindices) {
  inflacion %>%
    filter(rubro == r) %>%
    ggplot(aes(x = reorder(edo, tmac_2018_2022), y = tmac_2018_2022 * 100)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Tasa de Inflación Anual Compuesta 2018–2022:", r),
      x = "Estado",
      y = "Tasa de Inflación Anual Compuesta"
    ) +
    theme_minimal(base_size = 13) -> p_compuesta
  
  print(p_compuesta)
}

