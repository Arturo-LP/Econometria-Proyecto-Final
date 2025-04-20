library(tidyverse)
library(srvyr)
library(kableExtra)
options(survey.lonely.psu = "adjust")

#########################
# Funciones y variables #
#########################

# Etiquetas para estados
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

# Función para preparar la base
preparar_base_enigh <- function(enigh_df, periodo) {
  # Seleccionar y procesar variables en una sola operación
  base <- enigh_df %>%
    transmute(
      # Variables para el diseño muestral
      upm,           # Unidad Primaria de Muestreo
      est_dis,       # Estrato de diseño muestral
      factor,        # Factor de expansión
      
      # Clave para identificar al hogar
      clave_hogar = paste(ubica_geo, upm, est_dis, folioviv, foliohog, sep = "_"),
      
      # Estado y periodo
      id_edo = substr(ubica_geo, 1, 2),
      edo = codigo_a_nombre[id_edo],
      periodo = periodo,
      
      # Variables de ingreso y gasto
      ing_cor, 
      gasto_mon, 
      
      # Proporciones de gasto calculadas directamente
      prop_alimentos = alimentos / gasto_mon,
      prop_vesti_calz = vesti_calz / gasto_mon,
      prop_vivienda = vivienda / gasto_mon,
      prop_limpieza = limpieza / gasto_mon,
      prop_salud = salud / gasto_mon,
      prop_transporte = transporte / gasto_mon,
      prop_educ_espa = educa_espa / gasto_mon,
      prop_cuid_personal = (cuida_pers + acces_pers) / gasto_mon,
      prop_otros_gas = (otros_gas + transf_gas) / gasto_mon
    )
  
  # Crear diseño muestral y calcular decil
  base <- base %>%
    as_survey_design(id = upm, strata = est_dis, weights = factor, nest = TRUE) %>%
    mutate(decil = ntile(ing_cor, 10)) %>%
    as_tibble()
  
  return(base)
}


# Función para generar tablas de análisis
tabla_analisis <- function(diseno, variables_agrupacion, var_interes, titulo) {
  diseno %>%
    group_by(across(all_of(variables_agrupacion))) %>%
    summarise(
      valor_media = round(survey_mean({{var_interes}}, na.rm = TRUE) * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(across(all_of(variables_agrupacion))) %>%
    kable(caption = titulo) %>%
    kable_styling(full_width = FALSE)
}


#############
# Ejecución #
#############

# Descargar ENIGH 2018 y 2022
enigh2018 <- importinegi::enigh(2018, "concentradohogar", formato = "sav")
enigh2022 <- importinegi::enigh(2022, "concentradohogar", formato = "sav")

# Preparar ambas bases
base_2018 <- preparar_base_enigh(enigh2018, 2018)
base_2022 <- preparar_base_enigh(enigh2022, 2022)

# Combinar 
base_general <- bind_rows(base_2018, base_2022)

# Crear diseño muestral complejo con la base general
diseno_enigh <- base_general %>%
  as_survey_design(
    id = upm,
    strata = est_dis,
    weights = factor,
    nest = TRUE
  )

# Check (tiene que dar 100 con se casi 0)
diseno_enigh %>%
  summarise(
    suma_ponderada = survey_mean(
      prop_alimentos +
        prop_vesti_calz +
        prop_vivienda +
        prop_limpieza +
        prop_salud +
        prop_transporte +
        prop_educ_espa +
        prop_cuid_personal +
        prop_otros_gas,
      na.rm = TRUE
    ) * 100
  )

# Liberar memoria
rm(codigo_a_nombre, enigh2018, enigh2022, base_2018, base_2022)
gc() # Forzar garbage collection


############
# Análisis #
############


# Función para generar tablas de análisis (solo jala con variables de proporción):
# tabla_analisis(tabla de diseño muestral, 
#                variables de agrupacion, 
#                variable de interés, 
#                título de la tabla) 

# names(diseno_enigh$variables) # para ver el nombre de las proporciones

# Ej 1: Proporción de gasto en alimentos por estado para 2018 y 2022
proporciones_por_estado <- tabla_analisis(
  diseno_enigh, 
  c("edo", "periodo"), 
  prop_alimentos, 
  "Proporción de gasto en alimentos por estado (2018 y 2022)"
)

proporciones_por_estado

# Ej 2: Proporción de gasto en alimentos por decil de ingreso para 2018 y 2022
proporciones_por_decil <- tabla_analisis(
  diseno_enigh, 
  c( "decil", "periodo"), 
  prop_alimentos, 
  "Proporción de gasto en alimentos por decil de ingreso (2018 y 2022)"
)

proporciones_por_decil

# Ej 3: Proporción de gasto en alimentos por estado y decil de ingreso para 2018 y 2022
proporciones_por_estado_decil <- tabla_analisis(
  diseno_enigh, 
  c( "edo", "decil", "periodo"), 
  prop_alimentos, 
  "Proporción de gasto en alimentos por estado y decil de ingreso (2018 y 2022)"
)

proporciones_por_estado_decil
