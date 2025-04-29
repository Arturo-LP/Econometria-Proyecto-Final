library(tidyverse)
library(srvyr)
library(kableExtra)
# Aumentar el timeout para permitir descargas grandes
options(timeout = 600)
# Ajustar la varianza cuando hay algún estrato con una sola PSU
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

# Descargar ENIGH 2018, 2020 y 2022
enigh2018 <- importinegi::enigh(2018, "concentradohogar", formato = "sav")
enigh2020 <- importinegi::enigh(2020, "concentradohogar", formato = "sav")
enigh2022 <- importinegi::enigh(2022, "concentradohogar", formato = "sav")

# Preparar ambas bases
base_2018 <- preparar_base_enigh(enigh2018, 2018)
base_2020 <- preparar_base_enigh(enigh2020, 2020)
base_2022 <- preparar_base_enigh(enigh2022, 2022)

# Combinar 
base_general <- bind_rows(base_2018, base_2020, base_2022)

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

# Guardar en la memoria
saveRDS(diseno_enigh, "diseno_enigh.rds")

# Liberar memoria
rm(codigo_a_nombre, enigh2018, enigh2020, enigh2022, base_2018, base_2020, base_2022)
gc() # Forzar garbage collection


##########
# Tablas #
##########


# Función para generar tablas de análisis (solo jala con variables de proporción):
# tabla_analisis(tabla de diseño muestral, 
#                variables de agrupacion, 
#                variable de interés, 
#                título de la tabla) 

# names(diseno_enigh$variables) # para ver el nombre de las proporciones

# Ej 1: Proporción de gasto en alimentos por estado para 2018 - 2022
proporciones_por_estado <- tabla_analisis(
  diseno_enigh, 
  c("edo", "periodo"), 
  prop_alimentos, 
  "ENIGH: Proporción de gasto en alimentos por estado (2018 - 2022)"
)

proporciones_por_estado

# Ej 2: Proporción de gasto en alimentos por decil de ingreso para 2018 - 2022
proporciones_por_decil <- tabla_analisis(
  diseno_enigh, 
  c( "decil", "periodo"), 
  prop_alimentos, 
  "ENIGH: Proporción de gasto en alimentos por decil de ingreso (2018 - 2022)"
)

proporciones_por_decil

# Ej 3: Proporción de gasto en alimentos por estado y decil de ingreso para 2018 - 2022
proporciones_por_estado_decil <- tabla_analisis(
  diseno_enigh, 
  c( "edo", "decil", "periodo"), 
  prop_alimentos, 
  "ENIGH: Proporción de gasto en alimentos por estado y decil de ingreso (2018 - 2022)"
)

proporciones_por_estado_decil


############
# GRÁFICAS #
############

library(ggplot2)

# names(diseno_enigh$variables) # para ver el nombre de las proporciones

# --- Ventana completa ---

# Calcular proporción ponderada promedio por estado y periodo
datos_para_grafico <- diseno_enigh %>%
  group_by(periodo, edo) %>%
  summarise(
    prop_media = survey_mean(prop_alimentos,    # Insertar proporción de interés
                             na.rm = TRUE),
    .groups = "drop"
  )

# Gráficos
plot_prop_gasto <- unique(datos_para_grafico$periodo) %>%
  purrr::map(~ {
    ggplot(datos_para_grafico %>% filter(periodo == .x),
           aes(x = reorder(edo, prop_media), y = prop_media)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      coord_flip() +
      labs(title = paste("ENIGH", .x),
           x = "Estado", y = "Proporción del gasto monetario total") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(size = 6)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
    }
  )

patchwork::wrap_plots(plot_prop_gasto, ncol = 3) +
  plot_annotation(
    title = "Proporción del gasto corriente en alimentos por estado y periodo", # Insertar proporción de interés
    caption = "Fuente: Elaboración propia con datos de la ENIGH, INEGI.",
    theme = theme(plot.title = element_text(size = 16, face = "bold"),
                  plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10))
    )
  )

