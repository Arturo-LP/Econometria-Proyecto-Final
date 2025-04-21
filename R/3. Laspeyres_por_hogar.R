#############################################
# CÁLCULO DE INFLACIÓN INDIVIDUAL POR HOGAR #
#############################################

# Asumimos que ya tenemos cargados de códigos anteriores las tablas: 
# diseno_enigh e inflacion
# Si no, podemos cargarlas:
# diseno_enigh <- readRDS("diseno_enigh.rds")
# inflacion <- readRDS("inflacion.rds")


# Preparamos la estructura de gasto para cada hogar
estructura_gasto <- diseno_enigh$variables %>%
  select(
    clave_hogar, edo, periodo, decil, ing_cor, gasto_mon,
    prop_alimentos, prop_vesti_calz, prop_vivienda, prop_limpieza,
    prop_salud, prop_transporte, prop_educ_espa, prop_cuid_personal, prop_otros_gas
  ) %>%
  # Aseguramos que la suma de proporciones sea 1
  mutate(
    suma_props = prop_alimentos + prop_vesti_calz + prop_vivienda + prop_limpieza +
      prop_salud + prop_transporte + prop_educ_espa + prop_cuid_personal + prop_otros_gas,
    # Normalizar en caso de que no sumen exactamente 1
    across(starts_with("prop_"), ~ .x / suma_props)
  ) %>%
  select(-suma_props)

# Convertimos de formato ancho a largo para facilitar el join
estructura_gasto_largo <- estructura_gasto %>%
  pivot_longer(
    cols = starts_with("prop_"),
    names_to = "categoria",
    values_to = "proporcion"
  ) %>%
  # Convertimos los nombres de categorías para que coincidan con los de la inflación
  mutate(
    rubro = case_when(
      categoria == "prop_alimentos" ~ "inpc_alimentos",
      categoria == "prop_vesti_calz" ~ "inpc_vesti_calz",
      categoria == "prop_vivienda" ~ "inpc_vivienda",
      categoria == "prop_limpieza" ~ "inpc_limpieza",
      categoria == "prop_salud" ~ "inpc_salud",
      categoria == "prop_transporte" ~ "inpc_transporte",
      categoria == "prop_educ_espa" ~ "inpc_educ_espa",
      categoria == "prop_cuid_personal" ~ "inpc_cuid_personal",
      categoria == "prop_otros_gas" ~ "inpc_otros_gas",
      TRUE ~ "NA"
    )
  ) %>%
  select(-categoria)

# Hacemos el join directamente con la tabla de inflación original
inflacion_individual <- estructura_gasto_largo %>%
  left_join(
    inflacion,
    by = c("edo", "rubro")
  )

# Calculamos la inflación individual para cada tipo
inflacion_hogar <- inflacion_individual %>%
  # Un hogar por fila, con cada tipo de inflación ponderada
  group_by(clave_hogar, edo, periodo, decil, ing_cor, gasto_mon) %>%
  summarise(
    # Inflación acumulada 2018-2020
    inflacion_2018_2020 = sum(proporcion * inflacion_2018_2020, na.rm = TRUE),
    # Inflación acumulada 2018-2022
    inflacion_2018_2022 = sum(proporcion * inflacion_2018_2022, na.rm = TRUE),
    # TMAC 2018-2020
    tmac_2018_2020 = sum(proporcion * tmac_2018_2020, na.rm = TRUE),
    # TMAC 2018-2022
    tmac_2018_2022 = sum(proporcion * tmac_2018_2022, na.rm = TRUE),
    .groups = "drop"
  )

# Filtramos según el periodo de interés
laspeyres_hogar <- inflacion_hogar %>%
  mutate(
    # Para hogares en 2018, mantenemos todas las medidas
    inflacion_relevante = case_when(
      # periodo == 2020 ~ inflacion_2018_2020,
      periodo == 2022 ~ inflacion_2018_2022,
      TRUE ~ NA_real_
    ),
    tmac_relevante = case_when(
      # periodo == 2020 ~ tmac_2018_2020,
      periodo == 2022 ~ tmac_2018_2022,
      TRUE ~ NA_real_
    )
  )

# Guardamos en la memoria
saveRDS(laspeyres_hogar, "laspeyres_hogar.rds")

############
# Análisis #
############

# Resumen estadístico por decil de ingreso
resumen_decil <- laspeyres_hogar %>%
  filter(periodo == "2022") %>%
  group_by(periodo, decil) %>%
  summarise(
    n_hogares = n(),
    "Ingreso promedio" = mean(ing_cor, na.rm = TRUE),
    "inflación acum promedio" = round(mean(inflacion_relevante * 100, na.rm = TRUE),2),
    "tmac promedio" = round(mean(tmac_relevante * 100, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Mostramos el resumen
resumen_decil %>% 
kable(caption = "titulo") %>%
  kable_styling(full_width = FALSE)
