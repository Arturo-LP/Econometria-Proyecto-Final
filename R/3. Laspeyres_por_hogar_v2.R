#############################################
# CÁLCULO DE INFLACIÓN INDIVIDUAL POR HOGAR #
#############################################

# Asumimos que ya tenemos cargados de códigos anteriores las tablas: 
# "diseno_enigh" e "inflacion"
# Si no, podemos cargarlas:
# diseno_enigh <- readRDS("diseno_enigh.rds")
# inflacion <- readRDS("inflacion.rds")

library(dplyr)
library(tidyverse)
library(tidyr)

##########################
# Preparación de la base #
##########################

# Preparamos la estructura de gasto para cada hogar

# Usamos directamente todas las variables del objeto diseno_enigh
estructura_gasto <- diseno_enigh$variables %>%
  # Calculamos la suma de proporciones para verificación
  mutate(
    suma_props = prop_alimentos + prop_vesti_calz + prop_vivienda + prop_limpieza +
      prop_salud + prop_transporte + prop_educ_espa + prop_cuid_personal + prop_otros_gas
  )

# Verificamos si hay hogares con proporciones que no sumen exactamente 1
hogares_con_problemas <- estructura_gasto %>%
  filter(abs(suma_props - 1) > 0.001) %>%  # Tolerancia de 0.1%
  select(clave_hogar, suma_props)

# Mostramos advertencia si encontramos casos
if(nrow(hogares_con_problemas) > 0) {
  warning(paste("Se encontraron", nrow(hogares_con_problemas), 
                "hogares cuyas proporciones de gasto no suman exactamente 1 (+/- 0.1%)"))
  # Mostrar los primeros 5 ejemplos
  print(head(hogares_con_problemas, 5))
} else {
  message("Todo bien")
}

# Ahora solo normalizamos las proporciones para asegurar que sumen exactamente 1
estructura_gasto <- estructura_gasto %>%
  mutate(
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
  group_by(clave_hogar, edo, periodo, factor, est_dis, upm, decil, ing_cor, gasto_mon) %>%
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

# Filtramos
laspeyres_hogar <- inflacion_hogar %>%
  mutate(
    # Para hogares en 2018, mantenemos todas las medidas
    inflacion_acum = case_when(
      periodo == 2020 ~ inflacion_2018_2020,
      periodo == 2022 ~ inflacion_2018_2022,
      TRUE ~ NA_real_
    ),
    tmac = case_when(
      periodo == 2020 ~ tmac_2018_2020,
      periodo == 2022 ~ tmac_2018_2022,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-inflacion_2018_2020, -inflacion_2018_2022,
         -tmac_2018_2020, -tmac_2018_2022) %>%
  filter(periodo != "2018")

# Guardamos en la memoria
saveRDS(laspeyres_hogar, "laspeyres_hogar.rds")

############
# Análisis #
############

library(srvyr)
library(knitr)
library(kableExtra)

# Creamos un objeto de diseño para cada año
diseno_general <- laspeyres_hogar %>%
  as_survey_design(id = upm,
                   strata = est_dis,
                   weights = factor,
                   nest = TRUE)

diseno_2020 <- diseno_general %>%
  filter(periodo == "2020") %>%
                   mutate(periodo = "2020")

diseno_2022 <- diseno_general %>%
  filter(periodo == "2022") %>%
                   mutate(periodo = "2022")

# Cálculo de medias ponderadas por decil y estado
resumen_decil <- diseno_general %>%
  group_by(edo, periodo, decil) %>%
  summarise(
    ing_cor = survey_mean(ing_cor, na.rm = TRUE),
    gasto_mon = survey_mean(gasto_mon, na.rm = TRUE),
    tmac = survey_mean(tmac, na.rm = TRUE),
    inflacion_acum = survey_mean(inflacion_acum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(-ing_cor_se, -gasto_mon_se, -inflacion_acum_se, -tmac_se)

resumen_decil_2020 <- resumen_decil %>%
  group_by(decil, edo) %>%
  summarise(
    ing_cor = survey_mean(ing_cor, na.rm = TRUE),
    inflacion_acum = survey_mean(inflacion_acum, na.rm = TRUE),
    tmac = survey_mean(tmac, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(-ing_cor_se, -inflacion_acum_se, -tmac_se) %>%
  mutate(
    "Ingreso promedio" = ing_cor,
    "Inflación Acumulada Promedio" = round(inflacion_acum * 100, 2),
    "Inflación Media Anual Promedio" = round(tmac * 100, 2),
    periodo = "2020"
  ) %>%
  select(periodo, edo, decil, "Ingreso promedio", "Inflación Acumulada Promedio", "Inflación Media Anual Promedio")

resumen_decil_2022 <- diseno_2022 %>%
  group_by(decil, edo) %>%
  summarise(
    ing_cor = survey_mean(ing_cor, na.rm = TRUE),
    inflacion_acum = survey_mean(inflacion_acum, na.rm = TRUE),
    tmac = survey_mean(tmac, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(-ing_cor_se, -inflacion_acum_se, -tmac_se) %>%
  mutate(
    "Ingreso promedio" = ing_cor,
    "Inflación Acumulada Promedio" = round(inflacion_acum * 100, 2),
    "Inflación Media Anual Promedio" = round(tmac * 100, 2),
    periodo = "2022"
  ) %>%
  select(periodo, edo, decil, "Ingreso promedio", "Inflación Acumulada Promedio", "Inflación Media Anual Promedio")

# Mostramos el resumen
resumen_decil_2020 %>% 
  kable(caption = "Inflación promedio por decil (2020)") %>%
  kable_styling(full_width = FALSE)

resumen_decil_2022 %>% 
  kable(caption = "Inflación promedio por decil (2022)") %>%
  kable_styling(full_width = FALSE)


############
# Gráficos #
############

# Combinar bases
resumen_nacional <- bind_rows(resumen_decil_2020, resumen_decil_2022)

# --- Barras ---

# Inflación por decil a nivel nacional
ggplot(resumen_nacional, aes(x = factor(decil), y = `Inflación Media Anual Promedio`, fill = periodo)) +
  geom_col(position = "dodge") +
  labs(title = "Tasa de Inflación Anual Compuesta a Nivel Hogar",
       subtitle = "Promedio Nacional por Decil de Ingreso - Año Base 2018",
       x = "Decil de ingreso", y = "Inflación (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("darkblue", "red"))

# Inflación por decil y estado
ggplot(resumen_nacional, aes(x = factor(decil), y = `Inflación Media Anual Promedio`, group = periodo, color = periodo)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ edo, scales = "free_y") +
  scale_color_manual(values = c("2020" = "darkblue", "2022" = "red")) +
  labs(title = "Tasa de Inflación Anual Compuesta a Nivel Hogar",
       subtitle = "Promedio por Decil y Entidad - Año Base 2018",
       x = "Decil de ingreso", y = "Inflación (%)") +
  theme_minimal()


# --- Heatmap ---

# Datos
data_heatmap <- resumen_nacional %>%
  group_by(edo, periodo) %>%
  mutate(inflacion_escalada = scales::rescale(`Inflación Media Anual Promedio`)) %>%
  ungroup()

# Escala absoluta
ggplot(data_heatmap, aes(x = factor(decil), y = edo, fill = `Inflación Media Anual Promedio`)) +
  geom_tile() +
  facet_wrap(~ periodo) +
  scale_fill_gradient2(
    low = "blue",
    mid = "yellow",
    high = "red",
    midpoint = median(resumen_nacional$`Inflación Media Anual Promedio`, na.rm = TRUE),
    name = "Inflación (%)"
  ) +
  labs(title = "Tasa de Inflación Anual Compuesta a Nivel Hogar",
       subtitle = "Promedio por Decil y Entidad - Año Base 2018",
    x = "Decil de Ingreso", y = "Entidad Federativa"
  ) +
  theme_minimal()
  
# Escala relativa
ggplot(data_heatmap, aes(x = factor(decil), y = edo, fill = inflacion_escalada)) +
  geom_tile() +
  facet_wrap(~ periodo) +
  scale_fill_gradient2(
    low = "blue",
    mid = "yellow",
    high = "red",
    midpoint = 0.5,
    limits = c(0, 1),
    name = "Inflación\nrelativa"
  ) +
  labs(
    title = "Tasa de Inflación Anual Compuesta a Nivel Hogar (Escala Relativa)",
    subtitle = "Promedio por Decil y Entidad - Año Base 2018",
    x = "Decil de Ingreso", y = "Entidad Federativa"
  ) +
  geom_text(aes(label = round(`Inflación Media Anual Promedio`, 1)), size = 3.5, color = "white") +
  theme_minimal()

# --- Dinámica de brechas ---
ggplot(resumen_nacional, aes(x = periodo, y = `Inflación Media Anual Promedio`, group = interaction(edo, decil), color = factor(decil))) +
  geom_point() +
  geom_line(alpha = 0.5) +
  facet_wrap(~ edo) +
  scale_color_viridis_d(name = "Decil", option = "D") +
  labs(
    title = "Dinámica de la Brecha Inflacionaria por Decil de Ingreso",
    subtitle = "2020 vs 2022. Año Base 2018",
    x = "Periodo", y = "Inflación (%)"
  ) +
  theme_minimal()


# --- Boxplots por decil ---
ggplot(resumen_nacional, aes(x = factor(decil), y = `Inflación Media Anual Promedio`, fill = periodo)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("2020" = "blue", "2022" = "red"),
    name = "Periodo"
  ) +
  labs(title = "Distribución de la Inflación Anual Compuesta a Nivel Hogar",
       subtitle = "Promedio por Decil de Ingreso - Año Base 2018",
       x = "Decil de ingreso", y = "Inflación (%)") +
  theme_minimal()

# Acompañar esta gráfica ocn una tabla de estadísticas descriptivas por decil
resumen_nacional %>%
  group_by(decil, periodo) %>%
  summarise(
    Media = round(mean(`Inflación Media Anual Promedio`, na.rm = TRUE), 2),
    `Desv. estándar` = round(sd(`Inflación Media Anual Promedio`, na.rm = TRUE), 2),
    Mínimo = round(min(`Inflación Media Anual Promedio`, na.rm = TRUE), 2),
    P25 = round(quantile(`Inflación Media Anual Promedio`, 0.25, na.rm = TRUE), 2),
    Mediana = round(median(`Inflación Media Anual Promedio`, na.rm = TRUE), 2),
    P75 = round(quantile(`Inflación Media Anual Promedio`, 0.75, na.rm = TRUE), 2),
    Máximo = round(max(`Inflación Media Anual Promedio`, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  kable(caption = "Estadísticas descriptivas de inflación media anual por decil") %>%
  kable_styling(full_width = FALSE)

# --- Gasto vs Inflación ---

# Por decil
ggplot(
  laspeyres_hogar %>% 
    filter(gasto_mon != 0) %>%
    mutate(Periodo = as.factor(periodo))%>%
    sample_n(5000),
  aes(x = tmac, y = gasto_mon, color = Periodo)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ decil, scales = "free") +
  labs(title = "Gasto Monetario Trimestral e Inflación a Nivel Hogar",
       subtitle = "Decil de Ingreso - Año Base 2018",
       x = "Inflación media anual (%)",
       y = "Pesos Corrientes") +
  theme_minimal()

# Por estado
ggplot(
  laspeyres_hogar %>% 
    filter(gasto_mon != 0) %>%
    mutate(Periodo = as.factor(periodo))%>%
    sample_n(5000),
  aes(x = tmac, y = gasto_mon, color = Periodo)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ edo, scales = "free") +
  labs(title = "Gasto Monetario Trimestral e Inflación a Nivel Hogar",
       subtitle = "Entidad Federativa - Año Base 2018",
       x = "Inflación media anual (%)",
       y = "Pesos Corrientes") +
  theme_minimal()

# Por periodo
ggplot(
  laspeyres_hogar %>% 
    filter(gasto_mon != 0) %>%
    mutate(Periodo = as.factor(periodo))%>%
    sample_n(5000),
  aes(x = tmac, y = gasto_mon)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Periodo, scales = "free") +
  labs(title = "Gasto Monetario Trimestral e Inflación a Nivel Hogar",
       subtitle = "Nacional - Año Base 2018",
       x = "Inflación media anual (%)",
       y = "Pesos Corrientes") +
  theme_minimal()

# Por periodo y estado
ggplot(
  laspeyres_hogar %>% 
    filter(gasto_mon != 0) %>%
    mutate(Entidad = as.factor(edo))%>%
    sample_n(5000),
  aes(x = tmac, y = gasto_mon, color = Entidad)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ periodo, scales = "free") +
  labs(title = "Relación entre gasto monetario e inflación (muestra aleatoria)",
       x = "Inflación media anual (%)",
       y = "Gasto monetario trimestral (pesos)") +
  theme_minimal()


# Promedio estatal
ggplot(resumen_decil %>% 
         mutate(Periodo = as.factor(periodo)),
       aes(x = tmac, y = gasto_mon, color = Periodo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Gasto Promedio por Decil de Ingreso e Inflación a Nivel Hogar",
    subtitle = "Estatal - Año Base 2018",
    x = "Inflación (%)",
    y = "Pesos Corrientes"
  ) +  
  theme_minimal()

# Promedio estatal por decil
ggplot(resumen_decil %>% 
         mutate(Periodo = as.factor(periodo)),
       aes(x = tmac, y = gasto_mon, color = Periodo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ decil, scales = "free") +
  labs(
    title = "Gasto Promedio Trimestral e Inflación a Nivel Hogar",
    subtitle = "Estatal por Decil de Ingreso - Año Base 2018",
    x = "Inflación %)",
    y = "Pesos Corrientes"
  ) +
  theme_minimal()
