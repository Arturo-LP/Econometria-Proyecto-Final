library(quantreg)
library(tidyverse)
library(ggplot2)

# Asumimos que ya tenemos cargado del código anterior:
# laspeyres_hogar
# Si no, podemos cargarlo:
# laspeyres_hogar <- readRDS("laspeyres_hogar.rds")

#########################################
# Regresión Cuantil: Inflación vs Gasto #
#########################################

# Preparamos los datos para la regresión
datos_regresion <- laspeyres_hogar %>%
  # Filtrar valores no válidos
  filter(!is.na(tmac_relevante) & !is.na(gasto_mon) & gasto_mon > 0) %>%
  # Transformar el gasto logs para controlar la dispersión
  mutate(log_gasto = log(gasto_mon))

# Ajustamos regresiones cuantiles para diferentes cuantiles
cuantiles <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
regresiones_cuantil <- list()

for (tau in cuantiles) {
  regresiones_cuantil[[as.character(tau)]] <- rq(log_gasto ~ tmac_relevante, 
                                             data = datos_regresion, 
                                             tau = tau)
}

# Modelo de regresión lineal ordinaria (OLS) como referencia
modelo_ols <- lm(log_gasto ~ tmac_relevante, data = datos_regresion)

# Resumen de resultados
resumen_modelos <- data.frame(
  Cuantil = c("OLS (Media Condicional)", as.character(cuantiles)),
  Intercepto = c(coef(modelo_ols)[1], 
                 sapply(regresiones_cuantil, function(m) coef(m)[1])),
  Pendiente = c(coef(modelo_ols)[2], 
                sapply(regresiones_cuantil, function(m) coef(m)[2]))
)

print(resumen_modelos)

# Gráfico de coeficientes por cuantil
plot_coefs <- data.frame(
  tau =cuantiles,
  pendiente = sapply(regresiones_cuantil, function(m) coef(m)[2])
)

ggplot(plot_coefs, aes(x = tau, y = pendiente)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = coef(modelo_ols)[2], linetype = "dashed", color = "red") +
  labs(
    title = "Coeficientes de regresión cuantil: Gasto vs Inflación",
    subtitle = "La línea punteada representa el coeficiente de OLS",
    x = "Cuantil",
    y = "Pendiente (impacto de la inflación en log_gasto)"
  ) +
  theme_minimal()

# Visualización de las líneas de regresión por cuantil
# Crear un grid para predicciones
grid <- with(datos_regresion, 
                   data.frame(tmac_relevante = seq(min(tmac_relevante), 
                                              max(tmac_relevante), 
                                              length.out = 100)))

# Predicciones por cuantil
predicciones <- lapply(regresiones_cuantil, function(m) {
  pred <- predict(m, newdata = grid)
  data.frame(tmac_relevante = grid$tmac_relevante, prediccion = pred)
})

# Convertir a un solo dataframe para ggplot
predicciones_df <- bind_rows(predicciones, .id = "cuantil")
predicciones_df$cuantil <- factor(predicciones_df$cuantil, 
                                  levels = as.character(cuantiles))

# Predicción OLS
prediccion_ols <- predict(modelo_ols, newdata = grid)
prediccion_ols_df <- data.frame(tmac_relevante = grid$tmac_relevante, 
                                prediccion = prediccion_ols,
                                cuantil = "OLS")

# Combinar todas las predicciones
todas_predicciones <- bind_rows(
  predicciones_df,
  prediccion_ols_df %>% mutate(cuantil = "OLS")
)

# Crear gráfico 
ggplot() +
  # Puntos de datos originales (muestra aleatoria para no saturar el gráfico)
  geom_point(data = sample_n(datos_regresion, 10000), 
             aes(x = tmac_relevante, y = log_gasto), 
             alpha = 0.2) +
  # Líneas de regresión por cuantil
  geom_line(data = todas_predicciones, 
            aes(x = tmac_relevante, y = prediccion, color = cuantil, 
                linetype = ifelse(cuantil == "OLS", "OLS", "Cuantil")),
            size = 1) +
  # Etiquetas y tema
  labs(
    title = "Regresión Cuantil: Gasto Monetario vs Inflación",
    subtitle = "Relación entre la tasa de inflación y el logaritmo del gasto",
    x = "Tasa de Inflación",
    y = "Logaritmo del Gasto Monetario",
    color = "Cuantil",
    linetype = "Tipo"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_linetype_manual(values = c("Cuantil" = "solid", "OLS" = "dashed")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Análisis por decil de ingreso
analisis_decil <- datos_regresion %>%
  group_by(decil) %>%
  summarise(
    n = n(),
    gasto_medio = mean(gasto_mon, na.rm = TRUE),
    inflacion_media = mean(tmac_relevante, na.rm = TRUE),
    inflacion_mediana = median(tmac_relevante, na.rm = TRUE),
    inflacion_p25 = quantile(tmac_relevante, 0.25, na.rm = TRUE),
    inflacion_p75 = quantile(tmac_relevante, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Gráfico de inflación por decil
ggplot(analisis_decil, aes(x = factor(decil), y = inflacion_media)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = inflacion_p25, ymax = inflacion_p75), width = 0.3) +
  labs(
    title = "Inflación promedio por decil de ingreso",
    subtitle = "Las barras de error representan los cuartiles 25 y 75",
    x = "Decil de ingreso",
    y = "Tasa de inflación"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
