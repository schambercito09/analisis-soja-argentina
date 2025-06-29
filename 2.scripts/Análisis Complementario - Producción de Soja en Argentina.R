# ------------------------------------------------------------
# Análisis Complementario - Producción de Soja en Argentina
# Autor: Juan Ignacio Schamberger
# Descripción: Análisis riguroso para GitHub y LinkedIn
# ------------------------------------------------------------
setwd("C:/Users/JUAN/Desktop/Proyecto/")

library(tidyverse)

#  Cargar datos
df <- read_csv("C:/Users/JUAN/Desktop/Proyecto/soja-serie-1941-2023-anual.csv")

# 1 Evolución superficie sembrada vs cosechada
df_long <- df %>% 
  select(indice_tiempo, superficie_sembrada_soja_ha, superficie_cosechada_soja_ha) %>%
  pivot_longer(cols = -indice_tiempo, names_to = "variable", values_to = "hectareas")

ggplot(df_long, aes(x = indice_tiempo, y = hectareas, color = variable)) +
  geom_line(size = 1) +
  labs(title = "Evolución Superficie Sembrada vs Cosechada de Soja en Argentina (1941-2023)",
       scale_color_manual(values = c("#E63946", "#457B9D")),
       geom_line(size = 1.2),
       x = "Año", y = "Hectáreas", color = "Variable") +
  theme_minimal()
ggsave("outputs/superficie_sembrada_vs_cosechada.png", width = 8, height = 5, bg = "white")


# 2 Evolución del rendimiento (kg/ha)
ggplot(df, aes(x = indice_tiempo, y = rendimiento_soja_kgxha)) +
  geom_line(color = "#1f77b4", size = 1) +
  labs(title = "Evolución del Rendimiento de Soja en Argentina (kg/ha)",
       x = "Año", y = "Rendimiento (kg/ha)") +
  theme_minimal()
ggsave("outputs/rendimiento_soja.png", width = 8, height = 5, bg = "white")

# 3 Relación superficie cosechada vs producción
ggplot(df, aes(x = superficie_cosechada_soja_ha, y = produccion_soja_t)) +
  geom_point(color = "#ff7f0e", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#1f77b4") +
  labs(title = "Relación Superficie Cosechada vs Producción de Soja en Argentina",
       x = "Superficie Cosechada (ha)", y = "Producción (toneladas)") +
  theme_minimal()
ggsave("outputs/superficie_vs_produccion.png", width = 8, height = 5, bg = "white")

# 4 Tasa de crecimiento interanual de producción
df <- df %>% arrange(indice_tiempo) %>%
  mutate(tasa_crecimiento = (produccion_soja_t - lag(produccion_soja_t)) / lag(produccion_soja_t) * 100)

ggplot(df, aes(x = indice_tiempo, y = tasa_crecimiento)) +
  geom_col(fill = "#1f77b4") +
  labs(title = "Tasa de Crecimiento Interanual de la Producción de Soja en Argentina",
       x = "Año", y = "Tasa de crecimiento (%)") +
  theme_minimal()
ggsave("outputs/tasa_crecimiento_produccion.png", width = 8, height = 5, bg = "white")

# 5 Correlaciones
cor_prod_sup <- cor(df$produccion_soja_t, df$superficie_cosechada_soja_ha, use = "complete.obs")
cor_prod_rend <- cor(df$produccion_soja_t, df$rendimiento_soja_kgxha, use = "complete.obs")

cat("\nCorrelación Producción - Superficie Cosechada:", round(cor_prod_sup, 3))
#Correlación Producción - Superficie Cosechada: 0.983
cat("\nCorrelación Producción - Rendimiento:", round(cor_prod_rend, 3))
#Correlación Producción - Rendimiento: 0.898

# 6 Promedios por década
df_decada <- df %>%
  mutate(decada = paste0(substr(indice_tiempo, 1, 3), "0s")) %>%
  group_by(decada) %>%
  summarise(
    promedio_produccion_t = mean(produccion_soja_t, na.rm = TRUE),
    promedio_superficie_ha = mean(superficie_cosechada_soja_ha, na.rm = TRUE),
    promedio_rendimiento_kgxha = mean(rendimiento_soja_kgxha, na.rm = TRUE)
  )

print(df_decada)

# Guardar tabla como CSV para el repo
write_csv(df_decada, "outputs/promedios_por_decada.csv")

# ------------------------------------------------------------
# Este análisis complementario robustece tu portfolio,
# dejando material profesional para LinkedIn y GitHub.
# ------------------------------------------------------------
