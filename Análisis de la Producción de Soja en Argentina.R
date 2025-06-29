# Cargar librerías necesarias
library(tidyverse)

# Importar dataset (ajusta la ruta según tu PC)
soja <- read_csv("C:/Users/JUAN/Downloads/soja-serie-1941-2023-anual.csv")

# Verificar estructura
head(soja)
str(soja)

# Visualización de la evolución de la producción de soja en Argentina (1941-2023)
ggplot(soja, aes(x = indice_tiempo, y = produccion_soja_t)) +
  geom_line(color = "#1f77b4", size = 1) +
  geom_point(color = "#ff7f0e", size = 1.5) +
  labs(
    title = "Evolución de la producción de soja en Argentina (1941-2023)",
    x = "Año",
    y = "Producción de soja (toneladas)"
  ) +
  theme_minimal()

# Guardar gráfico
ggsave("produccion_soja_argentina.png", width = 8, height = 5)

# Insight preliminar
cat("La producción de soja en Argentina muestra un crecimiento sostenido a partir de los años 90, alcanzando picos superiores a 60M de toneladas, reflejando su importancia estructural en la balanza comercial del país.\n")


