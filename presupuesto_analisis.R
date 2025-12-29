# =============================================================================
# Análisis del Presupuesto Devengado en Ciencia y Educación como % del PBI
# Argentina 1993-2026
# =============================================================================

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggrepel)

# -----------------------------------------------------------------------------
# 1. Leer y procesar datos
# -----------------------------------------------------------------------------

presupuesto <- read.csv("presupuesto.csv", stringsAsFactors = FALSE)

# Calcular Presupuesto Devengado como % del PBI
presupuesto <- presupuesto %>%
  mutate(
    Ciencia_pct_PBI = (Devengado.Ciencia / PBI) * 100,
    Educacion_pct_PBI = (Devengado.Educación / PBI) * 100
  )

# -----------------------------------------------------------------------------
# 2. Definir períodos de gobierno
# -----------------------------------------------------------------------------

definir_gobierno <- function(año) {
  case_when(
    año >= 1989 & año <= 1999 ~ "Menem\n(1989-1999)",
    año >= 2000 & año <= 2001 ~ "De la Rúa\n(2000-2001)",
    año == 2002 ~ "Duhalde\n(2002-2003)",
    año == 2003 ~ "Duhalde\n(2002-2003)",
    año >= 2004 & año <= 2007 ~ "Kirchner\n(2003-2007)",
    año >= 2008 & año <= 2015 ~ "Fernández de Kirchner\n(2008-2015)",
    año >= 2016 & año <= 2019 ~ "Macri\n(2016-2019)",
    año >= 2020 & año <= 2023 ~ "Fernández\n(2020-2023)",
    año >= 2024 & año <= 2027 ~ "Milei\n(2024-2027)",
    TRUE ~ "Otro"
  )
}

presupuesto <- presupuesto %>%
  mutate(Gobierno = definir_gobierno(Año))

# Ordenar los gobiernos cronológicamente
presupuesto$Gobierno <- factor(presupuesto$Gobierno, levels = c(
  "Menem\n(1989-1999)",
  "De la Rúa\n(2000-2001)",
  "Duhalde\n(2002-2003)",
  "Kirchner\n(2003-2007)",
  "Fernández de Kirchner\n(2008-2015)",
  "Macri\n(2016-2019)",
  "Fernández\n(2020-2023)",
  "Milei\n(2024-2027)"
))

# -----------------------------------------------------------------------------
# 3. Preparar datos para el gráfico
# -----------------------------------------------------------------------------

# Transformar a formato largo para ggplot
datos_largo <- presupuesto %>%
  select(Año, Gobierno, Ciencia_pct_PBI, Educacion_pct_PBI) %>%
  pivot_longer(
    cols = c(Ciencia_pct_PBI, Educacion_pct_PBI),
    names_to = "Categoria",
    values_to = "Porcentaje_PBI"
  ) %>%
  mutate(
    Categoria = recode(Categoria,
      "Ciencia_pct_PBI" = "Ciencia y Técnica",
      "Educacion_pct_PBI" = "Educación"
    )
  )

# -----------------------------------------------------------------------------
# 4. Definir colores para cada gobierno
# -----------------------------------------------------------------------------

colores_gobierno <- c(
  "Menem\n(1989-1999)" = "#d2b0ff",
  "De la Rúa\n(2000-2001)" = "#E6E6FA",
  "Duhalde\n(2002-2003)" = "#7df566",
  "Kirchner\n(2003-2007)" = "#87CEEB",
  "Fernández de Kirchner\n(2008-2015)" = "#8df3fa",
  "Macri\n(2016-2019)" = "#fffc5e",
  "Fernández\n(2020-2023)" = "#B0E0E6",
  "Milei\n(2024-2027)" = "#DDA0DD"
)

# -----------------------------------------------------------------------------
# 5. Crear rectángulos de fondo para cada gobierno
# -----------------------------------------------------------------------------

# Obtener los límites de años para cada gobierno
gobiernos_rect <- presupuesto %>%
  group_by(Gobierno) %>%
  summarise(
    xmin = ifelse(Gobierno=="Menem\n(1989-1999)", min(Año), min(Año)-.5),
    xmax = max(Año)+.5,
    .groups = "drop"
  ) %>%
  filter(!is.na(Gobierno))

# -----------------------------------------------------------------------------
# 6. Crear el gráfico
# -----------------------------------------------------------------------------

# Definir tema profesional
tema_profesional <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0, margin = margin(t = 15), lineheight = 1.3),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Definir la nota al pie con fuentes
caption_text <- paste0(
  "Fuentes: Presupuesto histórico (economia.gob.ar/onp/documentos/series/Serie6506.pdf) | ",
  "PBI 1993-2004 (INDEC, cuadro8_1.xls) | PBI 2004-2025 (INDEC, sh_oferta_demanda_12_25.xls)\n",
  "Devengados (presupuestoabierto.gob.ar/sici/datos-abiertos) | ",
  "Nota: PBI 2025-2026 corresponde a estimaciones del Presupuesto Nacional 2026"
)

# -----------------------------------------------------------------------------
# 6a. Gráfico de CIENCIA
# -----------------------------------------------------------------------------

datos_ciencia <- presupuesto %>%
  select(Año, Gobierno, Ciencia_pct_PBI)

p_ciencia <- ggplot() +
  # Agregar rectángulos de fondo para cada gobierno
  geom_rect(
    data = gobiernos_rect,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Gobierno),
    alpha = 0.3
  ) +
  scale_fill_manual(values = colores_gobierno, guide = "none") +
  # Agregar líneas verticales entre gobiernos
  geom_vline(
    xintercept = c(1999.5, 2001.5, 2003.5, 2007.5, 2015.5, 2019.5, 2023.5),
    linetype = "dashed",
    color = "gray50",
    alpha = 0.5
  ) +
  # Agregar línea de la serie temporal
  geom_line(
    data = datos_ciencia,
    aes(x = Año, y = Ciencia_pct_PBI),
    color = "#2E86AB",
    linewidth = 1.2
  ) +
  # Agregar puntos
  geom_point(
    data = datos_ciencia,
    aes(x = Año, y = Ciencia_pct_PBI),
    color = "#2E86AB",
    size = 3
  ) +
  # Agregar etiquetas con contorno blanco
  geom_label_repel(
    data = datos_ciencia,
    aes(x = Año, y = Ciencia_pct_PBI, label = sprintf("%.2f%%", Ciencia_pct_PBI)),
    size = 5,
    color = "#2E86AB",
    fill = "white",
    label.size = 0.2,
    label.padding = unit(0.15, "lines"),
    max.overlaps = 25,
    segment.size = 0.3,
    segment.alpha = 0.5,
    box.padding = 0.4,
    point.padding = 0.3,
    force_pull = 0,
    direction = "y"
  ) +
  # Escalas de los ejes
  scale_x_continuous(
    breaks = seq(1993, 2026, by = 2),
    limits = c(1992.5, 2026.5)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  # Títulos y etiquetas
  labs(
    title = "Presupuesto Devengado en Ciencia y Técnica como % del PBI",
    subtitle = "Argentina 1993-2026 | Por período de gobierno",
    x = "Año",
    y = "Porcentaje del PBI",
    caption = caption_text
  ) +
  # Aplicar tema
  tema_profesional +
  # Agregar etiquetas de gobierno en la parte superior (alternando posiciones y)
  annotate(
    "text",
    x = c(1996, 2002, 2011, 2021),
    y = Inf,
    label = c("Menem", "Duhalde", "CFK", "Fernández"),
    vjust = 2,
    hjust=0.5,
    size = 4,
    fontface = "bold",
    color = "gray30"
  ) +
  annotate(
    "text",
    x = c(2000, 2005, 2017, 2024.5),
    y = Inf,
    label = c("De la Rúa", "Kirchner", "Macri", "Milei"),
    vjust = 4,
    hjust=0.5,
    size = 4,
    fontface = "bold",
    color = "gray30"
  )

# -----------------------------------------------------------------------------
# 6b. Gráfico de EDUCACIÓN
# -----------------------------------------------------------------------------

datos_educacion <- presupuesto %>%
  select(Año, Gobierno, Educacion_pct_PBI)

p_educacion <- ggplot() +
  # Agregar rectángulos de fondo para cada gobierno
  geom_rect(
    data = gobiernos_rect,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Gobierno),
    alpha = 0.3
  ) +
  scale_fill_manual(values = colores_gobierno, guide = "none") +
  # Agregar líneas verticales entre gobiernos
  geom_vline(
    xintercept = c(1999.5, 2001.5, 2003.5, 2007.5, 2015.5, 2019.5, 2023.5),
    linetype = "dashed",
    color = "gray50",
    alpha = 0.5
  ) +
  # Agregar línea de la serie temporal
  geom_line(
    data = datos_educacion,
    aes(x = Año, y = Educacion_pct_PBI),
    color = "#A23B72",
    linewidth = 1.2
  ) +
  # Agregar puntos
  geom_point(
    data = datos_educacion,
    aes(x = Año, y = Educacion_pct_PBI),
    color = "#A23B72",
    size = 4
  ) +
  # Agregar etiquetas con contorno blanco
  geom_label_repel(
    data = datos_educacion,
    aes(x = Año, y = Educacion_pct_PBI, label = sprintf("%.2f%%", Educacion_pct_PBI)),
    size = 5,
    color = "#A23B72",
    fill = "white",
    label.size = 0.2,
    label.padding = unit(0.15, "lines"),
    max.overlaps = 25,
    segment.size = 0.3,
    segment.alpha = 0.5,
    box.padding = 0.4,
    point.padding = 0.3,
    force_pull = 0,
    direction = "y"
  ) +
  # Escalas de los ejes
  scale_x_continuous(
    breaks = seq(1993, 2026, by = 2),
    limits = c(1992.5, 2026.5)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  # Títulos y etiquetas
  labs(
    title = "Presupuesto Devengado en Educación como % del PBI",
    subtitle = "Argentina 1993-2026 | Por período de gobierno",
    x = "Año",
    y = "Porcentaje del PBI",
    caption = caption_text
  ) +
  # Aplicar tema
  tema_profesional +
  # Agregar etiquetas de gobierno en la parte superior (alternando posiciones y)
  annotate(
    "text",
    x = c(1996, 2002, 2011, 2021),
    y = Inf,
    label = c("Menem", "Duhalde", "CFK", "Fernández"),
    vjust = 2,
    hjust=0.5,
    size = 4,
    fontface = "bold",
    color = "gray30"
  ) +
  annotate(
    "text",
    x = c(2000, 2005, 2017, 2024.5),
    y = Inf,
    label = c("De la Rúa", "Kirchner", "Macri", "Milei"),
    vjust = 4,
    hjust=0.5,
    size = 4,
    fontface = "bold",
    color = "gray30"
  )

# -----------------------------------------------------------------------------
# 7. Guardar los gráficos
# -----------------------------------------------------------------------------

# Guardar gráfico de CIENCIA en PNG de alta resolución
ggsave(
  "presupuesto_ciencia_pbi.png",
  plot = p_ciencia,
  width = 12,
  height = 12,
  dpi = 300,
  bg = "white"
)

# Guardar gráfico de CIENCIA en PDF para publicación
ggsave(
  "presupuesto_ciencia_pbi.pdf",
  plot = p_ciencia,
  width = 12,
  height = 12,
  bg = "white"
)

# Guardar gráfico de EDUCACIÓN en PNG de alta resolución
ggsave(
  "presupuesto_educacion_pbi.png",
  plot = p_educacion,
  width = 12,
  height = 12,
  dpi = 300,
  bg = "white"
)

# Guardar gráfico de EDUCACIÓN en PDF para publicación
ggsave(
  "presupuesto_educacion_pbi.pdf",
  plot = p_educacion,
  width = 12,
  height = 12,
  bg = "white"
)

# Mostrar los gráficos
print(p_ciencia)
print(p_educacion)

# -----------------------------------------------------------------------------
# 8. Imprimir resumen de datos
# -----------------------------------------------------------------------------

cat("\n=== Resumen: Presupuesto Devengado como % del PBI ===\n\n")

resumen <- presupuesto %>%
  select(Año, Gobierno, Ciencia_pct_PBI, Educacion_pct_PBI) %>%
  mutate(
    Ciencia_pct_PBI = round(Ciencia_pct_PBI, 4),
    Educacion_pct_PBI = round(Educacion_pct_PBI, 4)
  )

print(resumen, n = nrow(resumen))

cat("\n=== Promedio por Gobierno ===\n\n")

promedio_gobierno <- presupuesto %>%
  group_by(Gobierno) %>%
  summarise(
    Años = paste(min(Año), "-", max(Año)),
    Promedio_Ciencia = round(mean(Ciencia_pct_PBI), 4),
    Promedio_Educacion = round(mean(Educacion_pct_PBI), 4),
    .groups = "drop"
  )

print(promedio_gobierno)
