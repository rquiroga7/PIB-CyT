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

# Cargar funciones auxiliares
source("functions.R")

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
    xmin = ifelse(Gobierno=="Menem\n(1989-1999)", min(Año), min(Año)-.8),
    xmax = max(Año)+.2,
    .groups = "drop"
  ) %>%
  filter(!is.na(Gobierno))

# -----------------------------------------------------------------------------
# 6. Definir tema y caption
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
  "PBI 1993-2004 (INDEC, cuadro8_1.xls)\nPBI 2004-2025 (INDEC, sh_oferta_demanda_12_25.xls) | ",
  "Devengados (presupuestoabierto.gob.ar/sici/datos-abiertos)\n",
  "PBI 2025-2026: estimaciones del Presupuesto Nacional 2026 | ",
  "Para 2026 se usa el presupuesto como estimación proyectada del devengado.\n",
  "Gráfico: Rodrigo Quiroga | Repositorio: github.com/rquiroga7/PIB-CyT"
)

# -----------------------------------------------------------------------------
# 7. Crear y guardar los gráficos
# -----------------------------------------------------------------------------

# Gráficos con escala Y automática
p_ciencia <- crear_grafico_pbi(
  data = presupuesto,
  variable = "Ciencia_pct_PBI",
  titulo = "Presupuesto Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "presupuesto_ciencia_pbi",
  gobiernos_rect = gobiernos_rect,
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional
)

p_educacion <- crear_grafico_pbi(
  data = presupuesto,
  variable = "Educacion_pct_PBI",
  titulo = "Presupuesto Devengado en Educación como % del PBI",
  filename_base = "presupuesto_educacion_pbi",
  gobiernos_rect = gobiernos_rect,
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional
)

# Gráficos con escala Y comenzando en 0
p_ciencia_y0 <- crear_grafico_pbi(
  data = presupuesto,
  variable = "Ciencia_pct_PBI",
  titulo = "Presupuesto Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "presupuesto_ciencia_pbi_y0",
  start_y_zero = TRUE,
  gobiernos_rect = gobiernos_rect,
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional
)

p_educacion_y0 <- crear_grafico_pbi(
  data = presupuesto,
  variable = "Educacion_pct_PBI",
  titulo = "Presupuesto Devengado en Educación como % del PBI",
  filename_base = "presupuesto_educacion_pbi_y0",
  start_y_zero = TRUE,
  gobiernos_rect = gobiernos_rect,
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional
)

# Mostrar los gráficos
print(p_ciencia)
print(p_educacion)
print(p_ciencia_y0)
print(p_educacion_y0)

# Gráficos de barras
p_ciencia_bar <- crear_grafico_barras_pbi(
  data = presupuesto,
  variable = "Ciencia_pct_PBI",
  titulo = "Presupuesto Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "presupuesto_ciencia_pbi_bar",
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional
)

p_educacion_bar <- crear_grafico_barras_pbi(
  data = presupuesto,
  variable = "Educacion_pct_PBI",
  titulo = "Presupuesto Devengado en Educación como % del PBI",
  filename_base = "presupuesto_educacion_pbi_bar",
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional
)

print(p_ciencia_bar)
print(p_educacion_bar)

# Gráficos de barras de variación interanual
p_ciencia_var <- crear_grafico_variacion_pbi(
  data = presupuesto,
  variable = "Ciencia_pct_PBI",
  titulo = "Variación Interanual del Presupuesto en Ciencia y Técnica como % del PBI",
  filename_base = "presupuesto_ciencia_pbi_variacion",
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional
)

p_educacion_var <- crear_grafico_variacion_pbi(
  data = presupuesto,
  variable = "Educacion_pct_PBI",
  titulo = "Variación Interanual del Presupuesto en Educación como % del PBI",
  filename_base = "presupuesto_educacion_pbi_variacion",
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional
)

print(p_ciencia_var)
print(p_educacion_var)

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
