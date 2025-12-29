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
    xmin = ifelse(Gobierno=="Menem\n(1989-1999)", min(Año), min(Año)-.8),
    xmax = max(Año)+.2,
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
# 6. Función para crear gráficos
# -----------------------------------------------------------------------------

crear_grafico_pbi <- function(data, variable, titulo, filename_base, start_y_zero = FALSE) {
  # Preparar datos
  datos_plot <- data %>%
    select(Año, Gobierno, valor = all_of(variable))
  
  # Filtrar solo el último año de cada gobierno para las etiquetas
  datos_labels <- datos_plot %>%
    group_by(Gobierno) %>%
    filter(Año == max(Año)) %>%
    ungroup()
  
  # Crear el gráfico

  p <- ggplot() +
    # Agregar rectángulos de fondo para cada gobierno
    geom_rect(
      data = gobiernos_rect,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Gobierno),
      alpha = 0.3
    ) +
    scale_fill_manual(values = colores_gobierno, guide = "none") +
    # Agregar líneas verticales entre gobiernos
    geom_vline(
      xintercept = c(1999.2, 2001.2, 2003.2, 2007.2, 2015.2, 2019.2, 2023.2),
      linetype = "dashed",
      color = "gray50",
      alpha = 0.5
    ) +
    # Agregar línea de la serie temporal
    geom_line(
      data = datos_plot,
      aes(x = Año, y = valor),
      color = "black",
      linewidth = 1.2
    ) +
    # Agregar puntos
    geom_point(
      data = datos_plot,
      aes(x = Año, y = valor),
      color = "black",
      size = 3
    ) +
    # Agregar etiquetas con color de gobierno como fondo
    geom_label_repel(
      data = datos_labels,
      aes(x = Año, y = valor, label = sprintf("%.2f%%", valor), fill = Gobierno),
      size = 5,
      color = "black",
      label.size = 0.3,
      label.padding = unit(0.15, "lines"),
      max.overlaps = 25,
      segment.size = 0.3,
      segment.color = "black",
      segment.alpha = 0.7,
      box.padding = 0.4,
      point.padding = 0.3,
      force_pull = 0,
      direction = "y",
      show.legend = FALSE
    ) +
    # Escalas de los ejes
    scale_x_continuous(
      breaks = seq(1993, 2026, by = 2),
      limits = c(1992.5, 2026.5)
    ) +
    # Títulos y etiquetas
    labs(
      title = titulo,
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
      x = c(1996.2, 2002.2, 2011.2, 2021.2),
      y = Inf,
      label = c("Menem", "Duhalde", "CFK", "Fernández"),
      vjust = 2,
      hjust = 0.5,
      size = 4,
      fontface = "bold",
      color = "gray30"
    ) +
    annotate(
      "text",
      x = c(2000.2, 2005.2, 2017.2, 2025),
      y = Inf,
      label = c("De la Rúa", "Kirchner", "Macri", "Milei"),
      vjust = 6,
      hjust = 0.5,
      size = 4,
      fontface = "bold",
      color = "gray30"
    )
  
  # Configurar escala Y según parámetro
  if (start_y_zero) {
    p <- p + scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.15)),
      limits = c(0, NA)
    )
  } else {
    p <- p + scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.05, 0.15))
    )
  }
  
  # Guardar en PNG
  ggsave(
    paste0(filename_base, ".png"),
    plot = p,
    width = 12,
    height = 12,
    dpi = 300,
    bg = "white"
  )
  
  return(p)
}

# -----------------------------------------------------------------------------
# 6b. Función para crear gráficos de barras
# -----------------------------------------------------------------------------

crear_grafico_barras_pbi <- function(data, variable, titulo, filename_base) {
  # Preparar datos
  datos_plot <- data %>%
    select(Año, Gobierno, valor = all_of(variable))
  
  # Filtrar solo el último año de cada gobierno para las etiquetas
  datos_labels <- datos_plot %>%
    group_by(Gobierno) %>%
    filter(Año == max(Año)) %>%
    ungroup()
  
  # Crear datos para etiquetas de gobierno (posición central de cada gobierno)
  # Dividir en dos grupos para alternar posiciones verticales
  gobierno_labels_row1 <- data %>%
    group_by(Gobierno) %>%
    summarise(x_pos = mean(Año), .groups = "drop") %>%
    filter(!is.na(Gobierno)) %>%
    slice(c(1, 3, 5, 7)) %>%  # Menem, Duhalde, CFK, Fernández
    mutate(label = c("Menem", "Duhalde", "CFK", "Fernández"))
  
  gobierno_labels_row2 <- data %>%
    group_by(Gobierno) %>%
    summarise(x_pos = mean(Año), .groups = "drop") %>%
    filter(!is.na(Gobierno)) %>%
    slice(c(2, 4, 6, 8)) %>%  # De la Rúa, Kirchner, Macri, Milei
    mutate(label = c("De la Rúa", "Kirchner", "Macri", "Milei"))
  
  # Crear el gráfico
  p <- ggplot() +
    # Agregar barras con color de gobierno
    geom_col(
      data = datos_plot,
      aes(x = Año, y = valor, fill = Gobierno),
      color = "black",
      width = 0.7,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = colores_gobierno) +
    # Agregar etiquetas de datos rotadas
    geom_label(
      data = datos_labels,
      aes(x = Año, y = valor, label = sprintf("%.2f%%", valor), fill = Gobierno),
      size = 4,
      color = "black",
      fontface = "bold",
      label.size = 0.3,
      label.padding = unit(0.15, "lines"),
      vjust = 0.5,
      hjust = -0.1,
      angle = 90,
      show.legend = FALSE
    ) +
    # Agregar etiquetas de gobierno en la parte superior (fila 1)
    geom_label(
      data = gobierno_labels_row1,
      aes(x = x_pos, y = Inf, label = label, fill = Gobierno),
      vjust = 1.5,
      hjust = 0.5,
      size = 4,
      fontface = "bold",
      color = "black",
      label.size = 0.3,
      show.legend = FALSE
    ) +
    # Agregar etiquetas de gobierno en la parte superior (fila 2)
    geom_label(
      data = gobierno_labels_row2,
      aes(x = x_pos, y = Inf, label = label, fill = Gobierno),
      vjust = 3.5,
      hjust = 0.5,
      size = 4,
      fontface = "bold",
      color = "black",
      label.size = 0.3,
      show.legend = FALSE
    ) +
    # Escalas de los ejes
    scale_x_continuous(
      breaks = seq(1993, 2026, by = 2),
      limits = c(1992.5, 2026.5)
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.15)),
      limits = c(0, NA)
    ) +
    # Títulos y etiquetas
    labs(
      title = titulo,
      subtitle = "Argentina 1993-2026 | Por período de gobierno",
      x = "Año",
      y = "Porcentaje del PBI",
      caption = caption_text
    ) +
    # Aplicar tema
    tema_profesional
  
  # Guardar en PNG
  ggsave(
    paste0(filename_base, ".png"),
    plot = p,
    width = 12,
    height = 12,
    dpi = 300,
    bg = "white"
  )
  
  return(p)
}

# -----------------------------------------------------------------------------
# 7. Crear y guardar los gráficos
# -----------------------------------------------------------------------------

# Gráficos con escala Y automática
p_ciencia <- crear_grafico_pbi(
  data = presupuesto,
  variable = "Ciencia_pct_PBI",
  titulo = "Presupuesto Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "presupuesto_ciencia_pbi"
)

p_educacion <- crear_grafico_pbi(
  data = presupuesto,
  variable = "Educacion_pct_PBI",
  titulo = "Presupuesto Devengado en Educación como % del PBI",
  filename_base = "presupuesto_educacion_pbi"
)

# Gráficos con escala Y comenzando en 0
p_ciencia_y0 <- crear_grafico_pbi(
  data = presupuesto,
  variable = "Ciencia_pct_PBI",
  titulo = "Presupuesto Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "presupuesto_ciencia_pbi_y0",
  start_y_zero = TRUE
)

p_educacion_y0 <- crear_grafico_pbi(
  data = presupuesto,
  variable = "Educacion_pct_PBI",
  titulo = "Presupuesto Devengado en Educación como % del PBI",
  filename_base = "presupuesto_educacion_pbi_y0",
  start_y_zero = TRUE
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
  filename_base = "presupuesto_ciencia_pbi_bar"
)

p_educacion_bar <- crear_grafico_barras_pbi(
  data = presupuesto,
  variable = "Educacion_pct_PBI",
  titulo = "Presupuesto Devengado en Educación como % del PBI",
  filename_base = "presupuesto_educacion_pbi_bar"
)

print(p_ciencia_bar)
print(p_educacion_bar)

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
