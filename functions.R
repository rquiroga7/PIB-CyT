# =============================================================================
# Funciones auxiliares para análisis del presupuesto
# =============================================================================

# -----------------------------------------------------------------------------
# Función para definir períodos de gobierno
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

# -----------------------------------------------------------------------------
# Función para crear gráficos de línea
# -----------------------------------------------------------------------------

crear_grafico_pbi <- function(data, variable, titulo, filename_base, start_y_zero = FALSE,
                               gobiernos_rect, colores_gobierno, caption_text, tema_profesional) {
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
# Función para crear gráficos de barras
# -----------------------------------------------------------------------------

crear_grafico_barras_pbi <- function(data, variable, titulo, filename_base,
                                      colores_gobierno, caption_text, tema_profesional) {
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
      vjust = 1.2,
      hjust = 0.5,
      size = 5,
      fontface = "bold",
      color = "black",
      label.size = 0.3,
      show.legend = FALSE
    ) +
    # Agregar etiquetas de gobierno en la parte superior (fila 2)
    geom_label(
      data = gobierno_labels_row2,
      aes(x = x_pos, y = Inf, label = label, fill = Gobierno),
      vjust = 2.8,
      hjust = 0.5,
      size = 5,
      fontface = "bold",
      color = "black",
      label.size = 0.3,
      show.legend = FALSE
    ) +
    # Escalas de los ejes
    scale_x_continuous(
      breaks = seq(1993, 2026, by = 1),
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
# Función para crear gráficos de barras de variación interanual
# -----------------------------------------------------------------------------

crear_grafico_variacion_pbi <- function(data, variable, titulo, filename_base,
                                         colores_gobierno, caption_text, tema_profesional) {
  # Preparar datos y calcular variación interanual
  datos_plot <- data %>%
    select(Año, Gobierno, valor = all_of(variable)) %>%
    arrange(Año) %>%
    mutate(
      variacion = valor - lag(valor),
      variacion_pct = (valor / lag(valor) - 1) * 100
    ) %>%
    filter(!is.na(variacion))  # Remover el primer año sin variación
  
  # Crear datos para etiquetas de gobierno (posición central de cada gobierno)
  # Dividir en dos grupos para alternar posiciones verticales
  gobierno_labels_row1 <- datos_plot %>%
    group_by(Gobierno) %>%
    summarise(x_pos = mean(Año), .groups = "drop") %>%
    filter(!is.na(Gobierno)) %>%
    slice(c(1, 3, 5, 7)) %>%
    mutate(label = c("Menem", "Duhalde", "CFK", "Fernández"))
  
  gobierno_labels_row2 <- datos_plot %>%
    group_by(Gobierno) %>%
    summarise(x_pos = mean(Año), .groups = "drop") %>%
    filter(!is.na(Gobierno)) %>%
    slice(c(2, 4, 6, 8)) %>%
    mutate(label = c("De la Rúa", "Kirchner", "Macri", "Milei"))
  
  # Crear el gráfico
  p <- ggplot() +
    # Agregar línea horizontal en y=0
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.8) +
    # Agregar barras con color de gobierno
    geom_col(
      data = datos_plot,
      aes(x = Año, y = variacion, fill = Gobierno),
      color = "black",
      width = 0.7,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = colores_gobierno) +
    # Agregar etiquetas de gobierno en la parte superior (fila 1)
    geom_label(
      data = gobierno_labels_row1,
      aes(x = x_pos, y = Inf, label = label, fill = Gobierno),
      vjust = 1.2,
      hjust = 0.5,
      size = 5,
      fontface = "bold",
      color = "black",
      label.size = 0.3,
      show.legend = FALSE
    ) +
    # Agregar etiquetas de gobierno en la parte superior (fila 2)
    geom_label(
      data = gobierno_labels_row2,
      aes(x = x_pos, y = Inf, label = label, fill = Gobierno),
      vjust = 2.8,
      hjust = 0.5,
      size = 5,
      fontface = "bold",
      color = "black",
      label.size = 0.3,
      show.legend = FALSE
    ) +
    # Escalas de los ejes
    scale_x_continuous(
      breaks = seq(1994, 2026, by = 1),
      limits = c(1993.5, 2026.5)
    ) +
    scale_y_continuous(
      labels = function(x) paste0(ifelse(x > 0, "+", ""), sprintf("%.2f", x), " pp"),
      expand = expansion(mult = c(0.15, 0.15))
    ) +
    # Títulos y etiquetas
    labs(
      title = titulo,
      subtitle = "Argentina 1994-2026 | Variación interanual en puntos porcentuales del PBI",
      x = "Año",
      y = "Variación interanual (puntos porcentuales)",
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
