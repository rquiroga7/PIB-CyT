# =============================================================================
# Devengado en Ciencia y Educación como % del PBI
# Argentina 1995-2025
#
# A diferencia de presupuesto_analisis.R (que usa la serie anual ya armada en
# presupuesto.csv), acá el devengado se obtiene directamente de la API de
# Presupuesto Abierto (get_devengado.sh) con desagregación MENSUAL y por
# finalidad/función:
#
#   * finalidad 3, función 4 -> Educación y Cultura
#   * finalidad 3, función 5 -> Ciencia, Tecnología e Innovación
#
# La clasificación por función incluye automáticamente a todos los organismos
# descentralizados (CONICET, INTA, INTI, CNEA, CONAE, universidades, etc.).
#
# El % del PBI se calcula dividiendo el devengado de cada mes por el PBI mensual
# interpolado y promediando los 12 meses:
#
#       % PBI del año = (1/12) * sum_m  devengado_m / PBI_m
#
# Si el PBI mensual fuese uniforme (PBI/12), esto coincide exactamente con
# devengado anual / PBI anual, que es lo que permite validar contra
# presupuesto.csv.
# =============================================================================

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggrepel)
library(jsonlite)
library(readxl)

# Cargar funciones auxiliares
source("functions.R")

FUNC_EDUCACION <- 4
FUNC_CIENCIA   <- 5

# -----------------------------------------------------------------------------
# 1. Devengado mensual por función (datos scrapeados de la API)
# -----------------------------------------------------------------------------

dev_mensual <- fromJSON("devengado_ciencia_educacion.json") %>%
  as.data.frame() %>%
  filter(funcion_id %in% c(FUNC_EDUCACION, FUNC_CIENCIA)) %>%
  transmute(
    anio = impacto_presupuestario_anio,
    mes  = impacto_presupuestario_mes,
    funcion_id,
    devengado = credito_devengado
  ) %>%
  group_by(anio, mes, funcion_id) %>%
  summarise(devengado = sum(devengado, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# 2. PBI trimestral nominal (INDEC, cuadro 8 de sh_oferta_demanda_12_25.xls)
# -----------------------------------------------------------------------------
# En el cuadro las columnas son trimestres consecutivos: [T1 T2 T3 T4 Total],
# con 6 columnas por año. La fila 4 tiene el año y la fila 7 el PIB.

raw <- read_excel("sh_oferta_demanda_12_25.xls", sheet = "cuadro 8",
                  col_names = FALSE, .name_repair = "minimal")

YEAR_ROW <- 4
PIB_ROW  <- 7
FIRST_YEAR_COL <- 2
YEAR_STEP <- 6

trimestral <- do.call(rbind, lapply(
  seq(FIRST_YEAR_COL, ncol(raw), by = YEAR_STEP),
  function(c0) {
    yv <- raw[[c0]][YEAR_ROW]
    if (is.na(yv)) return(NULL)
    anio <- as.integer(sub("^(\\d{4}).*", "\\1", as.character(yv)))
    data.frame(
      anio = anio,
      trimestre = 1:4,
      pib = vapply(0:3, function(k) {
        if (c0 + k > ncol(raw)) return(NA_real_)
        v <- suppressWarnings(as.numeric(raw[[c0 + k]][PIB_ROW]))
        if (length(v) == 0) NA_real_ else v
      }, numeric(1))
    )
  }
))
trimestral <- trimestral %>% filter(!is.na(pib))

# PBI anual de referencia (presupuesto.csv): años previos a 2004 y 2025
presupuesto <- read.csv("presupuesto.csv", stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 3. PBI mensual interpolado
# -----------------------------------------------------------------------------
#  * 2004-2024: el PBI anual (promedio de trimestres) se reparte entre los meses
#    según la participación de cada trimestre. Como el PBI anual es el promedio
#    de los 4 trimestres, esto equivale a asignar a cada mes un tercio de un
#    cuarto del valor trimestral:  PBI_mes = PBI_trimestre / 12.
#  * 2025: el cuarto trimestre aún no está publicado; se extrapola usando la
#    variación T4/T3 de 2024. Se escala al PBI anual de presupuesto.csv.
#  * 1995-2003: no hay datos trimestrales -> PBI anual / 12.

pbi_anual_mensual <- function(anio, trim_vals, pbi_anual) {
  share <- trim_vals / sum(trim_vals)
  data.frame(anio = anio, mes = 1:12, pib = pbi_anual * rep(share, each = 3) / 3)
}

pbi_mensual <- lapply(sort(unique(trimestral$anio)), function(anio) {
  tvals <- trimestral %>%
    filter(anio == !!anio) %>%
    arrange(trimestre) %>%
    pull(pib)

  if (length(tvals) == 4) {
    pbi_anual_mensual(anio, tvals, sum(tvals) / 4)
  } else if (length(tvals) == 3) {
    # Extrapolación del T4 faltante con el crecimiento T4/T3 del año previo
    q3_prev <- trimestral$pib[trimestral$anio == anio - 1 & trimestral$trimestre == 3]
    q4_prev <- trimestral$pib[trimestral$anio == anio - 1 & trimestral$trimestre == 4]
    t4_est <- tvals[3] * q4_prev / q3_prev
    pbi_anual <- presupuesto$PBI[presupuesto$Año == anio]
    pbi_anual_mensual(anio, c(tvals, t4_est), pbi_anual)
  } else {
    NULL
  }
})
pbi_mensual <- bind_rows(pbi_mensual)

pbi_mensual_viejo <- presupuesto %>%
  filter(Año >= 1995, Año <= 2003) %>%
  group_by(Año) %>%
  summarise(data = list(data.frame(mes = 1:12, pib = PBI / 12)), .groups = "drop") %>%
  unnest(data) %>%
  transmute(anio = Año, mes, pib)

pbi_mensual <- bind_rows(pbi_mensual_viejo, pbi_mensual)

# 2026: aún no hay datos trimestrales; se usa el PBI anual del Presupuesto 2026 / 12.
pbi_mensual <- bind_rows(
  pbi_mensual,
  data.frame(anio = 2026, mes = 1:12,
             pib = presupuesto$PBI[presupuesto$Año == 2026] / 12)
)

# -----------------------------------------------------------------------------
# 4. Cálculo del % del PBI anual
# -----------------------------------------------------------------------------

datos_mensuales <- dev_mensual %>%
  left_join(pbi_mensual, by = c("anio", "mes")) %>%
  # El mes en curso está incompleto: se excluye para no sesgar el promedio.
  filter(!(anio == as.integer(format(Sys.Date(), "%Y")) &
             mes >= as.integer(format(Sys.Date(), "%m")))) %>%
  mutate(pct_mensual = devengado / pib * 100)

devengado_anual <- datos_mensuales %>%
  group_by(anio) %>%
  summarise(
    Meses = n_distinct(mes),
    PBI = 12 * mean(pib[funcion_id == FUNC_CIENCIA]),
    Ciencia_devengado = 12 * mean(devengado[funcion_id == FUNC_CIENCIA]),
    Educacion_devengado = 12 * mean(devengado[funcion_id == FUNC_EDUCACION]),
    Ciencia_pct_PBI = mean(pct_mensual[funcion_id == FUNC_CIENCIA]),
    Educacion_pct_PBI = mean(pct_mensual[funcion_id == FUNC_EDUCACION]),
    .groups = "drop"
  ) %>%
  filter(!is.na(Ciencia_pct_PBI), !is.na(Educacion_pct_PBI)) %>%
  arrange(anio) %>%
  rename(Año = anio)

# -----------------------------------------------------------------------------
# 5. Períodos de gobierno
# -----------------------------------------------------------------------------

devengado_anual <- devengado_anual %>%
  mutate(Gobierno = definir_gobierno(Año))

devengado_anual$Gobierno <- factor(devengado_anual$Gobierno, levels = c(
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
# 6. Colores y rectángulos de fondo por gobierno
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

gobiernos_rect <- devengado_anual %>%
  group_by(Gobierno) %>%
  summarise(
    xmin = ifelse(cur_group()$Gobierno == "Menem\n(1989-1999)", min(Año), min(Año) - 0.8),
    xmax = max(Año) + 0.2,
    .groups = "drop"
  ) %>%
  filter(!is.na(Gobierno))

# -----------------------------------------------------------------------------
# 7. Tema y caption
# -----------------------------------------------------------------------------

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

caption_text <- paste0(
  "Fuentes: Devengado mensual (presupuestoabierto.gob.ar API v1/credito: finalidad 3, funciones 4 y 5) | ",
  "PBI trimestral nominal 2004-2025 (INDEC, sh_oferta_demanda_12_25.xls, cuadro 8)\n",
  "PBI 1995-2003: serie anual INDEC | % del PBI = promedio mensual de (devengado / PBI mensual interpolado).\n",
  "2025: T4 del PBI extrapolado con la variación T4/T3 de 2024. 2026: devengado estimado como promedio mensual × 12 (año en curso).\n",
  "Gráfico: Rodrigo Quiroga | Repositorio: github.com/rquiroga7/PIB-CyT"
)

subtitulo <- "Argentina 1995-2026 | Devengado mensual por período de gobierno"

# -----------------------------------------------------------------------------
# 8. Crear y guardar los gráficos
# -----------------------------------------------------------------------------

p_ciencia <- crear_grafico_pbi(
  data = devengado_anual,
  variable = "Ciencia_pct_PBI",
  titulo = "Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "devengado_ciencia_pbi",
  gobiernos_rect = gobiernos_rect,
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional,
  subtitulo = subtitulo
)

p_educacion <- crear_grafico_pbi(
  data = devengado_anual,
  variable = "Educacion_pct_PBI",
  titulo = "Devengado en Educación como % del PBI",
  filename_base = "devengado_educacion_pbi",
  gobiernos_rect = gobiernos_rect,
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional,
  subtitulo = subtitulo
)

p_ciencia_y0 <- crear_grafico_pbi(
  data = devengado_anual,
  variable = "Ciencia_pct_PBI",
  titulo = "Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "devengado_ciencia_pbi_y0",
  start_y_zero = TRUE,
  gobiernos_rect = gobiernos_rect,
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional,
  subtitulo = subtitulo
)

p_educacion_y0 <- crear_grafico_pbi(
  data = devengado_anual,
  variable = "Educacion_pct_PBI",
  titulo = "Devengado en Educación como % del PBI",
  filename_base = "devengado_educacion_pbi_y0",
  start_y_zero = TRUE,
  gobiernos_rect = gobiernos_rect,
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional,
  subtitulo = subtitulo
)

p_ciencia_bar <- crear_grafico_barras_pbi(
  data = devengado_anual,
  variable = "Ciencia_pct_PBI",
  titulo = "Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "devengado_ciencia_pbi_bar",
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional,
  subtitulo = subtitulo
)

p_educacion_bar <- crear_grafico_barras_pbi(
  data = devengado_anual,
  variable = "Educacion_pct_PBI",
  titulo = "Devengado en Educación como % del PBI",
  filename_base = "devengado_educacion_pbi_bar",
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional,
  subtitulo = subtitulo
)

p_ciencia_var <- crear_grafico_variacion_pbi(
  data = devengado_anual,
  variable = "Ciencia_pct_PBI",
  titulo = "Variación Interanual del Devengado en Ciencia y Técnica como % del PBI",
  filename_base = "devengado_ciencia_pbi_variacion",
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional,
  subtitulo = "Argentina 1996-2026 | Variación interanual en puntos porcentuales del PBI"
)

p_educacion_var <- crear_grafico_variacion_pbi(
  data = devengado_anual,
  variable = "Educacion_pct_PBI",
  titulo = "Variación Interanual del Devengado en Educación como % del PBI",
  filename_base = "devengado_educacion_pbi_variacion",
  colores_gobierno = colores_gobierno,
  caption_text = caption_text,
  tema_profesional = tema_profesional,
  subtitulo = "Argentina 1996-2026 | Variación interanual en puntos porcentuales del PBI"
)

# -----------------------------------------------------------------------------
# 9. Guardar la serie anual y validar contra presupuesto.csv
# -----------------------------------------------------------------------------

write.csv(devengado_anual, "devengado_anual.csv", row.names = FALSE)

comparacion <- devengado_anual %>%
  select(Año, PBI, Ciencia_devengado, Educacion_devengado) %>%
  left_join(
    presupuesto %>% select(Año, PBI_pc = PBI,
                           Ciencia_pc = Devengado.Ciencia,
                           Educacion_pc = Devengado.Educación),
    by = "Año"
  ) %>%
  mutate(
    dif_Ciencia = round(Ciencia_devengado - Ciencia_pc, 1),
    dif_Educacion = round(Educacion_devengado - Educacion_pc, 1)
  )

cat("\n=== Validación: devengado API vs presupuesto.csv (millones de $) ===\n\n")
print(as.data.frame(comparacion), row.names = FALSE)

cat("\n=== Diferencias absolutas medias ===\n")
cat("Ciencia:   ", round(mean(comparacion$dif_Ciencia, na.rm = TRUE), 1), "millones\n")
cat("Educación: ", round(mean(comparacion$dif_Educacion, na.rm = TRUE), 1), "millones\n")
