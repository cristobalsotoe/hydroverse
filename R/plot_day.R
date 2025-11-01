
plot_day <- function(
    main,                           # Título principal del gráfico combinado
    data,                           # Data.frame o zoo: datos de entrada (debe contener fechas y una o más series numéricas)
    x = "fecha",                    # Nombre columna de fechas (si es data.frame)
    y = "valor",                    # Nombre columna de valores (si es data.frame y solo una serie)
    caption = "",                   # Texto de pie de figura
    filename.png = NULL,            # Nombre de archivo para guardar gráfico en PNG (opcional, si es NULL solo muestra en consola)
    filename.xlsx = NULL,           # Nombre de archivo para guardar datos en Excel (opcional, si es NULL no exporta)
    width = 7,                      # Ancho del gráfico exportado (pulgadas)
    height = 7,                     # Alto del gráfico exportado (pulgadas)
    dpi = 600,                      # Resolución en dpi para exportar PNG
    font_size=7,
    # ---------- Configuración multiserie ----------
    series_names = NULL,            # Vector con los nombres de cada serie (si hay varias)
    legend_position = "bottom",     # Ubicación de la leyenda ("bottom", "right", etc)
    show_legend = TRUE,             # Mostrar la leyenda (TRUE/FALSE)
    legend_title = NULL,      # Texto personalizado para la leyenda, o NULL para sin título
    show_legend_dia = TRUE,
    legend.position_day =  c(0.95, 0.95),   # posición x, y
    legend.justification_day =c("right", "top"),
    legend.direction_day = "horizontal",
    # ---------- Tipo de geometría por gráfico ----------
    dia_geom_type = "line",         # "line", "point", "both". Tipo de gráfico para la serie diaria
    point_size = 1,                 # Tamaño de los puntos si type="point" o "both"
    point_alpha = 1,                # Transparencia de los puntos
    # ---------- Control de límites Y por panel ----------
    dia_y_limit_factor = 1.0,       # Factor para ampliar el eje Y diario (ej: 1.2 para 20% más)
    # ---------- Control de posición de título ----------
    title_position_method = "relative",  # "relative" o "fixed": posicionar título relativo al rango de Y o como valor absoluto
    title_vertical_position = 0.85,      # Relativo (0.85 = 85% de altura)
    title_padding_factor = 0.1,          # Ampliación por padding en Y
    title_offset = 0,                    # Offset manual
    dia_title_position = NULL,           # Sobrescribe posición vertical solo para diario (opcional)
    auto_adjust_y_limits = TRUE,         # Ajustar límites Y automáticamente
    y_padding_factor = 0.1,              # Factor de padding superior para todos los paneles
  # ---------- Estética global ----------
    plot.title = ggplot2::element_text(size = 11, hjust = 0.5),  # Formato título general
    plot.subtitle = ggplot2::element_text(size = 12),
    plot.caption = ggplot2::element_text(size = 8),
    font_size_title = 3,                # Tamaño texto para los títulos internos de subgráficos
    font_face = "plain",                # Tipo de fuente de títulos internos
    hjust = 0.5,                        # Justificación horizontal títulos internos
    vjust = 0,                          # Justificación vertical títulos internos
    color_graph = tidyplots::colors_discrete_friendly_long,  # Paleta de color usada
    # ---------- Etiquetas de ejes y títulos de cada panel ----------
    ylab_day = "Caudal (m³/s)",         # Eje Y diario
    dia_title_label = "Escala diaria",  # Título específico gráfico diario
    dia_x_axis_title = "Fecha",         # Eje X diario
    dia_line_width = 0.5,               # Grosor línea diario
    dia_date_breaks = "5 years",        # Saltos de fecha diario
    dia_date_minor_breaks = "1 years",  # Saltos menores
    dia_date_labels = "%d-%b\n%Y",      # Etiquetas de fecha
    dia_na_rect_fill = "grey40",        # Relleno para áreas sin datos
    dia_na_rect_alpha = 0.2)           # Transparencia de áreas sin datos
   {

require(tidyplots)
require(ggplot2)
require(dplyr)
require(lubridate)
require(scales)
require(zoo)
require(hydroTSM)
require(patchwork)
require(StratigrapheR)
require(tidyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

add_geom_by_type <- function(p, geom_type, line_width = 0.5, point_size = 1, point_alpha = 1, is_bar_plot = FALSE) {
  if (is_bar_plot) {
    switch(geom_type,
           "bar"   = p %>% add(geom_col(position = position_dodge(width = 0.75), width = 0.7, na.rm = TRUE)),
           "line"  = p %>% add(geom_line(aes(group = serie), linewidth = line_width)),
           "point" = p %>% add(geom_point(size = point_size, alpha = point_alpha, position = position_dodge(width = 0.3))),
           "both"  = p %>% add(geom_line(aes(group = serie), linewidth = line_width)) %>%
             add(geom_point(size = point_size, alpha = point_alpha)),
           p %>% add(geom_col(position = position_dodge(width = 0.75), width = 0.7, na.rm = TRUE))
    )
  } else {
    switch(geom_type,
           "line"  = p %>% add(geom_line(linewidth = line_width)),
           "point" = p %>% add(geom_point(size = point_size, alpha = point_alpha)),
           "both"  = p %>% add(geom_line(linewidth = line_width)) %>%
             add(geom_point(size = point_size, alpha = point_alpha)),
           p %>% add(geom_line(linewidth = line_width))
    )
  }
}

calculate_title_positions <- function(vals, plot_type) {
  if (length(na.omit(vals)) == 0) { return(list(y_pos = 0, y_max = 1, y_min = 0)) }
  y_min <- suppressWarnings(min(vals, na.rm = TRUE))
  y_max <- suppressWarnings(max(vals, na.rm = TRUE))
  rng <- y_max - y_min
  if (!is.finite(rng) || rng == 0) { rng <- abs(y_max) * 0.1 + 1 }
  if (auto_adjust_y_limits) {
    pad <- rng * y_padding_factor
    y_upper <- y_max + pad
    y_lower <- y_min - pad#*0.1
    # y_lower <- if (y_min > 0) 0 else (y_min - pad*0.1)
  } else {
    y_upper <- y_max * 1.1
    y_lower <- 0
  }
  pos <- switch(plot_type,
                "day" = dia_title_position %||% title_vertical_position,
                title_vertical_position
  )
  y_title <- if (title_position_method == "relative") {
    y_lower + (y_upper - y_lower) * pos + title_offset
  } else {
    y_max * pos + title_offset
  }
  list(y_pos = y_title, y_max = y_upper, y_min = y_lower)
}

# Preprocesamiento
multiple_series <- FALSE
if (is.data.frame(data)) {
  if (ncol(data) < 2) { stop("El data.frame debe tener al menos 2 columnas.") }
  if (x %in% names(data)) { date_column <- x
  } else {
    date_column <- NULL
    for (cn in names(data)) {
      if (inherits(data[[cn]], c("Date", "POSIXct", "POSIXt")) || grepl("fecha|date|time", cn, ignore.case = TRUE)) { date_column <- cn; break }
    }
    if (is.null(date_column)) { date_column <- names(data)[1] }
  }
  if (!inherits(data[[date_column]], c("Date", "POSIXct", "POSIXt"))) { data[[date_column]] <- as.Date(data[[date_column]]) }
  num_cols <- names(data)[sapply(data, is.numeric)]
  value_columns <- setdiff(num_cols, date_column)
  if (length(value_columns) > 1) {
    multiple_series <- TRUE
    if (is.null(series_names)) { series_names <- value_columns }
    if (length(series_names) != length(value_columns)) { series_names <- value_columns }
    m <- as.matrix(data[value_columns])
    colnames(m) <- series_names
    data <- zoo(m, data[[date_column]])
  } else {
    if (length(value_columns) == 0) { value_columns <- setdiff(names(data), date_column)[1] }
    data <- zoo(data[[value_columns[1]]], data[[date_column]])
  }
} else if (!is.zoo(data)) { stop("'data' debe ser zoo o data.frame.") }
else if (NCOL(data) > 1) {
  multiple_series <- TRUE
  if (is.null(colnames(data))) { colnames(data) <- paste0("serie_", seq_len(NCOL(data))) }
  if (is.null(series_names)) { series_names <- colnames(data) }
}

if (multiple_series) {
  n_series <- NCOL(data)
  x_df <- fortify.zoo(data) %>% pivot_longer(-Index, names_to = "serie", values_to = "valor")
 } else {
  x_df <- fortify.zoo(data)
}

# Cálculo posiciones y límites Y ----------------------------------
if (multiple_series) {
  pos_dia <- calculate_title_positions(x_df$valor, "day")
} else {
  pos_dia <- calculate_title_positions(x_df$data, "day")

}
# Aplicar el factor de expansión Y por panel y título ---------------
pos_dia$y_max    <- pos_dia$y_max * dia_y_limit_factor
pos_dia$y_min    <- pos_dia$y_min - (pos_dia$y_max-pos_dia$y_min)*(dia_y_limit_factor-1)

pos_dia$y_pos    <- min(pos_dia$y_pos, pos_dia$y_max)

dia_x_annotation <- mean(range(x_df$Index))

theme_common <- theme(
  panel.grid.major.y = element_line(colour = "grey", linewidth = 0.15),
  panel.grid.major.x = element_line(colour = "grey", linewidth = 0.15),
  panel.grid.minor = element_line(color = "gray90", linewidth = 0.1)
)

year_min <-lubridate::year(min(as.Date(x_df$Index)))
year_max <-lubridate::year(max(as.Date(x_df$Index)))
year_rang <- year_max-year_min
year_rang_in <- as.numeric(gsub(" years","",dia_date_breaks))
if (year_rang<year_rang_in){

  if (year_rang<1){

  dia_date_breaks <- "1 months"   # Saltos de fecha diario
  dia_date_minor_breaks <- "1 weeks"

  } else if (year_rang<2){
    dia_date_breaks <- paste0(year_rang," years")
    dia_date_minor_breaks <- "4 months"

  } else {
    dia_date_breaks <- paste0(year_rang," years")
  }

}

if (multiple_series) {
  plot_dia <- x_df %>%
    tidyplot(x = Index, y = valor, color = serie) %>%
    adjust_size(width = NA, height = NA) %>%
    adjust_colors(new_colors = color_graph[1:NCOL(data)]) %>%
    adjust_y_axis(limits = c(pos_dia$y_min, pos_dia$y_max)) %>%
    adjust_x_axis_title(dia_x_axis_title) %>%
    adjust_y_axis_title(ylab_day) %>%
    adjust_font(fontsize = font_size) %>%
    add(annotate("text", x = dia_x_annotation, y = pos_dia$y_pos,
                 label = dia_title_label, size = font_size_title,
                 fontface = font_face, hjust = hjust, vjust = vjust)) %>%
    add(scale_x_date(date_breaks = dia_date_breaks,
                     date_minor_breaks = dia_date_minor_breaks,
                     date_labels = dia_date_labels)) %>%
    adjust_theme_details(!!!theme_common)
  plot_dia <- add_geom_by_type(plot_dia, dia_geom_type, dia_line_width, point_size, point_alpha, FALSE)
  if (show_legend_dia) {
    plot_dia <- plot_dia %>% add(theme(legend.position = legend.position_day,   # posición x, y
                                       legend.justification =legend.justification_day,
                                       legend.direction = legend.direction_day)) %>%
      remove_legend_title()
  } else {
    plot_dia <- plot_dia %>% remove_legend()
  }
} else {
  plot_dia <- x_df %>%
    tidyplot(x = Index, y = data) %>%
    adjust_size(width = NA, height = NA) %>%
    adjust_colors(new_colors = color_graph) %>%
    adjust_y_axis(limits = c(pos_dia$y_min, pos_dia$y_max)) %>%
    adjust_x_axis_title(dia_x_axis_title) %>%
    adjust_y_axis_title(ylab_day) %>%
    adjust_font(fontsize = font_size) %>%
    add(annotate("text", x = dia_x_annotation, y = pos_dia$y_pos,
                 label = dia_title_label, size = font_size_title, fontface = font_face, hjust = hjust, vjust = vjust)) %>%
    add(scale_x_date(date_breaks = dia_date_breaks, date_minor_breaks = dia_date_minor_breaks, date_labels = dia_date_labels)) %>%
    adjust_theme_details(!!!theme_common)
  plot_dia <- add_geom_by_type(plot_dia, dia_geom_type, dia_line_width, point_size, point_alpha, FALSE)
  # Bloque de periodos NA (si aplican)
  na_periods <- x_df %>%
    mutate(is_na = is.na(data)) %>%
    group_by(grp = cumsum(c(1, diff(is_na)) != 0)) %>%
    filter(is_na) %>%
    summarise(start = min(Index), end = max(Index), .groups = "drop")
  if (nrow(na_periods) > 0) {
    plot_dia <- plot_dia %>% add_annotation_rectangle(
      xmin = na_periods$start, xmax = na_periods$end, ymin = -Inf, ymax = Inf,
      fill = dia_na_rect_fill, alpha = dia_na_rect_alpha
    )
  }
}

if (!is.null(filename.png)) {
  ggsave(filename = filename.png, plot = plot_dia, width = width, height = height, dpi = dpi)
} else {
  print(plot_dia)
}


}

# data_multiserie <- data.frame(
#   fecha = seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "day"),
#   estacion_1 = rnorm(7671, mean = 10, sd = 2),
#   estacion_2 = rnorm(7671, mean = 15, sd = 3),
#   estacion_3 = rnorm(7671, mean = 8, sd = 1.5)
# )
# data <- data_multiserie
# # Llamar la función
# plot_day(
#   main = "Análisis hidrológico - Múltiples estaciones",
#   data = data_multiserie,dia_geom_type = "point",
#   series_names = c("Estación A", "Estación B", "Estación C"),
#   show_legend_dia = T,
#   font_size = 7)


