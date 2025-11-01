

# Definición de todas las variables de configuración al inicio (comunes y específicas)
hydroplots <- function(
    x, # vector
    main,
    filename.png = FALSE,
    filename.xlsx = FALSE,
    na.rm.max = 0.85,

    # Argumentos para caption
    caption ="",
    plot.title = ggplot2::element_text(size = 11, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12),
    plot.caption = ggplot2::element_text(size = 8),

    # Argumentos comunes
    font_size_title = 3,
    font_face = "plain",
    hjust = 0.5,
    vjust = 0,
    color_graph = rev(tidyplots::colors_continuous_mako[seq(60,60+10*11,10)]),
    y_axis_title = "$Caudal~(m^3/s)$",

    # Argumentos figura PNG
    width = 7,
    height = 7,
    dpi=600,

    # Argumentos para gráfico diario
    dia_title_label = "Escala diaria",
    dia_x_axis_title = "Fecha",
    dia_line_width = 0.5,
    dia_date_breaks = "5 years",
    dia_date_minor_breaks = "1 years",
    dia_date_labels = "%d-%b\n%Y",
    dia_na_rect_fill = "grey40",
    dia_na_rect_alpha = 0.2,

    # Argumentos para gráfico mensual
    mes_title_label = "Escala mensual",
    mes_x_axis_title = "Fecha",
    mes_line_width = 0.5,
    mes_date_breaks = "10 years",
    mes_date_minor_breaks = "1 years",
    mes_date_labels = "%b %Y",

    # Argumentos para gráfico anual
    anual_title_label = "Escala anual",
    anual_x_axis_title = "Año",
    anual_x_limits = c(1980, 2024),
    anual_no_data_message = "La estación no tiene registros anuales\nsuficientes para generar una gráfica anual",
    anual_no_data_bg_fill = "grey80",
    anual_no_data_text_size = 2,

    # Argumentos para gráfico estacional
    estacional_title_label = "Caudal estacional",
    estacional_x_axis_title = "Mes",
    estacional_mean_bar_alpha = 1,
    estacional_mean_value_color = "black",
    estacional_mean_value_accuracy = 1,
    estacional_x_annotation = "Oct",
    estacional_no_data_message = "La estación no tiene registros mensuales\nsuficientes para generar una gráfica estacional",
    estacional_no_data_bg_fill = "grey80",
    estacional_no_data_text_size = 2,
    estacional_name_month = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
    estacional_month_initial = 4,

    # Argumentos para curva de duración
    curvaduracion_title_label = "Curva de duración",
    curvaduracion_x_axis_title = "Probabilidad de excedencia",
    curvaduracion_line_width = 1
) {

  require(tidyplots)
  require(ggplot2)
  require(dplyr)
  require(lubridate)
  require(scales)
  require(zoo)
  require(hydroTSM)
  require(patchwork)
  library(StratigrapheR)

  # Tema común
  theme_common <- theme(
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.15),
    panel.grid.major.x = element_line(colour = "grey", linewidth = 0.15),
    panel.grid.minor = element_line(color = "gray90", size = 0.3)
  )

  # Tema específico para gráfico estacional
  theme_estacional <- theme(
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.15)
  )

  # Procesamiento de datos
  ts.month <- hydroTSM::daily2monthly(x,
                                      FUN = mean,
                                      na.rm.max = na.rm.max)
  ts.annual <- hydroTSM::daily2annual(ts.month,
                                      FUN = mean,
                                      na.rm.max = na.rm.max)
  ts.monthly <- hydroTSM::monthlyfunction(ts.month,
                                          FUN = mean,
                                          na.rm = TRUE)
  cdf_diario <- data.frame(x = fdc(x, plot = FALSE) * 100,
                           y = x)

  # Conversión a data frames
  x_df <- fortify.zoo(x)
  ts.month_df <- fortify.zoo(ts.month) %>%
    mutate(month = month(Index),
           year = year(Index))
  ts.annual <- fortify.zoo(ts.annual)
  caudal_estacional_df <- fortify.zoo(ts.monthly)
  caudal_estacional_df$Index <- estacional_name_month

  # Calcular períodos sin datos
  na_periods <- x_df %>%
    mutate(is_na = is.na(x)) %>%
    group_by(grp = cumsum(c(1, diff(is_na)) != 0)) %>%
    filter(is_na) %>%
    summarise(start = min(Index),
              end = max(Index)) %>%
    ungroup()

  # Calcular variables dinámicas que dependen de los datos
  dia_y_axis_limit_max <- max(x_df$x, na.rm = TRUE) * 1.1
  dia_x_annotation <- mean(range(x_df$Index))
  dia_y_annotation <- max(x_df$x, na.rm = TRUE) - 0.1

  mes_y_axis_limit_max <- max(ts.month_df$ts.month, na.rm = TRUE) * 1.1
  mes_x_annotation <- mean(range(ts.month_df$Index))
  mes_y_annotation <- max(ts.month_df$ts.month, na.rm = TRUE) - 0.1

  anual_y_axis_limit_max <- max(ts.annual$ts.annual, na.rm = TRUE) * 1.2
  anual_x_annotation <- mean(range(year(ts.annual$Index)))
  anual_y_annotation <- max(ts.annual$ts.annual, na.rm = TRUE) * 1.1

  estacional_y_axis_limit_max <- max(caudal_estacional_df$ts.monthly, na.rm = TRUE) * 1.2
  estacional_y_annotation <- max(caudal_estacional_df$ts.monthly, na.rm = TRUE) * 1.25

  curvaduracion_y_axis_limit_max <- max(cdf_diario$y, na.rm = TRUE) * 1.1
  curvaduracion_x_annotation <- mean(range(cdf_diario$x, na.rm = TRUE))
  curvaduracion_y_annotation <- max(cdf_diario$y, na.rm = TRUE) - 0.1

  # Gráfico 1: Caudal diario
  plot_dia <- x_df %>%
    tidyplot(x = Index, y = x, color = x) %>%
    adjust_size(width = NA, height = NA) %>%
    add(geom_line(linewidth = dia_line_width)) %>%
    add(annotate("text",
                 x = dia_x_annotation,
                 y = dia_y_annotation,
                 label = dia_title_label,
                 size = font_size_title,
                 fontface = font_face,
                 hjust = hjust,
                 vjust = vjust)) %>%
    remove_legend() %>%
    adjust_colors(new_colors = color_graph) %>%
    adjust_y_axis(limits = c(0, dia_y_axis_limit_max)) %>%
    adjust_x_axis_title(dia_x_axis_title) %>%
    adjust_y_axis_title(y_axis_title) %>%
    add(scale_x_date(
      date_breaks = dia_date_breaks,
      date_minor_breaks = dia_date_minor_breaks,
      date_labels = dia_date_labels,
      limits = c(x_df$Index[1], NA)
    )) %>%
    add_annotation_rectangle(
      xmin = na_periods$start,
      xmax = na_periods$end,
      ymin = -Inf,
      ymax = Inf,
      fill = dia_na_rect_fill,
      alpha = dia_na_rect_alpha
    ) %>%
    adjust_theme_details(!!!theme_common)

  # Gráfico 2: Caudal mensual
  plot_mes <- ts.month_df %>%
    tidyplot(x = Index, y = ts.month, color = ts.month) %>%
    adjust_size(width = NA, height = NA) %>%
    add(geom_line(linewidth = mes_line_width)) %>%
    remove_legend() %>%
    adjust_colors(new_colors = color_graph) %>%
    adjust_y_axis(limits = c(0, mes_y_axis_limit_max)) %>%
    add(annotate("text",
                 x = mes_x_annotation,
                 y = mes_y_annotation,
                 label = mes_title_label,
                 size = font_size_title,
                 fontface = font_face,
                 hjust = hjust,
                 vjust = vjust)) %>%
    adjust_x_axis_title(mes_x_axis_title) %>%
    adjust_y_axis_title(y_axis_title) %>%
    add(scale_x_date(
      date_breaks = mes_date_breaks,
      date_minor_breaks = mes_date_minor_breaks,
      date_labels = mes_date_labels
    )) %>%
    adjust_theme_details(!!!theme_common)

  # Gráfico 3: Caudal anual
  if (length(na.omit(ts.annual$ts.annual)) != 0) {
    plot_anual <- ts.annual %>%
      mutate(year = year(Index)) %>%
      tidyplot(x = year, y = ts.annual, color = ts.annual) %>%
      adjust_size(width = NA, height = NA) %>%
      add_mean_bar() %>%
      adjust_colors(new_colors = color_graph) %>%
      adjust_y_axis(limits = c(0, anual_y_axis_limit_max)) %>%
      add(annotate("text",
                   x = anual_x_annotation,
                   y = anual_y_annotation,
                   label = anual_title_label,
                   size = font_size_title,
                   fontface = font_face,
                   hjust = hjust,
                   vjust = vjust)) %>%
      remove_legend() %>%
      adjust_x_axis(limits = anual_x_limits) %>%
      adjust_x_axis_title(anual_x_axis_title) %>%
      adjust_y_axis_title(y_axis_title) %>%
      adjust_theme_details(!!!theme_common)
  } else {
    plot_anual <- ggplot() +
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                fill = anual_no_data_bg_fill) +
      annotate("text",
               x = 0, y = 0,
               label = anual_no_data_message,
               size = anual_no_data_text_size,
               hjust = 0.5, vjust = 0.5) +
      theme_void()
  }

  # Gráfico 4: Caudal estacional
  if (length(na.omit(caudal_estacional_df$ts.monthly)) != 0) {

    # Ajustar precisión para valores pequeños
    accuracy_value <- if (min(caudal_estacional_df$ts.monthly, na.rm = TRUE) < 3) {
      0.1
    } else {
      estacional_mean_value_accuracy
    }

    plot_estacional <- caudal_estacional_df %>%
      tidyplot(x = Index, y = ts.monthly, color = ts.monthly) %>%
      adjust_size(width = NA, height = NA) %>%
      adjust_colors(new_colors = color_graph) %>%
      remove_legend() %>%
      add_mean_bar(alpha = estacional_mean_bar_alpha) %>%
      adjust_y_axis(limits = c(0, estacional_y_axis_limit_max)) %>%
      add(annotate("text",
                   x = estacional_x_annotation,
                   y = estacional_y_annotation,
                   label = estacional_title_label,
                   size = font_size_title,
                   fontface = font_face,
                   hjust = hjust,
                   vjust = vjust)) %>%
      add_mean_value(color = estacional_mean_value_color,
                     accuracy = accuracy_value) %>%
      reorder_x_axis_labels(shift(estacional_name_month , p = estacional_month_initial)) %>%
      adjust_x_axis_title(estacional_x_axis_title) %>%
      adjust_y_axis_title(y_axis_title) %>%
      adjust_theme_details(!!!theme_estacional)
  } else {
    plot_estacional <- ggplot() +
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                fill = estacional_no_data_bg_fill) +
      annotate("text",
               x = 0, y = 0,
               label = estacional_no_data_message,
               size = estacional_no_data_text_size,
               hjust = 0.5, vjust = 0.5) +
      theme_void()
  }

  # Gráfico 5: Curva de duración
  plot_curvaduracion <- cdf_diario %>%
    tidyplot(x = x, y = y) %>%
    adjust_size(width = NA, height = NA) %>%
    add_line(linewidth = curvaduracion_line_width) %>%
    adjust_y_axis(limits = c(0, curvaduracion_y_axis_limit_max)) %>%
    add(annotate("text",
                 x = curvaduracion_x_annotation,
                 y = curvaduracion_y_annotation,
                 label = curvaduracion_title_label,
                 size = font_size_title,
                 fontface = font_face,
                 hjust = hjust,
                 vjust = vjust)) %>%
    adjust_x_axis_title(curvaduracion_x_axis_title) %>%
    adjust_y_axis_title(y_axis_title) %>%
    add(scale_x_continuous(labels = scales::percent_format(scale = 1))) %>%
    adjust_theme_details(!!!theme_common)

  # Crear el gráfico final combinado
  hydro_graph <- (plot_dia / (plot_mes + plot_anual) / (plot_estacional + plot_curvaduracion)) +
    plot_annotation(
      title = main,
      caption = caption
    ) & theme(plot.title = plot.title,
              plot.subtitle = plot.subtitle,
              plot.caption = plot.subtitle)

  # Guardar el gráfico
  if (filename.png) {
    # dir.create(drty.out, showWarnings = FALSE, recursive = TRUE)
    ggsave(paste0(filename),
           plot = hydro_graph,
           width = width, height = height, dpi = dpi)

  } else {
    # Mostrar el gráfico en consola
    print(hydro_graph)
  }
}
