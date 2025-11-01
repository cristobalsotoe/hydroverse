

# Definición de todas las variables de configuración al inicio (comunes y específicas)
hydroplots <- function(
    main,
    data,
    x = "fecha", # Nombre por defecto para columna de fechas
    y = "valor", # Nombre por defecto para columna de valores
    caption ="",
    filename.png = FALSE,
    filename.xlsx=FALSE, filename=NULL, width = 7, height = 7,dpi=600, na.rm.max = 0.85,

    # Argumentos para caption
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

    # Argumentos para gráfico diario
    dia_title_label = "Escala diaria",
    dia_x_axis_title = "Fecha",
    dia_line_width = 0.5,
    dia_date_breaks = "5 years",
    dia_date_minor_breaks = "1 years",
    dia_date_labels = "%d-%b\n%Y",
    dia_na_rect_fill = "grey40",
    dia_na_rect_alpha = 0.2,
    ylab_day = "Caudal (m³/s)",

    # Argumentos para gráfico mensual
    mes_title_label = "Escala mensual",
    mes_x_axis_title = "Fecha",
    mes_line_width = 0.5,
    mes_date_breaks = "10 years",
    mes_date_minor_breaks = "1 years",
    mes_date_labels = "%b %Y",
    ylab_month = "Caudal (m³/s)",

    # Argumentos para gráfico anual
    anual_title_label = "Escala anual",
    anual_x_axis_title = "Año",
    anual_x_limits = c(1980, 2024),
    anual_no_data_message = "La estación no tiene registros anuales\nsuficientes para generar una gráfica anual",
    anual_no_data_bg_fill = "grey80",
    anual_no_data_text_size = 2,
    ylab_anual= "Caudal (m³/s)",

    # Argumentos para gráfico estacional
    estacional_title_label = "Año estacional",
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
    ylab_estational= "Caudal (m³/s)",

    # Argumentos para curva de duración
    curvaduracion_title_label = "Curva de duración",
    curvaduracion_x_axis_title = "Probabilidad de excedencia",
    curvaduracion_line_width = 1,
    ylab_cdf = "Caudal (m³/s)"

) {

  require(tidyplots)
  require(ggplot2)
  require(dplyr)
  require(lubridate)
  require(scales)
  require(zoo)
  require(hydroTSM)
  require(patchwork)
  require(StratigrapheR)

  # **NUEVA FUNCIONALIDAD: Detección y conversión del tipo de datos**

  # Si x es NULL (para pipeline), intentar obtener datos del pipeline
  if (is.null(data)) {
    stop("Debe proporcionar datos en el argumento 'data' o usar la función en un pipeline")
  }

  # Detectar si es data.frame y convertir a zoo
  if (is.data.frame(data)) {

    # Verificar que tenga al menos 2 columnas
    if (ncol(data) < 2) {
      stop("El data.frame debe tener al menos 2 columnas")
    }

    # Buscar columnas de fecha y valores
    date_column <- NULL
    value_column <- NULL

    # Intentar encontrar columna de fecha por nombre
    if (x %in% names(data)) {
      date_column <- x
    } else {
      # Buscar columna de tipo Date o que contenga "fecha", "date", "time"
      for (col_name in names(data)) {
        if (inherits(data[[col_name]], c("Date", "POSIXct", "POSIXt")) ||
            grepl("fecha|date|time", col_name, ignore.case = TRUE)) {
          date_column <- col_name
          break
        }
      }
    }

    # Intentar encontrar columna de valores por nombre
    if (y %in% names(data)) {
      value_column <- y
    } else {
      # Buscar columna numérica (primera que no sea fecha)
      for (col_name in names(data)) {
        if (is.numeric(data[[col_name]]) && col_name != date_column) {
          value_column <- col_name
          break
        }
      }
    }

    # Si no se encontraron, usar primeras 2 columnas
    if (is.null(date_column) || is.null(value_column)) {
      message("No se pudieron identificar automáticamente las columnas de fecha y valores.")
      message("Usando las primeras dos columnas como fecha y valores respectivamente.")
      date_column <- names(data)[1]
      value_column <- names(data)[2]
    }

    # Convertir columna de fecha si no es Date
    if (!inherits(data[[date_column]], c("Date", "POSIXct", "POSIXt"))) {
      data[[date_column]] <- as.Date(data[[date_column]])
    }

    # Crear objeto zoo
    message(paste("Usando columna", date_column, "como fechas y", value_column, "como valores"))
    data <- zoo(data[[value_column]], data[[date_column]])

  } else if (!is.zoo(data)) {
    stop("El argumento 'data' debe ser un objeto zoo o data.frame")
  }

  # **RESTO DEL CÓDIGO ORIGINAL** (sin cambios)


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
  ts.month <- hydroTSM::daily2monthly(data,
                                      FUN = mean,
                                      na.rm.max = na.rm.max)
  ts.annual <- hydroTSM::daily2annual(ts.month,
                                      FUN = mean,
                                      na.rm.max = na.rm.max)
  ts.monthly <- hydroTSM::monthlyfunction(ts.month,
                                          FUN = mean,
                                          na.rm = TRUE)
  cdf_month <- data.frame(x = fdc(ts.month, plot = FALSE) * 100,
                           y = ts.month)

  # Conversión a data frames
  x_df <- fortify.zoo(data)
  ts.month_df <- fortify.zoo(ts.month) %>%
    mutate(month = month(Index),
           year = year(Index))
  ts.annual <- fortify.zoo(ts.annual)
  caudal_estacional_df <- fortify.zoo(ts.monthly)
  caudal_estacional_df$Index <- estacional_name_month

  # Calcular períodos sin datos
  na_periods <- x_df %>%
    mutate(is_na = is.na(data)) %>%
    group_by(grp = cumsum(c(1, diff(is_na)) != 0)) %>%
    filter(is_na) %>%
    summarise(start = min(Index),
              end = max(Index)) %>%
    ungroup()

  # Calcular variables dinámicas que dependen de los datos
  dia_y_axis_limit_max <- max(x_df$data, na.rm = TRUE) * 1.1
  dia_x_annotation <- mean(range(x_df$Index))
  dia_y_annotation <- max(x_df$data, na.rm = TRUE) - 0.1

  mes_y_axis_limit_max <- max(ts.month_df$ts.month, na.rm = TRUE) * 1.1
  mes_x_annotation <- mean(range(ts.month_df$Index))
  mes_y_annotation <- max(ts.month_df$ts.month, na.rm = TRUE) - 0.1

  anual_y_axis_limit_max <- max(ts.annual$ts.annual, na.rm = TRUE) * 1.2
  anual_x_annotation <- mean(range(year(ts.annual$Index)))
  anual_y_annotation <- max(ts.annual$ts.annual, na.rm = TRUE) * 1.1

  estacional_y_axis_limit_max <- max(caudal_estacional_df$ts.monthly, na.rm = TRUE) * 1.2
  estacional_y_annotation <- max(caudal_estacional_df$ts.monthly, na.rm = TRUE) * 1.25

  curvaduracion_y_axis_limit_max <- max(cdf_month$y, na.rm = TRUE) * 1.1
  curvaduracion_x_annotation <- mean(range(cdf_month$x, na.rm = TRUE))
  curvaduracion_y_annotation <- max(cdf_month$y, na.rm = TRUE) - 0.1

  # Gráfico 1: Caudal diario
  plot_dia <- x_df %>%
    tidyplot(x = Index, y = data, color = data) %>%
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
    adjust_y_axis_title(ylab_day) %>%
    add(ggplot2::scale_x_date(
      date_breaks = dia_date_breaks,
      date_minor_breaks =  dia_date_minor_breaks,
      date_labels = dia_date_labels,
      limits = c(x_df$Index[1], NA)
    ))

  if (nrow(na_periods) > 0) {
    plot_dia <- plot_dia %>%
      add_annotation_rectangle(
        xmin = na_periods$start,
        xmax = na_periods$end,
        ymin = -Inf,
        ymax = Inf,
        fill = dia_na_rect_fill,
        alpha = dia_na_rect_alpha
      )
  }

  plot_dia <- plot_dia %>%
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
    adjust_y_axis_title(ylab_month) %>%
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
      adjust_y_axis_title(ylab_anual) %>%
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
      adjust_y_axis_title(ylab_estational) %>%
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
  plot_curvaduracion <- cdf_month %>%
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
    adjust_y_axis_title(ylab_cdf) %>%
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
  if (!is.null(filename.png)) {
    # dir.create(drty.out, showWarnings = FALSE, recursive = TRUE)
    ggsave(filename = filename.png,
           plot = hydro_graph,
           width = width, height = height, dpi = dpi)

  } else {
    # Mostrar el gráfico en consola
    print(hydro_graph)
  }

  # Gauardar sereies diarios, mensuales, anuales y estacionales en un excel
  if (!is.null(filename.xlsx)) {

     hs <- createStyle(
      textDecoration = "BOLD", fontColour = "black", fgFill = "grey80"
    )

    openxlsx::write.xlsx(
      x = list(
        day = x_df,
        month = ts.month_df,
        annual = ts.annual,
        monthly = caudal_estacional_df
      ),
      file = filename.xlsx,
      sheetName = c("day", "month", "annual", "monthly"),
      borders ="all",
      headerStyle = hs,
      append = TRUE
    )


  }

  return(hydro_graph)
}


# Ejemplo de uso (comentar o descomentar según necesidad)

title_case_custom <- function(texto) {
  # Palabras que van en minúscula (si no son la primera)
  palabras_min <- c("de", "del", "la", "las", "el", "los", "en", "y", "a", "con", "por", "para", "o")

  # Pasar todo a minúscula primero
  palabras <- tolower(strsplit(texto, " ")[[1]])

  # Capitalizar primera palabra siempre
  palabras[1] <- stringr::str_to_title(palabras[1])

  # Capitalizar solo las que no están en la lista de minúsculas
  for (i in 2:length(palabras)) {
    if (!(palabras[i] %in% palabras_min)) {
      palabras[i] <- stringr::str_to_title(palabras[i])
    }
  }

  # Unir y devolver
  paste(palabras, collapse = " ")
}


# drty.out.cantidad.esta <- paste0(drty.out.cantidad,"/","Estaciones Fluviometrica")
# drty.out.hidrograma <- paste0(drty.out.cantidad.esta,'/',"Hidrogramas")
#
# dir.create(drty.out.hidrograma)
#
# nrow(Inf_estacion_q)
#
# for (i in 1:nrow(Inf_estacion_q)) {
#
#   id <- as.character(Inf_estacion_q[i,1])
#   name.station <- title_case_custom(as.character(Inf_estacion_q[i,3]))
#   x <- caudales.zoo$estaciones[, id]
#
#   drty.out.hidrograma.estacion <- paste0(drty.out.hidrograma, "/", name.station)
#   if (!dir.exists(drty.out.hidrograma.estacion)){
#     dir.create(drty.out.hidrograma.estacion)
#   }
#
#   plots <- generate_flow_plots(
#     main = paste0("Estación ",name.station," (",id,")"),
#     plot_vars = plot_vars,
#     x = x,
#     drty.out = drty.out.hidrograma.estacion
#   )
#
# }


#
# # Mostrar los gráficos
# plots$dia
# plots$mes
# plots$anual
# plots$estacional
# plots$curvaduracion




# Ejemplo
# library(zoo)
# data <- zoo(rnorm(365, 10, 5), seq(as.Date("2020-01-01"), by = "day", length.out = 365))
#  data %>% hydroplots(main = "")
# Ejemplo de uso (comentar o descomentar según necesidad)

# title_case_custom <- function(texto) {
  # Palabras que van en minúscula (si no son la primera)
#   palabras_min <- c("de", "del", "la", "las", "el", "los", "en", "y", "a", "con", "por", "para", "o")
#
#   # Pasar todo a minúscula primero
#   palabras <- tolower(strsplit(texto, " ")[[1]])
#
#   # Capitalizar primera palabra siempre
#   palabras[1] <- stringr::str_to_title(palabras[1])
#
#   # Capitalizar solo las que no están en la lista de minúsculas
#   for (i in 2:length(palabras)) {
#     if (!(palabras[i] %in% palabras_min)) {
#       palabras[i] <- stringr::str_to_title(palabras[i])
#     }
#   }
#
#   # Unir y devolver
#   paste(palabras, collapse = " ")
# }


# drty.out.cantidad.esta <- paste0(drty.out.cantidad,"/","Estaciones Fluviometrica")
# drty.out.hidrograma <- paste0(drty.out.cantidad.esta,'/',"Hidrogramas")
#
# dir.create(drty.out.hidrograma)
#
# nrow(Inf_estacion_q)
#
# for (i in 1:nrow(Inf_estacion_q)) {
#
#   id <- as.character(Inf_estacion_q[i,1])
#   name.station <- title_case_custom(as.character(Inf_estacion_q[i,3]))
#   x <- caudales.zoo$estaciones[, id]
#
#   drty.out.hidrograma.estacion <- paste0(drty.out.hidrograma, "/", name.station)
#   if (!dir.exists(drty.out.hidrograma.estacion)){
#     dir.create(drty.out.hidrograma.estacion)
#   }
#
#   plots <- generate_flow_plots(
#     main = paste0("Estación ",name.station," (",id,")"),
#     plot_vars = plot_vars,
#     x = x,
#     drty.out = drty.out.hidrograma.estacion
#   )
#
# }


#
# # Mostrar los gráficos
# plots$dia
# plots$mes
# plots$anual
# plots$estacional
# plots$curvaduracion
