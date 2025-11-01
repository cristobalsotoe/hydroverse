library(tmap)
library(terra)


f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)

tm_options_reset()
tmap_options(legend.frame = FALSE)

# Función auxiliar para crear col.scale con label.style por defecto
# Guardar la función original

tm_shape(r) +
  tm_raster(col.scale = tm_scale_intervals())



tm_shape(r) +
  tm_raster(
            col.scale = tm_scale_intervals(label.style = "continuous",
                                           style = "pretty" ),          # Transparencia del borde (0-1)),
            col.legend = tm_legend(
              title = "Elevación (m)",      # Título de la leyenda
              orientation = "portrait",      # "portrait" o "landscape"
              col = "black",
              lwd = 1,# Transparencia del borde (0-1)
              item.r =0,
              frame = FALSE,
              position = tm_pos_out("right", "center")  # Posición
            ))
# Para intervalos con etiquetas mejoradas:
tm_shape(r) +
  tm_raster(col.scale = tm_scale_intervals(
    style = "pretty",
    n = 5
  ))


tm_shape(World) +
  tm_polygons("well_being",
              fill.scale = tm_scale_intervals(label.style = "continuous"),
              fill.legend = tm_legend(reverse = TRUE,
                                      lwd = 2))


tmap_mode("plot")

tm_shape(r) +
  tm_raster(
    col.scale = tm_scale_intervals(
      label.style	= "continue",
      breaks = c(100, 200, 300, 400, 500, 600 )
     ),
    col.legend = tm_legend(
     col = "black",
     frame = F,
     lwd = 1,
     title = ""
    )
  )


f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)

tm_shape(r) +
  tm_raster(col.scale = tm_scale_intervals(label.style = ""),
            col.legend = tm_legend(
              col = "black",
              frame = F,
              lwd = 1,
            ))

tm_shape(r) +
  tm_raster(col.scale =
              tm_scale_intervals(label.style = "",
                                 values = color_temp2(10)),
            col.legend = tm_legend(
              col = "black",
              frame = F,
              lwd = 1,
              title = ""
            ))


qtm(r,
    col.scale = tm_scale_intervals(
      label.style = "continue",
      colors = terrain.colors
))

library(tmap)

library(tmap)

tm_raster_default <- function(...,
                              col.scale = NULL,
                              col.legend = NULL) {
  # Define defaults
  default_scale <- tm_scale_intervals(
    label.style = "",
    values = color_temp2(10)
  )

  default_legend <- tm_legend(
    col = "black",
    frame = FALSE,
    lwd = 1,
    title = ""
  )

  # Usa los del usuario si se pasan; si no, usa los defaults
  if (is.null(col.scale)) col.scale <- default_scale
  if (is.null(col.legend)) col.legend <- default_legend

  tm_raster(col.scale = col.scale, col.legend = col.legend, ...)
}

tm_legend <- function( col = "black",
                       frame = FALSE,
                       lwd = 1,
                       title = "", ...) {
  tm_legend(col = col,frame=frame,lwd=lwd,title=title, ...)
}

# Guardar la función original
tm_scale_intervals_original <- tm_scale_intervals

# Sobrescribir con tu versión personalizada
tm_scale_intervals <- function(label.style = "", ...) {
  tm_scale_intervals_original(label.style = label.style, ...)
}

# Guardar la función original
tm_legend_original <- tm_legend

tm_legend <- function( col = "black",
                       frame = FALSE,
                       lwd = 1,
                       title = "", ...) {
  tm_legend_original(col = col,frame=frame,lwd=lwd,title=title, ...)
}

library(tmap)

tm_raster_default <- function(...,
                              col.scale = NULL,
                              col.legend = NULL) {
  # Define defaults
  default_scale <- tm_scale_intervals(
    label.style = "")

  default_legend <- tm_legend(
    col = "black",
    frame = FALSE,
    lwd = 1,
    title = ""
  )

  # Usa los del usuario si se pasan; si no, usa los defaults
  if (is.null(col.scale)) col.scale <- default_scale
  if (is.null(col.legend)) col.legend <- default_legend

  tm_raster(col.scale = col.scale, col.legend = col.legend, ...)
}


tm_shape(r) +
  tm_raster_default(
    col.scale = tm_scale_intervals(values=colors_green2blue(10)),
    col.legend = tm_legend(title = "juji"))



tm_shape(r) +
  tm_raster(col.scale = tm_scale_intervals(),
            col.legend = tm_legend())

