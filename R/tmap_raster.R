library(tmap)
library(sf)

colors_green2blue  <- colorRampPalette(rev(tidyplots::colors_continuous_mako[50:262]))

# Guardar la función original
tm_scale_intervals_original <- tm_scale_intervals

# Sobrescribir con tu versión personalizada
tm_scale_intervals <- function(label.style = "", values = colors_green2blue(100), ...) {
  tm_scale_intervals_original(label.style = label.style, values = values, ...)
}

# Guardar la función original
tm_legend_original <- tm_legend

tm_legend <- function( col = "black",
                       frame = FALSE,
                       lwd = 1,
                       title = "", ...) {
  tm_legend_original(col = col,frame=frame,lwd=lwd,title=title, ...)
}

tm_raster_original <- tm_raster

tm_raster<- function(...,
                              col.scale = NULL,
                              col.legend = NULL) {
  # Define defaults
  default_scale <- tm_scale_intervals()

  default_legend <- tm_legend(
  )

  # Usa los del usuario si se pasan; si no, usa los defaults
  if (is.null(col.scale)) col.scale <- default_scale
  if (is.null(col.legend)) col.legend <- default_legend

  tm_raster_original(col.scale = col.scale, col.legend = col.legend, ...)
}


# Ejemplo
# library(terra)
#  f <- system.file("ex/elev.tif", package="terra")
#  r <- rast(f)
# tm_shape(r) +
# tm_raster() +


