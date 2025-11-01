#' Color Palettes for Hydroclimatic Variables
#'
#' @description
#' This file defines color palette functions specifically designed for
#' visualizing hydroclimatic variables. Each palette uses \code{colorRampPalette}
#' to generate interpolated color gradients.
#'
#' @name color_palettes
#' @keywords internal
NULL

#' Green to blue color palette
#'
#' @description
#' Generates a continuous color palette from green to blue tones,
#' based on tidyplots' Mako palette (reversed).
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' # Generate 10 colors
#' colors_green2blue(10)
#'
#' # Visualize the palette
#' filled.contour(volcano, color.palette = colors_green2blue)
#'
#' @details
#' Recommended variables: soil moisture, vegetation indices, NDVI,
#' evapotranspiration, groundwater levels
colors_green2blue <- colorRampPalette(rev(tidyplots::colors_continuous_mako[50:262]))

#' White to red color palette
#'
#' @description
#' Generates a continuous color palette from white to intense red,
#' based on tidyplots' Rocket palette (reversed).
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contour(volcano, color.palette = colors_white2red)
#'
#' @details
#' Recommended variables: temperature, heat waves, thermal stress,
#' fire risk index, maximum temperatures
colors_white2red <- colorRampPalette(rev(tidyplots::colors_continuous_rocket[50:262]))

#' Spectral color palette
#'
#' @description
#' Generates a continuous spectral color palette based on ColorBrewer's
#' "Spectral" palette, useful for visualizing bidirectional gradients.
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contourvolcano, color.palette = colors_spectrals)
#'
#' @details
#' Recommended variables: temperature ranges, seasonal variations,
#' climate zones, multi-directional anomalies
colors_spectrals <- colorRampPalette(RColorBrewer::brewer.pal(10, "Spectral"))

#' Blue-yellow-red divergent palette
#'
#' @description
#' Generates a divergent palette from blue (low values) through yellow
#' (medium values) to red (high values). Ideal for anomalies or deviations.
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contour(volcano, color.palette = colors_BlueYellRed)
#'
#' @details
#' Recommended variables: temperature, precipitation,
#' standardized indices (SPI, SPEI), climate change signals
colors_BlueYellRed <- colorRampPalette(c("#313695", "#4575B4", "#74ADD1", "#ABD9E9",
                                         "#E0F3F8", "#FEE090", "#FDAE61", "#F46D43",
                                         "#D73027", "#A50026"))

#' Sequential yellow to red palette
#'
#' @description
#' Generates a sequential palette from light yellow to dark red,
#' useful for representing intensity or concentration of variables.
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contour(volcano, color.palette = colors_Yell2Red)
#'
#' @details
#' Recommended variables: temperature , drought severity,
#' solar radiation, heat index
colors_Yell2Red <- colorRampPalette(c('#ffffcc', '#ffeda0', '#fed976', '#feb24c',
                                      '#fd8d3c', '#fc4e2a', '#e31a1c', '#bd0026', '#800026'))

#' Yellow-green-blue palette (Viridis)
#'
#' @description
#' Generates a perceptually uniform palette based on Viridis,
#' ideal for accessible and scientific visualizations.
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contour(volcano, color.palette = colors_YellGrBlue)
#'
#' @details
#' Recommended variables: wind, precipitation, streamflow, water depth,
#' any continuous hydroclimatic variable (colorblind-friendly)
colors_YellGrBlue <- colorRampPalette(viridis::viridis(5))

#' Blue to red divergent palette
#'
#' @description
#' Generates a divergent palette from blue to red based on tidyplots,
#' ideal for showing contrasts or anomalies.
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contour(volcano, color.palette = colors_Blue2Red)
#'
#' @details
#' Recommended variables: temperature, wet/dry anomalies,
#' climate indices (ONI, NAO), trend analysis
colors_Blue2Red <- colorRampPalette(tidyplots::colors_diverging_BuRd)

#' Long friendly discrete palette
#'
#' @description
#' Generates a palette of distinguishable discrete colors, useful for
#' multiple categories in visualizations.
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' colors_friendly_long(12)
#'
#' @details
#' Recommended variables: hydrological basins, climate classifications,
#' land use types, weather stations, categorical climate data
colors_friendly_long <- colorRampPalette(tidyplots::colors_discrete_friendly_long)

#' Sequential blues palette
#'
#' @description
#' Generates a sequential palette of blue tones, from very light blue
#' to very dark blue. Useful for precipitation or water bodies.
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contour(volcano, color.palette = colors_Blue)
#'
#' @details
#' Recommended variables: precipitation, streamflow, water storage,
#' snow water equivalent, reservoir levels, humidity
colors_Blue <- colorRampPalette(c("#e1effc", "#C6DBEF", "#9ECAE1", "#6BAED6",
                                  "#4292C6", "#2171B5", "#08519C", "#08306B", "#011b3d"))

#' Blue to orange divergent palette
#'
#' @description
#' Generates a divergent palette from blue to orange with white center,
#' based on ggthemes palette.
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contour(volcano, color.palette = colors_Blue2Orange)
#'
#' @details
#' Recommended variables: temperature contrasts, seasonal transitions,
#' cooling/warming trends, hydrothermal gradients
colors_Blue2Orange <- colorRampPalette(paletteer::paletteer_c("ggthemes::Orange-Blue-White Diverging", 30))

#' Topographic elevation palette
#'
#' @description
#' Generates a palette specifically designed to represent elevation,
#' from dark green (low) to brown and white (high).
#'
#' @param n Number of colors to generate
#' @return A vector of colors in hexadecimal format
#' @export
#' @examples
#' filled.contour(volcano, color.palette = colors_elevacion)
#'
#' @details
#' Recommended variables: elevation, digital elevation models (DEM),
#' topographic maps, terrain analysis, altitude zones
colors_elevacion <- colorRampPalette(c('#004529', '#006837', '#41ab5d', '#78c679',
                                       '#addd8e', "#fee391", "#6e310e", "#FFFFFF"))

# Visualization examples (for development/testing only)
# Uncomment to test the palettes
# filled.contour(volcano, color.palette = colors_green2blue)
# filled.contour(volcano, color.palette = colors_white2red)
# filled.contour(volcano, color.palette = colors_spectrals)
# filled.contour(volcano, color.palette = colors_BlueYellRed)
# filled.contour(volcano, color.palette = colors_Yell2Red)
# filled.contour(volcano, color.palette = colors_YellGrBlue)
# filled.contour(volcano, color.palette = colors_Blue2Red)
# filled.contour(volcano, color.palette = colors_Blue)
# filled.contour(volcano, color.palette = colors_Blue2Orange)
# filled.contour(volcano, color.palette = colors_elevacion)
