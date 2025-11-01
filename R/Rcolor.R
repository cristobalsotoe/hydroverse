

colors_green2blue  <- colorRampPalette(rev(tidyplots::colors_continuous_mako[50:262]))
colors_white2red  <- colorRampPalette(rev(tidyplots::colors_continuous_rocket[50:262]))
colors_spectrals       <- colorRampPalette(RColorBrewer::brewer.pal(10, "Spectral"))
colors_BlueYellRed     <- colorRampPalette(c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8","#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"))
colors_Yell2Red <- colorRampPalette(c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
colors_YellGrBlue  <- colorRampPalette(viridis::viridis(5))
colors_Blue2Red <- colorRampPalette(tidyplots::colors_diverging_BuRd)
colors_friendly_long <- colorRampPalette(tidyplots::colors_discrete_friendly_long)
colors_Blue          <- colorRampPalette(c("#e1effc","#C6DBEF" ,"#9ECAE1", "#6BAED6" ,"#4292C6", "#2171B5", "#08519C", "#08306B","#011b3d"))
colors_Blue2Orange   <- colorRampPalette(paletteer::paletteer_c("ggthemes::Orange-Blue-White Diverging", 30))
colors_elevacion     <- colorRampPalette( c('#004529', '#006837', '#41ab5d','#78c679', '#addd8e', "#fee391","#6e310e", "#FFFFFF"))                                       

filled.contour(volcano, color.palette = colors_green2blue) 
filled.contour(volcano, color.palette = colors_white2red)
filled.contour(volcano, color.palette = colors_spectrals)
filled.contour(volcano, color.palette = colors_BlueYellRed)
filled.contour(volcano, color.palette = colors_Yell2Red)
filled.contour(volcano, color.palette = colors_YellGrBlue)
filled.contour(volcano, color.palette = colors_Blue2Red)
filled.contour(volcano, color.palette = colors_Blue)
filled.contour(volcano, color.palette = colors_Blue2Orange)
filled.contour(volcano, color.palette = colors_elevacion)


