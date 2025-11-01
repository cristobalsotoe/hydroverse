add_heatmap_cat <- function(.data,
                            x, 
                            y, 
                            color,
                            colors = c("#C02D45","#E37D46","#F6C54D","#4FAE62"),
                            breaks = c(0, 40, 80, 120, 160, 200, 240, 280, 320, 366),
                            ncut = 9 ,
                            legend_title = "Cantidad de días con\ndatos por año\n" ,
                            x_axis_title = "Año",
                            y_axis_title = "Código de la estación",
                            save.png = NULL,
                            width  =  7,
                            height  = 20 ,
                            fontsize =8
                     ) {
  require(tidyr)
 require(ggplot2)
   require(tidyplots)
  require(legendry)
  
  col_palette <- colorRampPalette(colors)
  
 plot <- .data %>%
    tidyplot(x = {{x}}, y = {{y}}, color = {{color}}) %>%
    adjust_size(NA, NA) %>%
    add_heatmap() %>%
    adjust_legend_title(legend_title) %>%
    adjust_x_axis_title(x_axis_title) %>%
    adjust_y_axis_title(y_axis_title) %>%
   adjust_font(fontsize = fontsize ) %>% 
    add(scale_fill_stepsn(
      colors = col_palette(ncut),
      breaks = breaks
    )) %>%
    add(
      guides(
        fill = guide_colsteps(
          first_guide  = guide_axis_base(bidi = FALSE),
          second_guide = "none",
          position     = "right",
          theme        = theme(
            legend.ticks        = element_line(colour = "black",linewidth=0.22),
            legend.ticks.length = grid::unit(3, "pt"),
            legend.axis.line    = element_line(colour = "black",linewidth=0.15),
            legend.key.size     = unit(2, "lines"),
            legend.key.width    = unit(1, "lines"),
            legend.frame        = element_rect(colour = "black",linewidth=0.2 )
          ),
          vanilla = TRUE
        )
      )
    ) %>%
    add(
      theme(panel.border = element_rect(colour = "black", fill = NA),
            legend.text = element_text(size = fontsize*0.9))
    )
  
  if (is.null(save.png)) {
    print(plot)
  } else {
    tidyplots::save_plot(plot,
                         filename = save.png,
                         width  = width ,
                         height  = height,
                         units = "cm",
                         bg = "white")
  }
  
}  
