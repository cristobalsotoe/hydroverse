
require(ggplot2)
theme_common <- theme_minimal(base_size = 7) +
  # scale_color_manual(values = okabe_ito) +
  theme(
    text = element_text(family = "sans", size = 7),
    plot.title = element_text(size=8, face="bold", hjust=0.5),
    axis.title = element_text(size=7),
    axis.text = element_text(size=7),
    # axis.line = element_line(color="black", linewidth=0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth=0.25, color="#cccccc"),
    plot.margin = margin(10,10,10,10, unit="pt"),
    strip.text = element_text(size = 7, face = "bold"),
    axis.line = element_line(linewidth = 0.25, colour = "black"),
    axis.ticks = element_line(linewidth = 0.25, colour = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white",color = "white")
  )

theme_set(theme_common)

# # Example usage:
# ggplot(mtcars, aes(x=wt, y=mpg)) +
#   geom_point() +
#   labs(title="Scatter plot of MPG vs Weight", x="Weight", y="Miles per Gallon")
