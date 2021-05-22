library(ggstream)
library(ggfx)
library(here)
library(tidyverse)
library(grid)
library(raster)
library(nord)

ggstream2 <- function(n_fill, y_range, palette, flip = F, bw = 0.65, geom = c("polygon", "line", "point"), size = 0.2, color = "white") {
  palette <- colorRampPalette(c(palette))
  
  
  data_maker <- function(n_fill, y_range = c(5, 10)) {
    data <- tibble(
      fill = paste0("fill", 1:n_fill),
      n = 1:n_fill,
      # y = sample(x = c(y_range), size = n_fill, replace = T)
      # y = rep(c(1,2), n_fill/2)
      # y = c(max(y_range), sample(x = y_range, size = n_fill-2, replace = T), max(y_range))
      y = runif(n_fill, min = y_range[1], max = y_range[2]),
      text_location_y = mean(y),
      text_location_x = mean(n)
    )
  }
  
  # if(flip == T) {
  #   g <- rasterGrob(rev(palette(20)), width=unit(1,"npc"), height = unit(1,"npc"), 
  #                   interpolate = TRUE)
  # } else {
  #   g <- rasterGrob(t(rev(palette(20))), width=unit(1,"npc"), height = unit(1,"npc"), 
  #                   interpolate = TRUE)
  # }
  
  test <- data_maker(n_fill = n_fill, y_range = y_range)
  
  p <- ggplot(test, aes(n, y, fill = fill)) +
    # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    as_reference(
      geom_text(aes(x = text_location_x, y = text_location_y-0.5, label = "Happy\nMother's\nDay!", color = fill),
                size = 35, fontface = "bold", family = "Fira Sans"),
      id = "text"
    ) +
    # with_blend(
    #   geom_stream(alpha = 0.88, sorting = "onset", bw = bw, type = "proportional", geom = "polygon"),
    #   bg_layer = "text",
    #   blend_type = "overlay"
    # ) +
    with_shadow("text", sigma = 3) +
    geom_stream(alpha = 0.88, sorting = "onset", bw = bw, type = "proportional", geom = "polygon") +
    # geom_stream(alpha = 0.15, sorting = "onset", bw = bw, type = "proportional",
    #             geom = geom, color = color, size = size, aes(y = y - y/2, linetype = "dashed")) +
    scale_fill_manual(values = palette(n_fill)) +
    scale_color_manual(values = palette(n_fill+1)[-(1)]) +
    theme_void() +
    theme(legend.position = "none")
  
  if (flip == T) {
     p <- p + coord_flip()
  }
  
  return(p)
}

# ggstream2(n_fill = 30, y_range = 1:3, palette = nord_palettes$baie_mouton,
#           flip = T)
# 
# ggsave(here("Images/cool_stream3.png"), height = 12, width = 8)
# 
# knitr::plot_crop(here("Images/cool_stream3.png"))

