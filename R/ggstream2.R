library(ggstream)
library(ggfx)
library(here)
library(tidyverse)
library(grid)
library(raster)

palette <- colorRampPalette(c("#0D3E80ff", "#99719Fff", "#47060Dff", "#B0613Cff", "#DBB832ff"))


data_maker <- function(n_fill, y_change = c(5, 10)) {
  data <- tibble(
    fill = paste0("fill", 1:n_fill),
    n = 1:n_fill,
    y = sample(x = c(y_change), size = n_fill, replace = T)
  )
}

g <- rasterGrob(t(rev(palette(20))), width=unit(1,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE)

test <- data_maker(n_fill = 20, y_change = c(20, 100))

ggplot(test, aes(n, y, fill = fill)) +
  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_stream(alpha = 0.88) +
  coord_flip() +
  scale_fill_manual(values = palette(20)) +
  theme_void() +
  theme(legend.position = "none")

ggsave(here("Images/PalettePillar.png"), bg = "grey50")



# ggplot(blockbusters, aes(year, box_office, fill = genre)) +
#   geom_stream(alpha = 0.88) +
#   coord_flip() +
#   scale_fill_manual(values = palette(10)) +
#   theme_void() +
#   theme(legend.position = "none")
