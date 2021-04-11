library(jasmines)
library(dplyr)
library(cowplot)
library(magick)
# Circle base
use_seed(2021) %>%
  entity_circle(grain = 1000) %>%
  unfold_tempest(iterations = 10) %>%
  style_ribbon(background = "wheat")
# Rainbow Circle
use_seed(2021) %>%
  entity_circle(grain = 1000, size = 2) %>%
  unfold_warp(iterations = 100) %>%
  style_ribbon(palette = "rainbow")
# Heart
use_seed(2021) %>%
  entity_heart(grain = 1000) %>%
  unfold_tempest(iterations = 100) %>%
  style_ribbon(
    palette = "base", 
    colour = "ind", 
    background = "mistyrose"
  ) %>%
  style_overlay(border = "white")
# Disc 
p <- use_seed(2021) %>%
  scene_discs(
    rings = 3, points = 5000, size = 5
  ) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 5,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_tempest(
    iterations = 10,
    scale = .1
  ) %>%
  style_ribbon(
    palette = palette_named("blood"),
    colour = "#7A6214",
    alpha = c(.3,.1),
    background = "black"
  )
cowplot::ggdraw(p) +
  theme(plot.background = element_rect(fill = "black"),
        # plot.caption.position = "plot",
        plot.margin = unit(c(t = -10, r = -10, b = -10, l = -10), units = "mm")) +
  cowplot::draw_label("Made by Duncan Gates • @gates_duncan", color = "#660000", fontfamily = "Sabon Roman", size = 6, x = 0.86, y = 0.059)
        # plot.caption = element_text(color = "#740707", family = "Sabon Roman", hjust = 0.89, size = 6))
ggsave(here("Images/WrappedinBlood.png"))

# Definitely somewhere to go with this "circle of blood"
use_seed(2021) %>%
  entity_circle(grain = 1000) %>%
  unfold_loop(radius = 2, points = 100) %>%
  style_ribbon(palette = "blood") #%>%
  # style_overlay(border = "black")
# Some bubbles
use_seed(2021) %>%
  scene_bubbles(n = 6, grain = 1000) %>%
  # mutate(ind = 1:n()) %>%
  unfold_loop(radius = 2) %>%
  unfold_tempest(scatter = T) %>%
  style_ribbon(palette = "ropensci",
               alpha = c(0.3, 0.1),
               type = "curve")
# Gaussian stuff
use_seed(2021) %>%
  entity_gaussian(grain = 100) %>%
  unfold_loop(points = 300) %>%
  style_ribbon(palette = "bilbao",
               alpha = c(1, 0.05),
               type = "point",
               background = "floralwhite")
# Attempt above with droplet
use_seed(2021) %>%
  entity_droplet(grain = 1000) %>%
  unfold_loop(points = 50,
              radius = 2) %>%
  style_ribbon(palette = "bilbao",
               alpha = c(0.5, 0.05),
               type = "segment",
               background = "floralwhite")
ggsave(here("Images/StrangeDroplet.png"), bg = "floralwhite")
  
# New stuff with scenes
use_seed(2021) %>%
  scene_discs(
    rings = 5, points = 1000, size = 2
  ) %>%
  mutate(ind = row_number()) %>%
  unfold_loop() %>%
  unfold_tempest() %>%
  style_ribbon(
    palette = palette_named("berlin"),
    colour = "#7A6214",
    alpha = c(.3,.1),
    background = "floralwhite",
    type = "curve"
  )
ggsave(here("Images/Loops.png"), bg = "floralwhite")

p <- use_seed(2021) %>%
  scene_sticks(
    n = 1, grain = 1000
  ) %>%
  unfold_loop(points = 40) %>%
  unfold_inside() %>%
  style_ribbon(
    palette = palette_named("hawaii"),
    colour = "#7A6214",
    alpha = c(.75,.075),
    background = "midnightblue",
    type = "curve"
  )

# Here is each of the shapes base
# Circle
use_seed(1) %>%
  entity_circle(grain = 1000) %>%
  unfold_warp(iterations = 10) %>%
  style_ribbon(palette = "blood")
# Heart
use_seed(1) %>%
  entity_heart(grain = 1000) %>%
  unfold_warp(iterations = 10) %>%
  style_ribbon(palette = "blood")
# Droplet
use_seed(1) %>%
  entity_droplet(grain = 1000) %>%
  unfold_warp(iterations = 10) %>%
  style_ribbon(palette = "blood")
# Line
use_seed(1) %>%
  entity_line(grain = 1000) %>%
  unfold_warp(iterations = 20) %>%
  style_ribbon(palette = "blood")
# Lissajous
use_seed(1) %>%
  entity_lissajous(grain = 3000) %>%
  unfold_warp(iterations = 20) %>%
  style_ribbon(palette = "blood")
# Gaussian
use_seed(1) %>%
  entity_gaussian(grain = 3000) %>%
  unfold_warp(iterations = 20) %>%
  style_ribbon(palette = "blood")
