library(tidyr)
library(purrr)
library(ggplot2)
library(extrafont)
library(ggtext)
library(scico)
library(ggfx)

generate_points <- function(grid_size, n_points) {
  # Generate random numbers to create probabiblity weights
  raw_p <- c(sample(c(1:1e6), grid_size))
  # Create grid layout
  group_points <- data.frame(expand.grid(1:sqrt(grid_size), 1:sqrt(grid_size)),
    # Assigin p for each grid square
    p = raw_p / sum(raw_p)
  )
  # Generate points location
  pmap_dfr(group_points, function(Var1, Var2, p) {
    points_on_grid <- data.frame(
      ypos = Var1 - runif(n = p * n_points),
      xpos = Var2 - runif(n = p * n_points)
    )
    return(points_on_grid)
  }) %>%
    mutate(color = ntile(n = grid_size),
           size = ntile(n = 1:10))
}

grid_points <- generate_points(grid_size = 1024, n_points = 1e5)

# col <- colorRampPalette(c(pal_uchicago("light")(9)))

ggplot(grid_points) +
  with_outer_glow(
    geom_point(aes(x = xpos, y = ypos, color = factor(color)), alpha = 0.33, size = 2),
    color = "white"
  ) +
  # scale_color_manual(values = col(49)) +
  scale_color_manual(values = scico(1024, palette = "berlin")) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  theme(
    plot.caption = element_markdown(hjust = 0.5, size = 10, color = "gray45", family = "Arvo"),
    plot.margin = margin(2, 2, 4, 2, "mm"),
    legend.position = "none"
  )

ggsave(here("Images/squares.png"), width = 14, height = 14)

knitr::plot_crop(here("Images/squares.png"))
