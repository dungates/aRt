library(tidyverse)
library(scico)


load_shapes <- function(height, width, randomness = runif(1, 0, 1), palette) {
  shapes <- data.frame(
    height = height
  ) %>%
    mutate(
      r = round(row_number() * randomness),
      c = list(c(1:width))
    ) %>% 
    unnest(c(c, r)) %>% 
    rowwise %>% 
    mutate(
      x = list(c + c(0, randomness, randomness, 0)),
      y = list(c * 0.5 + c(height - randomness, 
                           height, 
                           height + randomness, 
                           height + randomness)),
      fill = palette %>% sample(size = 1),
      color = palette %>% sample(size = 1)
    ) %>% 
    unnest(c(x, y))
}

test <- load_shapes(height = 100, width = 10,
                    palette = scico::scico(palette = "berlin", n = 30))


ggplot() +
  geom_polygon(data = test, aes(x = x, y = y, 
                                  group = interaction(c, r), 
                                  fill = fill, color = color)) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_void() +
  theme()
