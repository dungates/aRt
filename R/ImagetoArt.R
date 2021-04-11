library(imager)
library(tidyverse)     
library(ggvoronoi)
library(kableExtra)
library(here)
library(extrafont)
library(ggtext)

img <- load.image(here("Images/three_sisters.jpeg"))

img_df <- as.data.frame(img)

img_df %>% 
  arrange(x, y, cc) %>%
  filter(row_number() < 10) %>%
  kable("html") %>%
  kable_styling(full_width = F)

img_df <- img_df %>% 
  mutate(channel = case_when(
    cc == 1 ~ "Red",
    cc == 2 ~ "Green", 
    cc == 3 ~ "Blue"
  ))  

img_wide <- img_df %>%
  select(x, y, channel, value) %>%
  spread(key = channel, value = value) %>%
  mutate(
    color = rgb(Red, Green, Blue)
  )

sample_size <- 10000
img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]
img_sample$size <- runif(sample_size)



ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color, size = size)) +
  guides(size = FALSE) +
  labs(caption = "The Three Sisters<br><br><br><span style='font-size:12pt;'><span style='color:#9DABAC;'>#30DayChartChallenge | @gates_duncan</span></span>") +
  scale_color_identity() +
  scale_y_reverse() +
  theme_void() +
  theme(plot.caption = element_markdown(family = "Airborne II Pilot", hjust = 0.5, size = 24, margin = margin(20,0,10,0)),
        plot.margin = margin(20,20,20,20),
        plot.background = element_rect(fill = NA, colour = 'grey40', size = 2))

ggsave(here("Images/three_sisters_abstract.png"), plot = last_plot(), height = 264, width = 268, units = "mm", dpi = 600)

