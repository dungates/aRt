library(imager)
library(dplyr)
library(deldir)
library(ggplot2)
library(scales)
library(magick)
library(here)

# Download image
file <- "https://d2r55xnwy6nx47.cloudfront.net/uploads/2019/04/Bolt_2880x1620_Lede.jpg"
download.file(file, destfile = here("Images/Bolt.jpg"), mode = "wb")

# Load file
# img <- load.image(here("Images/Bolt.jpg"))
# Load second image
img <- load.image(here("Images/Dunking.jpg"))

# Convert to grayscale
(x <- img %>% grayscale(method = "Luma"))
# (x <- img2 %>% grayscale())

# Define frame limits
rw <- x %>%
  as.data.frame() %>%
  group_by() %>%
  summarize(xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y)) %>%
  as_vector()

# Filter image to convert to black and white
df <- x %>%
  threshold("45%") %>%
  as.cimg() %>%
  as.data.frame()

# Function to compute and plot Voronoi tesselation of a given sample size
doPlot <- function(n) {
  # Voronoi tesselation
  data <- df %>%
    sample_n(n, weight = (1 - value)) %>%
    select(x, y) %>%
    deldir(rw = rw, sort = TRUE) %>%
    .$dirsgs

  # This is just to add some alpha to lines depending on its longitude
  data <- data %>%
    mutate(
      long = sqrt((x1 - x2)^2 + (y1 - y2)^2),
      alpha = findInterval(long, quantile(long, probs = seq(0, 1, length.out = 20))) / 21
    )

  # A little bit of ggplot to plot results
  plot <- data %>%
    ggplot(aes(alpha = (1 - alpha))) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = "black", lwd = 1) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), trans = reverse_trans()) +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "white"),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    )

  return(plot)
}

doPlot(90000)
  

ggsave(here("Images/DunkingTesselated.png"))
