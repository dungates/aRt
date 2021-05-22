# library(tidyverse)
# library(ggfx)
# library(httr)
# library(readxl)
# 
# url1 <- "https://www.nahb.org/-/media/NAHB/news-and-economics/docs/housing-economics/hmi/2021-03/table2-national-hmi-history-2021-03.xls"
# invisible(GET(url1, write_disk(tf <- tempfile(fileext = ".xls"))))
# df3 <- read_excel(tf, range = "A3:M40") # read in data range A3:M40
# colnames(df3) <- c("year", month.abb)
# 
# df3 <-
#   df3 %>%
#   reshape2::melt(id.vars = "year") %>%
#   mutate(month = match(variable, month.abb)) %>%
#   mutate(date = as.Date(ISOdate(year, month, 1)))
# 

circle_area <- function(n, min, max, colors = c("red", "dodgerblue")) {
  
  
  new_df <- tibble(
    x = 1:n,
    y = df3 %>% arrange(date) %>% pull(value)
    # y = rnorm(n, sd = n/3, mean = n/10)
  )
  
  middle <- mean(y)
  
  ggplot(data = new_df, aes(x = x, y = y - 50)) +
    with_outer_glow(
      with_inner_glow(
        geom_area(data = filter(new_df, y > 50), fill = "white"),
        colour = colors[1]
      ),
      sigma = 10, colour = colors[1]
    ) +
    with_outer_glow(
      with_inner_glow(
        geom_area(data = filter(new_df, y < 50), fill = "white"), 
        colour = colors[2]
      ),
      sigma = 5, colour = colors[2]
    ) +
    theme(
      axis.ticks = element_blank(),
      panel.background = element_rect(color = NA, fill = "black"),
      plot.background = element_rect(color = NA, fill = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    coord_polar(start = 3)
}

circle_area(n = 444, colors = c("green", "yellow"))
  
# ggplot(data = df3, aes(x = date, y = value - 50)) +
#   with_outer_glow(
#     with_inner_glow(
#       geom_area(data = filter(df3, value > 50), fill = "white"),
#       colour = "goldenrod4"
#     ),
#     sigma = 10, colour = "goldenrod4"
#   ) +
#   with_outer_glow(
#     with_inner_glow(
#       geom_area(data = filter(df3, value < 50), fill = "white"), 
#       colour = "#C0C0C0"
#       ),
#     sigma = 5, colour = "#C0C0C0"
#   ) +
#   theme(
#     axis.ticks = element_blank(),
#     panel.background = element_rect(color = NA, fill = "black"),
#     plot.background = element_rect(color = NA, fill = "black"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank()
#   ) +
#   coord_polar(start = 3)
