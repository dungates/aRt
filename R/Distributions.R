library(tidyverse)
library(here)
library(scales)
library(ggfx)
library(ggtext)
library(glue)
library(colorspace)


theme_jk <- function(base_family="Oswald",
                     base_size = 11,
                     strip_text_family = base_family,
                     strip_text_size = 12,
                     plot_title_family = "Oswald",
                     plot_title_size = 18,
                     plot_title_margin = 10,
                     subtitle_family = "Lato",
                     subtitle_size = 12,
                     subtitle_margin = 15,
                     caption_family = "Lato",
                     caption_size = 9,
                     caption_margin = 10,
                     axis_title_family = "Oswald",
                     axis_title_size = 9,
                     axis_title_just = "mm",
                     dark = FALSE,
                     grid = TRUE,
                     axis = FALSE,
                     ticks = FALSE,
                     markdown = FALSE) {
  
  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)
  
  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())
  
  
  if (dark == TRUE) {
    
    ret <- ret + ggplot2::theme(plot.background = ggplot2::element_rect(fill ="#2E3440"),
                                text = ggplot2::element_text(color = "white"),
                                strip.text = ggplot2::element_text(color = "white"))
    
    grid_color <- "#E5E9F0"
    tick_color = "#E5E9F0"
    
  } else {
    
    grid_color <- "#cccccc"
    tick_color <- "black"
  }
  
  if (inherits(grid, "character") | grid == TRUE) {
    
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = grid_color, size = 0.05))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
    
  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  
  if (inherits(axis, "character") | axis  ==  TRUE) {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(color = grid_color, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }
  
  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks  =  ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length  =  grid::unit(5, "pt"))
  }
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  
  ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size/2)))
  ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size/2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))
  
  if(!markdown) {
    
    ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size/2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size/2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))
    
    ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
    
  } else {
    
    ret <- ret + ggplot2::theme(axis.text.x = ggtext::element_markdown(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size/2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggtext::element_markdown(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size/2))) + ggplot2::theme(axis.title = ggtext::element_markdown(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggtext::element_markdown(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggtext::element_markdown(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggtext::element_markdown(hjust = 0, size = strip_text_size, family = strip_text_family))
    
    ret <- ret + ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggtext::element_markdown(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggtext::element_markdown(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
    
  }
  
  ret <- ret + ggplot2::theme(plot.margin = ggplot2::margin(base_size/2, base_size/2, base_size/2, base_size/2))
  
  ret
  
}

data <- tibble(left =  rbeta(10000,5,2),
               right = rbeta(10000,2,5),
               centre = rbeta(10000,5,5)) %>%
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels =  c("left", "centre", "right"))) %>%
  arrange(name)


labels <- tibble(label = c("skewed left", "skewed right", "symmetric"),
                 name = c("left", "right", "centre"),
                 x = c(0.5, 0.5, 0.5),
                 y = 0.01) %>%
  mutate(name = factor(name, levels =  c("left", "centre", "right")))


plot <- ggplot(data) +
  as_reference(
    geom_text(data = labels, aes(label = toupper(label), x = x, y = y), size = 60, family = "Anton", color = "#FBFEF9", hjust = 0.5, vjust = 0),
    id = "text") +
  with_blend(
    geom_density(aes(x = value, fill = name, color = name), show.legend = FALSE),
    bg_layer = "text",
    blend_type = "xor") +
  labs(x = NULL,
       y = NULL,
       title = "The Shape of Distributions") +
  facet_wrap(~name, ncol = 1) +
  scale_x_continuous(limits = c(-0.1, 1.1)) +
  scale_fill_manual(values = c("#0081a7","#00afb9","#f07167")) +
  scale_color_manual(values = darken(c("#0081a7","#00afb9","#f07167"))) +
  theme_jk(dark = TRUE,
           grid = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        # plot.caption = element_markdown(),
        plot.title = element_markdown(hjust = 0.5, family = "Alata"))


ggsave(here("Images", "Distributions.png"), plot, height = 9, width = 20, units = 'in', device = ragg::agg_png())
