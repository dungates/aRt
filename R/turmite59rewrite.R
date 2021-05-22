library(here)
library(tidyverse)
library(turmite59)

turmite <- function(width, height, iter, step_size) {
  .Call("_turmite59_turmite", width, height, iter, step_size)
}

turmite60 <- function(
  shade,
  file = NULL,
  image_x = NULL,
  image_y = NULL,
  image_dpi = NULL,
  palette_n = NULL,
  palette_heart = NULL,
  palette_turmite = NULL,
  alpha_turmite = NULL,
  alpha_heart = NULL,
  heart_shift_x = NULL,
  heart_shift_y = NULL,
  heart_grain = NULL,
  heart_size = NULL,
  heart_jitter = NULL,
  slice_scale = NULL,
  slice_n = NULL,
  unfold_iter = NULL,
  unfold_scale = NULL,
  unfold_drift = NULL,
  unfold_fractal = NULL,
  unfold_octaves = NULL,
  turmite_grid_x = NULL,
  turmite_grid_y = NULL,
  turmite_steps = NULL,
  turmite_stepsize = NULL) {
  
  # control RNG seed
  seed <- sum(grDevices::col2rgb(shade)) + 1
  set.seed(seed)
  
  # constants
  param <- list(
    seed = seed,
    
    # colours
    palette_n = palette_n %||% 1000,
    palette_turmite = NA,
    palette_heart = NA,
    palette_background = shade,
    
    # transparency
    alpha_turmite = alpha_turmite %||% 0.5,
    alpha_heart = alpha_heart %||% function(x) exp(-(x - 1) / 20),
    
    # image size
    image_x = image_x %||% 16,
    image_y = image_y %||% 16,
    image_dpi = image_dpi %||% 100,
    
    # base heart
    heart_shift_x = heart_shift_x %||% .3,
    heart_shift_y = heart_shift_y %||% .4,
    heart_grain = heart_grain %||% 1000,
    heart_size = heart_size %||% stats::runif(1, min = .2, max = .5),
    heart_jitter = heart_jitter %||% 1 / 500,
    
    # heart slice operation
    slice_scale = slice_scale %||% .5 * 10^-24,
    slice_n = slice_n %||% sample(6:12, 1),
    
    # heart unfold operation
    unfold_iter = unfold_iter %||% 100,
    unfold_scale = unfold_scale %||% .0002,
    unfold_drift = unfold_drift %||% .0005,
    unfold_fractal = unfold_fractal %||% ambient::ridged,
    unfold_octaves = unfold_octaves %||% 8,
    
    # parameters describing the turmite background
    turmite_grid_x = turmite_grid_x %||% 2000,
    turmite_grid_y = turmite_grid_y %||% 2000,
    turmite_steps = turmite_steps %||% 10000000,
    turmite_stepsize = turmite_stepsize %||% 3
  )
  
  # generate the palettes
  param$palette_turmite <- palette_turmite %||% sample_palette(param$palette_n + 1)
  param$palette_heart <- palette_heart %||% grDevices::adjustcolor(
    col = param$palette_turmite,
    offset = c(0.5, 0.5, 0.5, 0),
    transform = diag(c(.7, .7, .7, 1))
  )
  
  
  cat("turmite wandering...\n")
  raster <- make_background(param)
  
  cat("dust heart beating...\n")
  dat <- make_hearts(param)
  
  cat("image rendering...\n")
  make_ggplot(param, raster, dat)
  # ggplot2::ggsave(
  #   filename = make_filename(file, shade),
  #   plot = make_ggplot(param, raster, dat),
  #   width = param$image_x,
  #   height = param$image_y,
  #   dpi = param$image_dpi
  # )
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

sample_palette <- function(n) {
  pal <- paletteer::palettes_c_names
  ind <- sample(nrow(pal), 1)
  pnm <- paste0(pal[ind, 1], "::", pal[ind, 2])
  return(paletteer::paletteer_c(pnm, n))
}


# turmite background ------------------------------------------------------

make_background <- function(param) {
  ar <- param$turmite_grid_y / param$turmite_grid_x
  raster <- ambient::long_grid(
    x = seq(0, 1, length.out = param$turmite_grid_x),
    y = seq(0, ar, length.out = param$turmite_grid_y)
  )
  
  grid <- turmite(
    width = param$turmite_grid_x,
    height = param$turmite_grid_y,
    iter = param$turmite_steps,
    step_size = param$turmite_stepsize
  )
  grid <- t(grid)
  inds <- 1 + ceiling(param$palette_n * grid / param$turmite_steps)
  raster$shade <- param$palette_turmite[inds]
  
  return(raster)
}


# bleeding heart ----------------------------------------------------------

make_hearts <- function(param) {
  dat <- jasmines::use_seed(param$seed) %>%
    jasmines::entity_heart(
      grain = param$heart_grain,
      size = param$heart_size
    ) %>%
    dplyr::mutate(ind = dplyr::row_number()) %>%
    jasmines::unfold_slice(
      iterations = param$slice_n,
      scale = param$slice_scale,
      scatter = TRUE,
      output1 = "id"
    ) %>%
    dplyr::mutate(
      x = x + stats::rnorm(dplyr::n()) * param$heart_jitter,
      y = y + stats::rnorm(dplyr::n()) * param$heart_jitter
    ) %>%
    jasmines::unfold_breeze(
      iterations = param$unfold_iter,
      scale = param$unfold_scale,
      drift = param$unfold_drift,
      fractal = param$unfold_fractal,
      octaves = param$unfold_octaves
    ) %>%
    jasmines::unfold_inside() %>%
    dplyr::mutate(val = 1 + (inside > 0) * ind)
  
  dat$val <- ambient::normalise(x = dat$val, to = c(1, param$palette_n + 1))
  dat$val <- round(dat$val)
  dat$shade <- param$palette_heart[dat$val]
  dat$x <- (dat$x + param$heart_shift_x)
  dat$y <- (dat$y + param$heart_shift_y)
  
  return(dat)
}



# plot and save -----------------------------------------------------------

make_filename <- function(file, shade) {
  if (is.null(file)) {
    file <- paste0("turmite_59_", gsub("#", "", shade), ".png")
  }
  return(file)
}

make_ggplot <- function(param, raster, dat) {
  ggplot2::ggplot(
    data = raster,
    mapping = ggplot2::aes(x, y, fill = shade)
  ) +
    # the raster object forms the background
    ggplot2::geom_raster(alpha = param$alpha_turmite) +
    
    # the heart is made of dust/points
    ggplot2::geom_point(
      data = dat,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        color = shade,
        alpha = param$alpha_heart(time)
      ),
      inherit.aes = FALSE,
      show.legend = FALSE,
      size = .5
    ) +
    
    # bunch of settings...
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    # ggtext::geom_richtext(aes(label = "Made by Duncan Gates • @gates_duncan", x = 0.9, y = 0.05),
    #                       family = "Roboto Condensed", size = 6, color = "white",
    #                       inherit.aes = F, show.legend = F) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = param$palette_background,
        color = param$palette_background
      )
    )
}

img <- turmite60(shade = "#9EB0FF", slice_n = 3, heart_size = 0.41, heart_shift_x = 0.49, heart_shift_y = 0.55, palette_n = 30)


