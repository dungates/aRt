
ft__check_not_null <- function(x, name) {
  if(is.null(x)) {
    stop("`", name, "` must not be null", call. = FALSE)
  }
}

# NOTE: don't forget that is.na(NaN) returns TRUE
ft__check_not_na <- function(x, name) {
  if(any(is.na(x))) {
    stop("`", name, "` must not contain missing values", call. = FALSE)
  }
}

ft__check_numeric <- function(x, name) {
  if(!is.numeric(x)) {
    stop("`", name, "` must be numeric", call. = FALSE)
  }
}

# NOTE: is.integer(1) returns FALSE, ft__check_soft_integer lets this through
ft__check_soft_integer <- function(x, name) {
  ft__check_numeric(x, name)
  weird <- is.na(x) | is.infinite(x)
  if(any(weird) & !is.integer(x[weird])) {
    stop("`", name, "` must be integer valued", call. = FALSE)
  }
  if(any(x[!weird] != as.integer(x[!weird]))) {
    stop("`", name, "` must be integer valued", call. = FALSE)
  }
}



# checks that are tested only indirectly ----------------------------------

ft__check_length_exact <- function(x, name, len) {
  if(length(x) != len) {
    stop("`", name, "` must have length ", len, call. = FALSE)
  }
}

ft__check_length_minimum <- function(x, name, len) {
  if(!(length(x) >= len)) {
    stop("`", name, "` must be at least length ", len, call. = FALSE)
  }
}

ft__check_value_minimum <- function(x, name, val) {
  ft__check_numeric(x, name)
  if(any(x < val)) {
    stop("`", name, "` cannot be less than ", val, call. = FALSE)
  }
}

ft__check_value_maximum <- function(x, name, val) {
  ft__check_numeric(x, name)
  if(any(x > val)) {
    stop("`", name, "` cannot be greater than ", val, call. = FALSE)
  }
}

spark_linear <- function(x = 0, y = 0, tree = 0, time = 0, constant = 0) {
  function(coord_x, coord_y, id_tree, id_time) {
    (x * coord_x) + (y * coord_y) + (tree * id_tree) + (time * id_time) + constant
  }
}

spark_decay <- function(x = 0, y = 0, tree = 0, time = 0, multiplier = 2, constant = 0) {
  function(coord_x, coord_y, id_tree, id_time) {
    multiplier * exp(-abs((x * coord_x) + (y * coord_y) + (tree * id_tree) + (time * id_time))^2) + constant
  }
}

spark_random <- function(multiplier = 3, constant = 0) {
  function(coord_x, coord_y, id_tree, id_time) {
    stats::runif(1, min = -multiplier/2, max = multiplier/2) + constant
  }
}

spark_nothing <- function() {
  function(coord_x, coord_y, id_tree, id_time) {
    0
  }
}

flametree_grow <- function(
  seed = 286,
  time = 6,
  scale = c(.6, .8, .9),
  angle = c(-10, 10, 20),
  split = 2,
  trees = 1,
  seg_col = spark_linear(tree = 2, time = 1),
  seg_wid = spark_decay(time = .3, multiplier = 5, constant = .1),
  shift_x = spark_random(multiplier = 3),
  shift_y = spark_nothing()
) {
  
  # collect parameters into a list
  options <- list(
    seed = seed,    # seed for the RNG
    time = time,    # time (iterations) to grow the tree
    scale = scale,  # possible values for rescaling at each time
    angle = angle,  # possible values for redirect at each time
    split = split,  # number of splits at each time point
    trees = trees,  # number of trees to include
    shift_x = shift_x, # function to control horizontal jitter
    shift_y = shift_y, # function to control vertical jitter
    seg_col = seg_col, # function to control segment colour
    seg_wid = seg_wid # function to control segment width
  )
  ft__check_opts(options)
  
  set.seed(options$seed)
  seeds <- sample(100000, size = options$trees)
  trees <- purrr::map2_dfr(
    .x = 1:options$trees,
    .y = seeds,
    .f = ~ ft__grow_tree(options, .x, .y)
  )
  attr(trees, "options") <- options
  return(trees)
}



# to grow the whole tree we need to "accumulate" the growth: starting with
# the sapling (a single shoot) we grow the second layer; the set of shoots
# that make the second layer are then used to grow the third later; and so on
ft__grow_tree <- function(param, id, local_seed) {
  
  set.seed(local_seed)
  
  tree <- purrr::accumulate(
    .x = 1:param$time,
    .f = ft__grow_layer,
    .init = ft__grow_sapling(),
    param = param,
    id = id
  )
  
  tree <- tree %>%
    ft__shape_tree(id) %>%
    dplyr::mutate(
      coord_x = coord_x + param$shift_x(coord_x, coord_y, id, id_time),
      coord_y = coord_y + param$shift_y(coord_x, coord_y, id, id_time),
      seg_col = param$seg_col(coord_x, coord_y, id, id_time),
      seg_wid = param$seg_wid(coord_x, coord_y, id, id_time)
    )
  
  return(tree)
}



# to grow a "layer" of the shrub, we extend (and possibly prune) each
# existing shoot multiple times
ft__grow_layer <- function(shoots, time, param, id) {
  
  new_shoots <- purrr::map_dfr(
    .x = 1:param$split,
    .f = ft__grow_shoots,
    shoots = shoots,
    param = param
  )
  return(new_shoots)
}



# for each existing shoot on the tree, grow an additional shoot that
# extends it; then prune some of them away
ft__grow_shoots <- function(time, shoots, param) {
  
  n_shoots <- nrow(shoots)
  
  ch_seg_len <- sample(x = param$scale, size = n_shoots, replace = TRUE)
  ch_seg_deg <- sample(x = param$angle, size = n_shoots, replace = TRUE)
  
  shoots <- shoots %>%
    dplyr::mutate(
      x_0 = x_2,
      y_0 = y_2,
      seg_len = seg_len * ch_seg_len,
      x_1 = x_0 + ft__extend_x(seg_len/2, seg_deg),
      y_1 = y_0 + ft__extend_y(seg_len/2, seg_deg),
      seg_deg = seg_deg + ch_seg_deg,
      id_time = id_time + 1L,
      x_2 = x_0 + ft__extend_x(seg_len, seg_deg) ,
      y_2 = y_0 + ft__extend_y(seg_len, seg_deg),
    )
  
  return(shoots)
}



# the very first shoot is the "sapling"
ft__grow_sapling <- function() {
  
  sapling <- tibble::tibble(
    x_0 = 0, y_0 = 0,  # first shoot starts at origin
    x_1 = 0, y_1 = .5, # first shoot guide is its midpoint
    x_2 = 0, y_2 = 1,  # first shoot grow to y = 1
    seg_deg = 90,      # segment orientation is vertical
    seg_len = 1,       # segment length is 1
    id_time = 1L       # the acorn grows at "time 1"
  )
  return(sapling)
}



# the data structure that we used to grow the tree is designed to allow
# efficient computation, but is not optimal for ggplot2 so it needs to
# be reshaped into a convenient form
ft__shape_tree <- function(tree, id) {
  
  tree <- tree %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id_path = as.integer(1:dplyr::n())) %>%
    tidyr::pivot_longer(
      cols = x_0:y_2,
      names_to = "id_step",
      values_to = "coord"
    ) %>%
    tidyr::separate(col = id_step, into = c("axis", "id_step")) %>%
    tidyr::pivot_wider(names_from = axis, values_from = coord) %>%
    dplyr::mutate(
      id_step = as.integer(id_step),
      id_leaf = id_time == max(id_time),  # adds leaf node indicator
      id_tree = id,                       # adds tree identifier
      id_pathtree = paste(id_tree, id_path, sep = "_")
    ) %>%
    dplyr::rename(coord_x = x, coord_y = y) %>%
    dplyr::select(coord_x, coord_y, id_tree, id_time, id_path, id_leaf,
                  id_pathtree, id_step, seg_deg, seg_len)
  
  return(tree)
}


# convert an angle from degrees to radians
ft__radians <- function(degree) {
  2 * pi * degree / 360
}

# horizontal distance
ft__extend_x <- function(distance, angle) {
  distance * cos(ft__radians(angle))
}

# vertical distance
ft__extend_y <- function(distance, angle) {
  distance * sin(ft__radians(angle))
}



# checks user input and throws error message if
ft__check_opts <- function(x) {
  
  # seed must be a single integer value
  ft__check_not_null(x$seed, "seed")
  ft__check_not_na(x$seed, "seed")
  ft__check_soft_integer(x$seed, "seed")
  ft__check_length_exact(x$seed, "seed", 1)
  
  # time must be a single positive integer
  ft__check_not_null(x$time, "time")
  ft__check_not_na(x$time, "time")
  ft__check_soft_integer(x$time, "time")
  ft__check_length_exact(x$time, "time", 1)
  ft__check_value_minimum(x$time, "time", 1)
  
  # scale values must be non-negative numbers
  ft__check_not_null(x$scale, "scale")
  ft__check_not_na(x$scale, "scale")
  ft__check_length_minimum(x$scale, "scale", 1)
  ft__check_value_minimum(x$scale, "scale", 0) # also checks numeric
  
  # angle values must be numeric (note: range of angles is not restricted)
  ft__check_not_null(x$angle, "angle")
  ft__check_not_na(x$angle, "angle")
  ft__check_numeric(x$angle, "angle")
  ft__check_length_minimum(x$angle, "angle", 1)
  
  # split must be a single positive integer
  ft__check_not_null(x$split, "split")
  ft__check_not_na(x$split, "split")
  ft__check_soft_integer(x$split, "split")
  ft__check_length_exact(x$split, "split", 1)
  ft__check_value_minimum(x$split, "split", 1)
  #
  # # prune must be numeric between 0 and 1
  # ft__check_not_null(x$prune, "prune")
  # ft__check_not_na(x$prune, "prune")
  # ft__check_length_exact(x$prune, "prune", 1)
  # ft__check_value_minimum(x$prune, "prune", 0)
  # ft__check_value_maximum(x$prune, "prune", 1)
  
}


flametree_plot <- function(
  data,
  background = "black",
  # palette = c("#c06014", "#eddbc0", "#000000", "#cdcdcd"),
  palette = rcartocolor::carto_pal(n = 7, "Earth"),
  style = "plain"
) {
  
  if(style == "plain") return(ft__plot_plain(data, background, palette))
  if(style == "voronoi") return(ft__plot_voronoi(data, background, palette))
  if(style == "wisp") return(ft__plot_wisp(data, background, palette))
  if(style == "nativeflora") return(ft__plot_nativeflora(data, background, palette))
  
  stop('`style` argument must be "plain", "voronoi", "wisp", or "nativeflora', call. = FALSE)
}


ft__plot_plain <- function(data, background, palette) {
  
  # build the ggplot
  picture <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = coord_x,          # x-coordinate
      y = coord_y,          # y-coordinate
      group = id_pathtree,  # each segment/path is a single bezier curve
      size = seg_wid,       # the seg_wid variable is used to set line width
      color = seg_col       # the seg_col variable is used to set line colour
    )
  ) +
    ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
    ggplot2::scale_color_gradientn(colours = palette) +
    ggplot2::scale_size_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      )
    )
  
  return(picture)
  
}

ft__plot_voronoi <- function(data, background, palette) {
  
  # "leaf" coordinates are at terminal locations (id_step = 2)
  # on the terminal branches (id_leaf == TRUE) in the tree
  leaf <- data %>%
    dplyr::filter(id_leaf == TRUE, id_step == 2) %>%
    dplyr::select(coord_x, coord_y, id_path)
  
  # remove duplicated rows
  leaf <- leaf[!duplicated(leaf[, c("coord_x", "coord_y")]),, drop = FALSE]
  
  # create the plot...
  picture <- ggplot2::ggplot() +
    
    # tree trunk is drawn using geom_bezier
    ggforce::geom_bezier(
      data = data,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y,
        group = id_pathtree,
        size = seg_wid
      ),
      color = "#3d5941",
      lineend = "round",
      show.legend = FALSE
    ) +
    
    # add voronoi tiles with no perturbation
    ggforce::geom_voronoi_tile(
      data = leaf,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y,
        fill = id_path,
        colour = id_path
      ),
      inherit.aes = FALSE,
      show.legend = FALSE,
      max.radius = .2,
      size = .1
    ) +
    
    ggplot2::scale_fill_gradientn(colours = palette) +
    ggplot2::scale_colour_gradientn(colours = palette) +
    ggplot2::scale_size_identity() +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      ),
      plot.background = ggplot2::element_rect(
        fill = background,
        colour = background
      )
    )
  
  return(picture)
}



ft__plot_wisp <- function(data, background, palette) {
  
  tree_shade <- palette[1]
  leaf_shade <- palette[2]
  background <- background
  
  # "leaf" coordinates are at terminal locations (id_step = 2)
  # on the terminal branches (id_leaf == TRUE) in the tree
  leaf <- data %>% dplyr::filter(id_leaf == TRUE, id_step == 2)
  
  picture <- ggplot2::ggplot() +
    ggforce::geom_bezier(
      data = data,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y,
        size = seg_wid,
        group = id_path
      ),
      colour = tree_shade,
      show.legend = FALSE,
      lineend = "round",
      alpha = 1
    ) +
    ggplot2::geom_point(
      data = leaf,  # add leaves last
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y
      ),
      size = 1,
      colour = leaf_shade,
      stroke = 0,
      alpha = 1,
      show.legend = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(
      fill = background,
      colour = background
    )) +
    ggplot2::scale_size_identity() +
    ggplot2::coord_equal()
  
  return(picture)
}



ft__plot_nativeflora <- function(data, background, palette) {
  
  data <- data %>%
    dplyr::group_by(id_tree) %>%
    dplyr::filter(
      id_path %in% sample(max(id_path), 0.5 * max(id_path)),
      id_time > 2
    ) %>%
    dplyr::ungroup()
  
  leaf <- data %>%
    dplyr::filter(id_time == max(id_time), id_step == 2)
  
  picture <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = coord_x,
      y = coord_y,
      group = id_pathtree,
      colour = id_tree
    )) +
    ggforce::geom_bezier(
      alpha = 1,
      size = 0.3,
      show.legend = FALSE, lineend = "round") +
    ggplot2::geom_point(data = leaf, show.legend = FALSE, size = 1.3, stroke = 0) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_color_gradientn(colours = palette) +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    ggplot2::theme(panel.background = ggplot2::element_rect(
      fill = background,
      colour = background
    ))
  
  return(picture)
}
