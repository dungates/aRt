library(here)
library(generativeart)

# set the paths
IMG_DIR <- here("GenerativeArt/img/")
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, IMG_SUBDIR)

LOGFILE_DIR <- here("GenerativeArt/logfile/")
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)

# create the directory structure
generativeart::setup_directories(IMG_DIR, IMG_SUBDIR, IMG_SUBDIR2, LOGFILE_DIR)

# include a specific formula, for example:
my_formula <- list(
  x = quote(runif(5, -5, 5) * x_i^2 + y_i^3 + log(x_i + y_i)),
  y = quote(runif(5, -5, 5) * x_i^2 - y_i^3 + log(x_i + y_i))
)

# call the main function to create five images with a polar coordinate system
generativeart::generate_img(formula = my_formula, nr_of_img = 5, polar = TRUE,
                            filetype = "png", color = "#d9a5b3", background_color = "floralwhite")
