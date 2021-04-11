#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(generativeart)
library(tidyverse)

# set the paths
IMG_DIR <- here("GenerativeArt/img/")
IMG_SUBDIR <- "shiny/"
IMG_PATH <- paste0(IMG_DIR, IMG_SUBDIR)

LOGFILE_DIR <- here("GenerativeArt/shinylogfile/")
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)


# include a specific formula, for example:
my_formula <- list(
    x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
    y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$image <- renderImage({
        # Read image's width and height. These are reactive values, so this
        # expression will re-run whenever these values change.
        # width  <- session$clientData$output_image_width
        # height <- session$clientData$output_image_height
        
        # call the main function to create five images with a polar coordinate system
        generativeart::generate_img(formula = my_formula, nr_of_img = 1, polar = TRUE,
                                    filetype = "png", color = "#829079", background_color = "#ede6b9")
        # Find most recently generated file
        file_df <- file.info(list.files(IMG_PATH, full.names = T))
        thefile <- file_df %>% arrange(desc(mtime)) %>% slice(1) %>% rownames()
        # Write list with generated image name
        # Return a list containing the filename
        list(src = thefile,
             width = "100%",
             height = "100%",
             alt = "Check out this Art!")
    }, deleteFile = F)

})
