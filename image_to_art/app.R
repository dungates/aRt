library(bs4Dash)
library(base64enc)
library(imager)
library(tidyverse)
library(ggvoronoi)
library(here)
library(extrafont)
library(ggtext)
library(glue)
library(shinycssloaders)
library(shinyjs)
library(emoGG)

ui <- dashboardPage(
  useShinyjs(),
  dashboardHeader(title = "Image Art Abstraction Dashboard"),
  dashboardSidebar(
    skin = "light",
    brandColor = "#7759a6",
    title = h4("Image Abstraction", align = "center"),
    textInput("title", "Type Custom Title Here", value = "Enter Custom Title"),
    actionButton("submit_title", "Submit"),
    br(),
    selectInput("shape", "Select a Shape",
                selected = "Circle",
                choices = setNames(
                  c(16, 17, 18, 15, "Laugh"),
                  c("Circle", "Triangle", "Diamond", "Square", emo::ji("laugh"))
                )
    ),
    numericInput("scale", "Change Image Scale (Increase/Decrease Size)",
                 value = 1.5, step = 0.25, min = 0, max = 10
    ),
    sliderInput("sampling", "Increase/Decrease Image Sampling", min = 0, max = 100, post = "%", value = 10),
    sliderInput("sizing", "Increase/Decrease Point Size", min = 0, max = 20, post = "x", value = 1)
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      align = "center",
      column(
        12,
        splitLayout(
          cellWidths = c("50%", "50%"),
          fileInput("upload", HTML("<h1 style = 'font-size:2.25vw'>Upload an image to replace <br>the example, large images <br>may take a while to process.</h1>"), accept = "image/*"),
          withSpinner(uiOutput("image"))
        )
      ),
      column(
        12,
        br(),
        br(),
        # br(),
        withSpinner(plotOutput("plot",
                               width = "900px",
                               height = "900px"
        ))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Change maximum file size
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  base64 <- reactive({
    if (is.null(input$upload)) {
      dataURI(file = here("Images/ExamplePhoto.jpeg"), mime = "image/png")
    } else {
      dataURI(file = input$upload$datapath, mime = "image/png")
    }
  })
  
  output$image <- renderUI({
    req(base64())
    tags$div(
      tags$img(src = base64(), width = "100%"),
      style = "width: 400px;"
    )
  })
  
  image_data <- reactive({
    if (is.null(input$upload)) {
      img_sample <- read_rds(here("Data/example_data.rds"))
      img_sample <- slice_sample(img_sample, prop = (input$sampling / 100))
      img_sample <- img_sample %>%
        mutate(size = size *input$sizing)
    } else {
      # Load Image With ImageR
      img <- load.image(input$upload$datapath)
      img_df <- as.data.frame(img)
      
      img_df_channel <- img_df %>%
        mutate(channel = case_when(
          cc == 1 ~ "Red",
          cc == 2 ~ "Green",
          cc == 3 ~ "Blue"
        ))
      
      img_wide <- img_df_channel %>%
        select(x, y, channel, value) %>%
        spread(key = channel, value = value) %>%
        mutate(
          color = rgb(Red, Green, Blue)
        )
      
      sample_size <- nrow(img_wide) * (input$sampling / (100))
      img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]
      img_sample$size <- runif(sample_size, min = 0.5, max = input$sizing)
      img_sample
    }
  })
  
  img_dim <- reactive({
    if (is.null(input$upload)) {
      dims <- tibble(width = 600*input$scale, height = 400*input$scale)
    } else {
      # Load Image With ImageR
      img <- load.image(input$upload$datapath)
      # Make dimensions adjustable
      dims <- tibble(width = dim(img)[1] * input$scale * 0.5, height = dim(img)[2] * input$scale * 0.5)
      # Set default width to 750
      multiplier <- 750 / dims$width
      dims <- dims %>%
        mutate(across(everything(), ~ .x * multiplier))
    }
  })
  
  values <- reactiveValues(
    title = "Enter Custom Title"
  )
  
  observeEvent(input$submit_title, {
    values$title <- input$title
  })
  
  # onclick("submit_title", updateTextInput(session, "title", value = input$title))
  
  output$plot <- renderPlot(
    {
      if (input$shape != 14) {
        ggplot(image_data()) +
          geom_point(mapping = aes(x = x, y = y, color = color, size = size), shape = as.numeric(input$shape)) +
          guides(size = FALSE) +
          labs(caption = glue::glue("<br>{str_wrap(values$title, 100)}<br><br><span style='font-size:12pt;'><span style='color:#9DABAC;'>Made with Shiny | @gates_duncan</span></span>")) +
          scale_color_identity() +
          scale_size_identity() +
          scale_y_reverse() +
          theme_void() +
          theme(
            plot.caption = element_markdown(family = "Airborne II Pilot", hjust = 0.5, size = 24, margin = margin(0, 0, 10, 0)),
            plot.margin = margin(20, 20, 10, 20),
            plot.background = element_rect(fill = NA, colour = "grey40", size = 2),
            legend.position = "none"
          )
      } else {
        ggplot(image_data()) +
          geom_emoji(emoji="1f602", mapping = aes(x = x, y = y, color = color, size = size), shape = as.numeric(input$shape)) +
          guides(size = FALSE) +
          labs(caption = glue::glue("<br>{str_wrap(values$title, 100)}<br><br><span style='font-size:12pt;'><span style='color:#9DABAC;'>Made with Shiny | @gates_duncan</span></span>")) +
          scale_color_identity() +
          scale_size_identity() +
          scale_y_reverse() +
          theme_void() +
          theme(
            plot.caption = element_markdown(family = "Airborne II Pilot", hjust = 0.5, size = 24, margin = margin(0, 0, 10, 0)),
            plot.margin = margin(20, 20, 10, 20),
            plot.background = element_rect(fill = NA, colour = "grey40", size = 2),
            legend.position = "none"
          )
      }
    },
    height = function() {
      img_dim()$height
    },
    width = function() {
      img_dim()$width
    }
  )
}

shinyApp(ui, server)
