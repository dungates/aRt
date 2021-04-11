library(bs4Dash)
library(base64enc)
library(imager)
library(tidyverse)
library(ggvoronoi)
library(kableExtra)
library(here)
library(extrafont)
library(ggtext)
library(glue)

ui <- dashboardPage(
  dashboardHeader(title = "Image Art Abstraction Dashboard"),
  dashboardSidebar(
    skin = "light",
    brandColor = "#7759a6",
    title = h4("Image Abstraction", align = "center"),
    textInput("title", "Type Custom Title Here", value = "Abstract Image Art"),
    selectInput("shape", "Select a Shape", selected = "Circle",
                choices = c("Circle" = 16, "Triangle" = 17, "Diamond" = 18, "Square" = 15)),
    numericInput("scale", "Change Image Scale (Increase/Decrease Size)",
              value = 1, step = 1)
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      align = "center",
      column(12,
             splitLayout(cellWidths = c("50%", "50%"),
        fileInput("upload", HTML("Upload an image, large images may <br> take a while to process."), accept = "image/*"),
        uiOutput("image")
      )),
      column(
        12,
        br(),
        br(),
        # br(),
        plotOutput("plot",
                   width = "900px",
                   height = "900px")
      )
    )
  )
)

server <- function(input, output) {
  base64 <- reactive({
    req(input$upload)
    dataURI(file = input$upload$datapath, mime = "image/png")
  })

  image_data <- reactive({
    # Require Image Upload
    req(input$upload)
    
    # Load Image With ImageR
    img <- load.image(input$upload$datapath)
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

    sample_size <- nrow(img_wide)*0.75
    img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]
    img_sample$size <- runif(sample_size, min = 0.5, max = 2)
    img_sample
  })

  img_dim <- reactive({
    req(input$upload)
    # Load Image With ImageR
    img <- load.image(input$upload$datapath)
    tibble(width = dim(img)[1]*input$scale, height = dim(img)[2]*input$scale)
  })
  
  output$plot <- renderPlot({
    req(input$upload)
    ggplot(image_data()) +
      geom_point(mapping = aes(x = x, y = y, color = color, size = size), shape = as.numeric(input$shape)) +
      guides(size = FALSE) +
      labs(caption = str_glue("{str_wrap(input$title, 100)}<br><br><span style='font-size:12pt;'><span style='color:#9DABAC;'>Made with Shiny | @gates_duncan</span></span>")) +
      scale_color_identity() +
      scale_y_reverse() +
      theme_void() +
      theme(
        plot.caption = element_markdown(family = "Airborne II Pilot", hjust = 0.5, size = 24, margin = margin(0, 0, 10, 0)),
        plot.margin = margin(20, 20, 10, 20),
        plot.background = element_rect(fill = NA, colour = "grey40", size = 2),
        legend.position = "none"
      )
  },
  height = function() {img_dim()$height},
  width = function() {img_dim()$width}
  )

  output$image <- renderUI({
    req(base64())
    tags$div(
      tags$img(src = base64(), width = "100%"),
      style = "width: 400px;"
    )
  })
}

shinyApp(ui, server)
