#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# Define UI for application that draws a histogram
shinyUI(dashboardPagePlus(
    header = dashboardHeaderPlus(
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
    ),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
        gradientBox(
            imageOutput("image"),
            title = "My gradient Box",
            width = 12,
            icon = "fa fa-heart",
            gradientColor = "maroon", 
            boxToolSize = "xs", 
            closable = TRUE,
            footer = "The footer goes here. You can include anything",
            "This is a gradient box"
        )
    ),
    rightsidebar = rightSidebar(
        background = "dark",
        rightSidebarTabContent(
            id = 1,
            title = "Tab 1",
            textInput("formula1", "X = ", "x^2 - sin(y^2)"),
            textInput("formula2", "Y = ", "y^3 - cos(x^2)")
        ),
        rightSidebarTabContent(
            id = 2,
            title = "Tab 2",
            icon = "desktop",
            active = TRUE,
            sliderInput(
                "obs",
                "Number of observations:",
                min = 0, max = 1000, value = 500
            )
        ),
        rightSidebarTabContent(
            id = 3,
            icon = "paint-brush",
            title = "Tab 3",
            numericInput("obs", "Observations:", 10, min = 1, max = 100)
        )
    ),
    title = "Generative Art"
    )
)
