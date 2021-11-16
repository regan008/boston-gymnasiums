library("shiny")
library("shinythemes")
library("leaflet")
library("DT")
library("shinyWidgets")
rng <- 1910:1920

ui <- fluidPage(
  theme= shinytheme("united"),
  tags$head(
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript")
    #tags$script(src="https://kit.fontawesome.com/e7de980416.js", type="text/javascript")
  ),  
  fluidRow(
    column(12,
           leafletOutput("spaces_map"),
           sliderTextInput("map.year", "Year",
                           choices = rng,
                           selected = rng[1],
                           grid = T,
                           width = "100%"),
           actionButton("reset_button", "Reset Filters",
                        icon = icon("repeat"), class = "btn-warning btn-sm")
           #conditionalPanel(
          #  condition = "input['map.year'] == 1967",
           #  includeHTML("nodatanotice.html")
          # )
           )
  ),
  HTML('<div data-iframe-height></div>')
  
)