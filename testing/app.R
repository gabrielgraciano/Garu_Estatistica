#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(htmltools)

link_shiny <- tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")
link_posit <- tags$a(shiny::icon("r-project"), "Posit", href = "https://posit.co", target = "_blank")

# Define UI for application that draws a histogram
ui <-
  navset_tab(
    nav_panel(title = "One", p("First tab content.")),
    nav_panel(title = "Two", p("Second tab content.")),
    nav_panel(title = "Three", p("Third tab content")),
    nav_spacer(),
    nav_menu(
      title = "Links",
      nav_item(link_shiny),
      nav_item(link_posit)
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
