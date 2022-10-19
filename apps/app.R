library(shiny)

ui <- fluidPage(
  
  # App title
  titlePanel("Monthly Random Forest")
  
  # Sidebar layout with input and output definitions
)

server <- function(input, output) {
  
}

############ 

shinyApp(ui = ui, server = server)