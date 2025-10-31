#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

whos_up_next <- function(){
  list_of_names <- c("Steve","Fang","Mayur","Billy","Diane W","Joe","Casey",
                     "Chris","Caitlin","Cindy","Diane K","Tom","Jorge","Ray")
  list_length <- length(list_of_names)
  sample(
    x = list_of_names, 
    size = 1, 
    replace = FALSE, 
    prob = rep(1/list_length, list_length)
  )
}

ui <- fluidPage(
  titlePanel("Who's Up Next?"),
  mainPanel(
    actionButton("pick_button", "Pick a Name"),
    verbatimTextOutput("name_output")
  )
)

server <- function(input, output, session) {
  observeEvent(input$pick_button, {
    output$name_output <- renderText({
      whos_up_next()
    })
  })
}

shinyApp(ui, server)
