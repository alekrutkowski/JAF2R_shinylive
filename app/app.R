library(shiny)
library(shinyjs)
library(jsonlite)

library(data.table)
library(magrittr)

IS_SHINYLIVE <- 
  grepl("wasm",R.Version()$arch)

fetchDataIntoInput <- function(input)
  runjs("
      fetch('https://raw.githubusercontent.com/alekrutkowski/JAF2R_shinylive/main/data/data.json')
        .then(response => response.json())
        .then(data => Shiny.setInputValue('jsonData', JSON.stringify(data)));
  ")

importData. <- memoise::memoise(function(input)
  (
    if (IS_SHINYLIVE) {
      fetchDataIntoInput(input)
      input$jsonData
    } else
      readLines('../data/data.json',warn=FALSE) %>%
      paste(collapse="")
  ) %>% 
    unserializeJSON() %>% 
    memDecompress('gzip') %>% 
    unserialize())

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Shinylive JavaScript GET Request"),
  actionButton("doCalc", "Do some calculation"),
  tableOutput("dataOutput")
)

server <- function(input, output) {
  
  output$dataOutput <- renderTable({
    importData.(input) %>% 
      .$JAF_SCORES %>% 
      head(20)
  })
  
}

shinyApp(ui, server)
