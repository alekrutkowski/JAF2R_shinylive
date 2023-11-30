library(shiny)
library(shinyjs)

library(data.table)
library(magrittr)

IS_SHINYLIVE <- 
  grepl("wasm",R.Version()$arch)

DATA <- 
  'data.Rds' %>% 
  {if (IS_SHINYLIVE)
    (.) %T>% 
      download.file(paste0('https://raw.githubusercontent.com/alekrutkowski/JAF2R_shinylive/main/data/',
                           .),.) else
        paste0('../data/',.)
  } %>% 
  readRDS()

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("JAF indicators"),
  actionButton("doCalc", "Do some calculation"),
  tableOutput("dataOutput")
)

server <- function(input, output) {
  
  output$dataOutput <- renderTable({
    DATA %>% 
      .$JAF_SCORES %>% 
      head(20)
  })
  
}

shinyApp(ui, server)
