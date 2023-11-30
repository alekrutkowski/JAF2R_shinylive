library(shiny)
library(plotly)
library(data.table)
library(magrittr)

IS_SHINYLIVE <- 
  grepl("wasm",R.Version()$arch)

DATA <- 
  'data.Rds' %>% 
  {`if`(IS_SHINYLIVE,
        (.) %T>% 
          download.file(
            paste0('https://raw.githubusercontent.com/alekrutkowski/JAF2R_shinylive/main/data/',
                   .),
            .),
        paste0('../data/',.)
  )} %>% 
  readRDS()

INDICATORS <-
  DATA$JAF_SCORES %>% 
  .[,.(JAF_KEY,Description)] %>% 
  unique() %>% 
  .[, JAF_KEY__Description :=
      paste0('[',JAF_KEY,'] ',Description)] %>% 
  {set_names(.$JAF_KEY,.$JAF_KEY__Description)} # key/name = '[JAF_KEY] Description', value = 'JAF_KEY'

GEOS <-
  DATA$EU_Members_geo_names %>% 
  .[, geo__geo_labels :=
      paste0('[',geo,'] ',geo_labels)] %>% 
  {set_names(.$geo,.$geo__geo_labels)} # key/name = '[geo] geo_labels', value = 'geo'

YEARS <-
  DATA$JAF_GRAND_TABLE_reduced %>% 
  unique() %>% 
  .$time

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .flex-container {
      display: flex;
      justify-content: space-between;
    }
    .left-text {
      /* Optional styling for left text */
    }
    .right-text {
      /* Optional styling for right text */
    }
    "))),
  HTML('
    <div class="flex-container">
      <div class="left-text"><h1>JAF Indicators</h1></div>
      <div class="right-text">
      <img src="https://raw.githubusercontent.com/alekrutkowski/JAF2R/main/JAF2R_logo_v3.png" alt="JAF2R project logo" height="40"/>
      </div>
    </div>'),
  selectInput(
    inputId = "SelectedIndics",
    label = HTML("<strong>&#x1F522; Select one or more indicators &ndash; start typing codes or names, use <kbd>Delete</kbd> or <kbd>Backspace</kbd> keyboard keys to delete</strong>"),
    choices = INDICATORS,
    multiple=TRUE,
    width = "100%"
  ),
  checkboxGroupInput(
    inputId = "SelectedScore",
    label = strong("Show standardised scores (latest year) or values (one or more years)"),
    choices = c("Tick for scores, untick for values" = TRUE),
    width = "100%"
  ),
  selectInput(
    inputId = "SelectedGeos",
    label = HTML("<strong>&#x1F310; Select countries or country aggregates, use <kbd>Delete</kbd> or <kbd>Backspace</kbd> keyboard keys to delete</strong>"),
    choices = GEOS,
    multiple=TRUE,
    width = "100%"
  ),
  sliderInput(
    inputId = "SelectedYears",
    label = strong("Select a start year for time series (if you want to see a longer period)"),
    min = min(YEARS),
    max = max(YEARS)-1,
    ticks=FALSE,
    sep="",
    value = max(YEARS)-1,
    width = "100%",
    step = 1
  ),
  tabsetPanel(
    tabPanel("Chart",
             plotlyOutput("ThePlot")),
    tabPanel("Table",
             tableOutput("TheTable"))
  )
  
)

server <- function(input, output) {
  
  output$TheTable <- renderTable({
    DATA %>% 
      .$JAF_SCORES %>% 
      head(20)
  })
  
  output$ThePlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
}

shinyApp(ui, server)
