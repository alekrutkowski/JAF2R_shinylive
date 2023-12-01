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

Main_Indicators_Codes <-
  c("PA1.O1.", "PA1.S1.M", "PA1.S1.F", "PA1.S3.", "PA1.S4.", "PA1.C3.T", "PA1.C4.T",
    "PA1.O2.", "PA1b.O1.", "PA1b.O1.n.", "PA1c.O1.", "PA1d.O1.", "PA2a.O1.", "PA2b.O1.",
    "PA3.O1.", "PA4.1.O1.", "PA4.2.O1.", "PA5.O1.", "PA6a.O1.", "PA6b.O1.", "PA7.1.O1.",
    "PA7.2.O1.", "PA8.1.O1.", "PA8.2.O1.", "PA9.1.O1.", "PA10.O1.", "PA11.O1.", "PA11.S1.",
    "PA11.S2.", "PA11.S3.T", "PA11.S4.", "PA11.S5.", "PA11.S8.", "PA11.S15.", "PA11a.O1.",
    "PA11b.O1.", "PA11c.O1.")

INDICATORS <-
  DATA$JAF_SCORES %>% 
  .[,.(JAF_KEY,Description)] %>% 
  unique() %>% 
  .[, JAF_KEY__Description :=
      paste0('[',JAF_KEY,'] ',Description)] %>% 
  {set_names(.$JAF_KEY,.$JAF_KEY__Description)} %>% # key/name = '[JAF_KEY] Description', value = 'JAF_KEY'
  c(`All Main Indicators`=paste(Main_Indicators_Codes, collapse=', '),
    .)

filteredSelectedIndics <- function(SelectedIndics)
  SelectedIndics %>% 
  `if`(is.character(.) && length(.)==1 && .==INDICATORS['All Main Indicators'],
       Main_Indicators_Codes,
       .)

GEOS <-
  DATA$EU_Members_geo_names %>% 
  .[, geo__geo_labels :=
      paste0('[',geo,'] ',geo_labels)] %>% 
  {set_names(.$geo,.$geo__geo_labels)} # key/name = '[geo] geo_labels', value = 'geo'

YEARS <-
  DATA$JAF_GRAND_TABLE_reduced %>% 
  unique() %>% 
  .$time

`%not in%` <- Negate(`%in%`)

ifScoresSelected <- function(input, x, y)
  if (input$SelectedScore) x else y

filteredDATA <- function(input)
  DATA %>% 
  .[[ifScoresSelected('JAF_SCORES','JAF_GRAND_TABLE_reduced')]] %>% 
  .[JAF_KEY %in% filteredSelectedIndics(input$SelectedIndics)]
  # TODO pick the right var: score or value, level or change

hist. <- function(input) {
  dta <-
    input %>% 
    filteredDATA() %>% 
    .[, .SD[time==max(time)], by=geo]
  plot_ly(x=dta[geo %in% EU_Members_geo_names],
          type="histogram",
          marker = list(color='#D3D3D3'),
          name="All countries") %>% 
    layout(title="Overall Plot Title",
           xaxis=list(title=ifScoresSelected('Score', 'Indicator value')),
           yaxis=list(title='Number of countries inside each interval'))  %>% 
    add_trace(name=GEOS[input$SelectedGeos],
              x = c(1, 1), # TODO replace 1s with the value for the selected geo,
              y = c(0, 20), # TODO replace 20 with the max bin height 
              type = "scatter", mode = "lines", 
              line = list(color = 'red', width = 2))
}



# App ---------------------------------------------------------------------

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
  selectInput(
    inputId = "SelectedGeos",
    label = HTML("<strong>&#x1F310; Select countries or country aggregates, use <kbd>Delete</kbd> or <kbd>Backspace</kbd> keyboard keys to delete</strong>"),
    choices = GEOS,
    multiple=TRUE,
    width = "100%"
  ),
  fluidRow(
    column(4,checkboxGroupInput(
      inputId = "SelectedScore",
      label = strong("Show standardised scores (latest year) or values (one or more years)?"),
      choices = c("Tick for scores, untick for values" = TRUE),
      width = "100%"
    )),
    column(4,checkboxGroupInput(
      inputId = "SelectedLevel",
      label = strong("Show levels or changes?"),
      choices = c("Tick for levels, untick for changes" = TRUE),
      width = "100%"
    )),
    column(4,sliderInput(
      inputId = "SelectedYears",
      label = strong("Select a start year for time series of values (if you want to see a longer period)"),
      min = min(YEARS),
      max = max(YEARS)-1,
      ticks=FALSE,
      sep="",
      value = max(YEARS)-1,
      width = "100%",
      step = 1
    ))),
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
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
}

shinyApp(ui, server)
