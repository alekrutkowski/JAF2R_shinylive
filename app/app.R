library(shiny)
library(plotly)
library(data.table)
library(magrittr)
library(kit)

IS_SHINYLIVE <- 
  grepl("wasm",R.Version()$arch)

`%not in%` <- Negate(`%in%`)

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
  {set_names(as.character(.$geo),.$geo__geo_labels)} # key/name = '[geo] geo_labels', value = 'geo'

YEARS <-
  DATA$JAF_GRAND_TABLE_reduced %>% 
  unique() %>% 
  .$time

ifScoresSelected <- function(input, x, y) {
  # str(input)
  if (identical(input$SelectedScore,'TRUE')) x else y
}

ifLevelsSelected <- function(input, x, y)
  if (identical(input$SelectedLevel,'TRUE')) x else y

selectedVarname <- function(input) {
  #   message('~~~~~~~~~~~~~~'); str(input$SelectedScore); str(input$SelectedLevel); message('~~~~~~~~~~~~~~')
  x <- identical(input$SelectedScore,'TRUE')
  y <- identical(input$SelectedLevel,'TRUE')
  kit::nif(x && y, 'score_latest_value',
           x && !y, 'score_change',
           !x && y, 'value_',
           !x && !y, 'value_change')  }

filteredDATA <- function(input)
  DATA %>% 
  .[[ifScoresSelected(input, 'JAF_SCORES','JAF_GRAND_TABLE_reduced')]] %>% 
  .[JAF_KEY %in% filteredSelectedIndics(input$SelectedIndics)]

hist. <- function(input) {
  dta <-
    filteredDATA(input) %>% 
    .[, .SD[time==max(time)], by=geo]
  var.. <-
    selectedVarname(input)
  val.. <-
    dta[geo==input$SelectedGeos, var.., with=FALSE] %>% as.numeric()
  hist.. <-
    dta %>% 
    .[geo %not in% c(DATA$EU_geo_code,DATA$EA_geo_code), 
      c(var..,'geo'), with=FALSE] %>% {
        d. <- (.)
        h. <- hist(d.[[var..]],plot=FALSE)
        l. <- d. %>% 
          .[, in_bin_no := 
              cut(d.[[var..]], breaks=h.$breaks,
                  include.lowest=TRUE, right=FALSE, labels=F)] %>% 
          .[, .(l = geo %>% 
                  paste(collapse=', ') %>% 
                  strwrap(10) %>% 
                  paste(collapse='\n'))
            , by=in_bin_no] %>% 
          setorder(in_bin_no)
        data.table(in_bin_no=seq_along(h.$mids),
                   x=h.$mids,
                   y=h.$counts) %>% 
          merge(l., by='in_bin_no', all.x=TRUE)
      }
  plot_ly(x=hist..$x, y=hist..$y,
          text=hist..$l,
          type="bar",
          marker = list(color='#D3D3D3'),
          name="All countries") %>% 
    layout(title=paste(names(INDICATORS)[INDICATORS==input$SelectedIndics],
                       '\nThe value for',names(GEOS)[GEOS==input$SelectedGeos],
                       'and the distribution of values across other countries'),
           xaxis=list(title=paste0(ifScoresSelected(input, 'Score ', 'Indicator value '),
                                   ifLevelsSelected(input, '(level)', '(change)'))),
           yaxis=list(title='Number of countries inside each interval'),
           bargap = 0,
           margin = list(t = 60), # more space at the top for the title
           annotations = list(
             x = val..,
             y = max(hist..$y),
             text = round(val..,1),
             showarrow = TRUE,
             arrowhead = 5,
             font = list(size = 18)
           )) %>%
    add_trace(name=names(GEOS)[GEOS==input$SelectedGeos],
              x = val.. %>%
                c(., .),
              y = c(0, max(hist..$y)),
              text = c("",""), # mandatory
              type = "scatter", mode = "lines",
              marker = list(opacity=0),
              line = list(color = 'black', width = 3))
  # add p25, p75, min, max (last two greyish)
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
      choices = c("Tick for scores, untick for values" = 'TRUE'),
      width = "100%"
    )),
    column(4,checkboxGroupInput(
      inputId = "SelectedLevel",
      label = strong("Show levels or changes?"),
      choices = c("Tick for levels, untick for changes" = 'TRUE'),
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
    # TODO add plot type dispatch function based on the length of selected inputs (countries, indics, years)
    hist.(input)
  })
  
}

shinyApp(ui, server)
