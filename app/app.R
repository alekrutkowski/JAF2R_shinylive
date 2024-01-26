library(shiny)
library(plotly)
library(data.table)
library(magrittr)
library(kit)
# dev mode: setwd('app')

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

`JAF_KEY->PA_string` <- function(JAF_KEY)
  sub("PA(.*?)\\.(C|O|S).*",'\\1',JAF_KEY) # e.g. PA11c.S1.T -> 11c ; PA7.2.S2.F -> 7.2

`JAF_KEY->C_O_S_part` <- function(JAF_KEY)
  sub("PA(.*?)\\.(C|O|S)(.+?)\\..*",'\\2\\3',JAF_KEY) # e.g. PA11c.S1.T -> S1 ; PA1.O1. -> O1

sort_JAF_KEY <- function(JAF_KEY) {
  pa <-
    JAF_KEY %>% 
    `JAF_KEY->PA_string` %>% 
    list(as.numeric(gsub('[^0-9.]',"",.)),
         .)
  mid <- 
    JAF_KEY %>% 
    `JAF_KEY->C_O_S_part` %>% 
    {list(substr(.,1,1) %>% kit::nswitch('O',1L, 'S',2L, 'C',3L,
                                         default=4L),
          substr(.,2,nchar(.)) %>% as.integer())}
  JAF_KEY %>% 
    .[order(pa[[1]],pa[[2]],mid[[1]],mid[[2]],.)]
}

Main_Indicators_Codes <-
  DATA %>% 
  .$JAF_NAMES_DESCRIPTIONS %>%
  .[(for_Main), JAF_KEY] %>%
  sort_JAF_KEY() %>%
  data.table(JAF_KEY=.,
             Main_Indicators_order = seq_along(.)) %>% 
  .$JAF_KEY

# Indicator_Codes_by_PA <-
#   DATA %>% 
#   .$JAF_NAMES_DESCRIPTIONS %>% 
#   .[,.(JAF_KEY)] %>% 
#   .[,PA := JAF_KEY %>% `JAF_KEY->PA_string`()] %>% 
#   split(by='PA', keep.by=FALSE)

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

ifScoresSelected <- function(input, x, y)
  if (identical(input$SelectedScore,'TRUE')) x else y

ifLevelsSelected <- function(input, x, y)
  if (identical(input$SelectedLevel,'TRUE')) x else y

selectedVarname <- function(input) {
  x <- identical(input$SelectedScore,'TRUE')
  y <- identical(input$SelectedLevel,'TRUE')
  kit::nif(x && y, 'score_latest_value',
           x && !y, 'score_change',
           !x && y, 'value_',
           !x && !y, 'value_change')
}

filteredDATA <- function(input)
  DATA %>% 
  .[[ifScoresSelected(input, 'JAF_SCORES','JAF_GRAND_TABLE_reduced')]] %>% 
  .[JAF_KEY %in% filteredSelectedIndics(input$SelectedIndics)]

hist. <- function(input, subselect=character(0)) {
  var.. <-
    selectedVarname(input)
  dta <-
    filteredDATA(input) %>%
    .[!is.na(.[[var..]])] %>% 
    .[, geo := as.character(geo)] %>% 
    .[, .SD[time==max(time)], by=geo] %>% 
    .[, geo := ifelse(time==max(time),geo,paste0(geo,' (',time,')'))]
  val.. <-
    dta[grepl(input$SelectedGeos,geo), var.., with=FALSE] %>% as.numeric()
  valEU.. <-
    dta[geo==DATA$EU_geo_code, var.., with=FALSE] %>% as.numeric()
  summaries.. <-
    dta[, var.., with=FALSE] %>% 
    .[[1]] %>% 
    {list(p25=quantile(.,.25),
          med=median(.),
          p75=quantile(.,.75))}
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
  addVerticalLine <- function(pl, name, x, color, dash='solid')
    add_trace(pl,
              name=name,
              x = x %>% c(., .),
              y = c(0, max(hist..$y)),
              text = c("",""), # mandatory
              type = "scatter", mode = "lines",
              marker = list(opacity=0),
              line = list(color=color, dash=dash, width=3))
  plot_ly(x=hist..$x, y=hist..$y,
          text=hist..$l,
          type="bar",
          marker = list(color='#D3D3D3'),
          name="All countries") %>% 
    layout(title=paste0(names(INDICATORS)[INDICATORS==input$SelectedIndics],
                        ', ',max(dta$time),
                        '\nThe value for ',names(GEOS)[GEOS==input$SelectedGeos],
                        ' and the distribution of values across other countries'),
           xaxis=list(title=paste0(ifScoresSelected(input, 'Score ', 'Indicator value '),
                                   ifLevelsSelected(input, '(level)', '(change)'))),
           yaxis=list(title='Number of countries inside each interval'),
           bargap = 0,
           margin = list(t = 60), # more space at the top for the title
           annotations = list(
             x = val..,
             y = max(hist..$y),
             text = paste0(names(GEOS)[GEOS==input$SelectedGeos],': ',round(val..,1)),
             showarrow = TRUE,
             arrowhead = 5,
             font = list(size = 18)
           )) %>%
    addVerticalLine(name=paste0(names(GEOS)[GEOS==input$SelectedGeos],': ',
                                round(val..,1)),
                    x = val..,
                    color='black') %>%
    addVerticalLine(name=paste('25th percentile:',round(summaries..$p25,1)),
                    x = summaries..$p25,
                    color='magenta', dash='dash') %>%
    addVerticalLine(name=paste('75th percentile:',round(summaries..$p75,1)),
                    x = summaries..$p75,
                    color='chartreuse', dash='dash') %>% 
    addVerticalLine(name=paste('Median:',round(summaries..$med,1)),
                    x = summaries..$med,
                    color='cyan', dash='dot') %>% 
    addVerticalLine(name=paste('EU:',round(valEU..,1)),
                    x = valEU..,
                    color='blue', dash='dot')
}

byIndic <- function(expr)
  bquote(lapply(input$SelectedIndics,
                     function(x) {
                       input$SelectedIndics <- x # TO FIX/REFACTOR "Can't modify read-only reactive value 'SelectedIndics"
                       .(substitute(expr))
                       }))

selectPlots <- function(input) {
  i <- length(input$SelectedIndics)
  s <- identical(input$SelectedScore,'TRUE')
  g <- length(input$SelectedGeos)
  y <- input$SelectedYears < max(YEARS)
  if (i==0 || g==0) return(NULL)
  if(i==1 && s && g==1 && !y) return(hist.(input)) # 
  if(i>1 && s && g==1 && !y) return(sortedBarChart.(input)) # 
  if(i==1 && s && g>1 && !y) return(sortedBarChart.(input)) # 
  if(i>1 && s && g>1 && !y) return(heatmapGrid.(input)) # 
  if(i==1 && s && g==1 && y) return(hist.(input)) # multiple years ignored -- showing only the latest year
  if(i>1 && s && g==1 && y) return(sortedBarChart.(input)) # multiple years ignored -- showing only the latest year
  if(i==1 && s && g>1 && y) return(sortedBarChart.(input)) # multiple years ignored -- showing only the latest year
  if(i>1 && s && g>1 && y) return(heatmapGrid.(input)) # multiple years ignored -- showing only the latest year
  if(i==1 && !s && g==1 && !y) return(hist.(input)) # 
  if(i>1 && !s && g==1 && !y) return(eval(byIndic(hist.(input)))) # TO FIX/REFACTOR "Can't modify read-only reactive value 'SelectedIndics" # lapply(by indicator) due to different units
  if(i==1 && !s && g>1 && !y) return(sortedBarChart.(input)) # 
  if(i>1 && !s && g>1 && !y) return(byIndic(sortedBarChart.(input))) # lapply(by indicator) due to different units
  if(i==1 && !s && g==1 && y) return(linePlotGeoEU.(input)) # 
  if(i>1 && !s && g==1 && y) return(byIndic(basicLinePlot.(input))) # lapply(by indicator) due to different units
  if(i==1 && !s && g>1 && y) return(basicLinePlot.(input)) # 
  if(i>1 && !s && g>1 && y) return(byIndic(basicLinePlot.(input))) # lapply(by indicator) due to different units
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
  HTML(paste0('
    <div class="flex-container">
      <div class="left-text"><h1>JAF Indicators</h1></div>
      <div class="right-text">',ifelse(IS_SHINYLIVE,'Shinylive','Shiny'),'
      <img src="https://raw.githubusercontent.com/alekrutkowski/JAF2R/main/JAF2R_logo_v3.png" alt="JAF2R project logo" height="40"/>
      </div>
    </div>')), # TODO add above 'shiny' or 'shinylive' before the logo
  selectInput(
    inputId = "SelectedIndics",
    label = HTML("<strong><big><big>&#x1F522;</big></big> Select one or more indicators &ndash; start typing codes or names, use <kbd>Delete</kbd> or <kbd>Backspace</kbd> keyboard keys to delete</strong><br>",
                 '<small>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Hint: Type <code>.O</code> to see "Overall" indicators or <code>.S</code> for "Subindicators" or <code>.C</code> for "Context" indicators</small>'),
    choices = INDICATORS,
    multiple=TRUE,
    width = "100%"
  ),
  selectInput(
    inputId = "SelectedGeos",
    label = HTML("<strong><big><big>&#x1F310;</big></big> Select countries or country aggregates, use <kbd>Delete</kbd> or <kbd>Backspace</kbd> keyboard keys to delete</strong>"),
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
      selected='TRUE',
      width = "100%"
    )),
    column(4,sliderInput(
      inputId = "SelectedYears",
      label = strong("Select a start year for time series of values (if you want to see a longer period)"),
      min = min(YEARS),
      max = max(YEARS),
      ticks=FALSE,
      sep="",
      value = max(YEARS),
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
    selectPlots(input)
  })
  
}

shinyApp(ui, server)
