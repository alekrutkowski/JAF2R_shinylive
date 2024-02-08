library(shiny)
library(plotly)
library(data.table)
library(magrittr)
library(kit)
library(openxlsx)
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

order_by_JAF_KEY <- function(dt)
  dt$JAF_KEY %>% 
  sort_JAF_KEY() %>% 
  data.table(JAF_KEY=.,
             .ordering.=seq_along(.)) %>% 
  merge(dt, by='JAF_KEY') %>% 
  setorder(.ordering.) %>% 
  .[, .ordering. := NULL]

Main_Indicators_Codes <-
  DATA %>% 
  .$JAF_NAMES_DESCRIPTIONS %>%
  .[(for_Main), JAF_KEY] %>%
  sort_JAF_KEY()

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
  order_by_JAF_KEY() %>% 
  .[, JAF_KEY__Description :=
      paste0('[',JAF_KEY,'] ',Description %>% gsub(' ,',",",.,fixed=TRUE))] %>% 
  {set_names(.$JAF_KEY,
             .$JAF_KEY__Description)} %>% # key/name = '[JAF_KEY] Description', value = 'JAF_KEY'
  c('\u2295 Select all Main Indicators (can be slow!)',
    '\u2296 Remove all the selected indicators',
    .)

# filteredSelectedIndics <- function(SelectedIndics)
#   SelectedIndics %>% 
#   `if`(is.character(.) && length(.)==1 && .==INDICATORS['All Main Indicators'],
#        Main_Indicators_Codes,
#        .)

GEOS <-
  DATA$EU_Members_geo_names %>% 
  .[, geo__geo_labels :=
      paste0('[',geo,'] ',geo_labels)] %>% 
  {c('\u2295 Select all the Member States',
     '\u2296 Remove all the selected countries',
     set_names(as.list(.$geo),.$geo__geo_labels))} # key/name = '[geo] geo_labels', value = 'geo'

# filteredSelectedGeos <- function(SelectedGeos)
#   SelectedGeos %>% 
#   `if`(is.character(.) && length(.)==1 && .==GEOS['All Member States'],
#        DATA$EU_Members_geo_names$geo %>% .[nchar(.)==2],
#        .)

YEARS <-
  DATA$JAF_GRAND_TABLE_reduced %>% 
  unique() %>% 
  .$time %>% 
  as.integer() %>% 
  .[.>=2000]

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

filteredDATA <- function(jaf_key, input)
  DATA %>% 
  .[[ifScoresSelected(input, 'JAF_SCORES','JAF_GRAND_TABLE_reduced')]] %>% 
  .[JAF_KEY %in% jaf_key]

hist. <- function(input) {
  lapply(X=input$SelectedIndics,
         input=input,
         FUN=function(single_indic, input) {
           if (input$toggle) return(NULL)
           var.. <-
             selectedVarname(input)
           dta <-
             filteredDATA(single_indic,input) %>%
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
             layout(title=paste0(names(INDICATORS)[INDICATORS==single_indic],
                                 ', ',max(dta$time),
                                 '\nThe value for ',names(GEOS)[GEOS==input$SelectedGeos],
                                 ' and the distribution of values across other countries (a histogram)'),
                    xaxis=list(title=list(text=paste0(ifScoresSelected(input, 'Score ', 'Indicator value '),
                                                      ifLevelsSelected(input, '(level)', '(change)')),
                                          font=list(size=18))),
                    yaxis=list(title='Number of countries inside each interval'),
                    bargap = 0,
                    margin = list(t = 60), # more space at the top for the title
                    annotations = list(
                      x=val..,
                      y=max(hist..$y),
                      text=paste0(grep(input$SelectedGeos,dta$geo,value=TRUE),': ',round(val..,1)),
                      showarrow=TRUE,
                      arrowhead=5,
                      font=list(size=18)
                    )) %>%
             addVerticalLine(name=paste0(grep(input$SelectedGeos,dta$geo,value=TRUE),': ',
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
                             color='blue', dash='dot') %>% 
             renderPlotly()  
         })
}

# sortedBarChart_plot <- function(input,
#                                 xcatvarname,
#                                 marked=NULL,
#                                 byvarname='JAF_KEY') {
#   
# }
# 
# if (input$SelectedScore=='TRUE' && input$SelectedIndics>1) 
#   sortedBarChart_plot(input, xcatvarname='JAF_KEY', marked=input$SelectedGeos, byvarname=NULL) else
#     sortedBarChart_plot(input, xcatvarname='geo')


sortedBarChart. <- function(input) {
  lapply(X=input$SelectedIndics,
         input=input,
         FUN=function(single_indic, input) {
           if (input$toggle) return(NULL)
           var.. <-
             selectedVarname(input)
           dta <-
             filteredDATA(single_indic,input) %>%
             .[!is.na(.[[var..]])] %>% 
             .[, .SD[time==max(time)], by=geo] %>% 
             .[, geo := as.character(geo)]
           dta_with_ordered_geos <-
             dta %>% 
             .[geo %in% input$SelectedGeos] %>% 
             .[, geo := ifelse(time==max(time),geo,paste0(geo,'\n(',time,')'))] %>% 
             .[, geo := factor(geo,levels=geo[order(get(var..),decreasing=TRUE)],ordered=TRUE)]
           valEU.. <-
             dta[geo==DATA$EU_geo_code, var.., with=FALSE] %>% as.numeric()
           addHorizontalLine <- function(pl, name, y, color, dash='solid')
             add_trace(pl,
                       name=name,
                       y = rep(y, length(dta_with_ordered_geos$geo)),
                       x = dta_with_ordered_geos$geo,
                       # text = c("",""), # mandatory
                       type = "scatter", mode = "lines",
                       marker = list(opacity=0),
                       line = list(color=color, dash=dash, width=3))
           plot_ly(x=dta_with_ordered_geos$geo, y=dta_with_ordered_geos[[var..]],
                   text = round(dta_with_ordered_geos[[var..]],1), # Add data labels
                   textposition = 'outside',
                   type="bar",
                   marker = list(color='#D3D3D3'),
                   name="Selected countries") %>% 
             layout(title=paste0(names(INDICATORS)[INDICATORS==single_indic],
                                 ', ',max(dta$time)),
                    xaxis=list(title=NULL),
                    yaxis=list(title=list(text=paste0(ifScoresSelected(input, 'Score ', 'Indicator value '),
                                                      ifLevelsSelected(input, '(level)', '(change)')),
                                          font=list(size=18))),
                    bargap = 5,
                    margin = list(t=60) # more space at the top for the title
             ) %>%
             addHorizontalLine(name=paste('EU:',round(valEU..,1)),
                               y = valEU..,
                               color='blue') %>%
             renderPlotly()  
         })
}

sortedHorizBarChart. <- function(input) {
  var.. <-
    selectedVarname(input)
  dta <-
    filteredDATA(input$SelectedIndics,input) %>%
    .[!is.na(.[[var..]])] %>% 
    .[, .SD[time==max(time)], by=.(geo,JAF_KEY)]
  dta_with_ordered_indics <-
    copy(dta) %>% 
    .[, geo := as.character(geo)] %>% 
    .[geo == input$SelectedGeos] %>% 
    merge(DATA$JAF_SCORES %>% 
            .[,.(JAF_KEY,Description)] %>% 
            unique() %>% 
            .[, JAF_KEY__Description :=
                paste0('[',JAF_KEY,'] ',Description)],
          by='JAF_KEY') %>% 
    .[, JAF_KEY__Description := ifelse(time==max(time),
                                       paste0(JAF_KEY__Description,' '),
                                       paste0(JAF_KEY__Description,', ',time,' '))] %>% 
    .[, JAF_KEY := factor(JAF_KEY__Description,
                          levels=JAF_KEY__Description[order(get(var..),decreasing=TRUE)],
                          ordered=TRUE)]
  plot_ly(y=dta_with_ordered_indics$JAF_KEY,
          x=dta_with_ordered_indics[[var..]],
          text = round(dta_with_ordered_indics[[var..]],1), # Add data labels
          textposition = 'outside',
          type="bar",
          marker = list(color='#D3D3D3'),
          name="Selected countries",
          height=100+30*length(dta_with_ordered_indics$JAF_KEY)) %>% 
    layout(title=paste0(names(GEOS)[GEOS==input$SelectedGeos],', ',max(dta_with_ordered_indics$time)),
           yaxis=list(title=NULL, showgrid=TRUE, gridcolor='#f5f5f5', gridwidth=1),
           xaxis=list(title=list(text=paste0(ifScoresSelected(input, 'Score ', 'Indicator value '),
                                             ifLevelsSelected(input, '(level)', '(change)')),
                                 font=list(size=18))),
           bargap = 5,
           margin = list(t=60) # more space at the top for the title
    ) %>%
    renderPlotly()  
}

basicLinePlot. <- function(input) {
  lapply(X=input$SelectedIndics,
         input=input,
         FUN=function(single_indic, input) {
           if (input$toggle) return(NULL)
           var.. <-
             selectedVarname(input)
           dta <-
             filteredDATA(single_indic,input) %>%
             .[!is.na(.[[var..]])] %>% 
             .[, geo := as.character(geo)] %>% 
             .[, time := as.integer(time)] %>% 
             .[as.integer(input$SelectedYears)<=time] %>% 
             .[geo %in% input$SelectedGeos]
           if (length(unique(dta$time))==1)
             return(return(renderUI(div(class="red-frame",
                                        paste0(single_indic,': No time series, only one time point is available')))))
           plot_ly(x=dta$time, y=dta[[var..]],
                   color=as.factor(dta$geo),
                   text = round(dta[[var..]],1), # Add data labels
                   textposition = 'outside',
                   type="scatter", mode='lines' #,
                   # marker = list(color='#D3D3D3') # ,
                   # name="Selected countries"
           ) %>% 
             layout(title=paste0(names(INDICATORS)[INDICATORS==single_indic],
                                 ifelse(length(input$SelectedGeos)==1,
                                        paste0(', ',names(GEOS)[GEOS==input$SelectedGeos]),"")),
                    xaxis=list(title=NULL,
                               dtick = 1, # Set gridlines at every integer
                               tickmode = "linear",
                               range=c(min(dta$time),max(dta$time))),
                    yaxis=list(title=list(text=paste0(ifScoresSelected(input, 'Score ', 'Indicator value '),
                                                      ifLevelsSelected(input, '(level)', '(change)')),
                                          font=list(size=18))),
                    margin = list(t=60) # more space at the top for the title
             ) %>%
             renderPlotly()  
         })
}

linePlotGeoEU. <- function(input) {
  lapply(X=input$SelectedIndics,
         input=input,
         FUN=function(single_indic, input) {
           if (input$toggle) return(NULL)
           var.. <-
             selectedVarname(input)
           dta <-
             filteredDATA(single_indic,input) %>%
             .[!is.na(.[[var..]])] %>% 
             .[, geo := as.character(geo) %>% 
                 factor(levels=rev(c(input$SelectedGeos,
                                     unique(grep('^EU',geo,value=TRUE)),
                                     unique(grep('^EA',geo,value=TRUE)))) %>% 
                          c(setdiff(unique(geo),.),.) # important so that the selected country is in front
                 )] %>% 
             .[, time := as.integer(time)] %>% 
             .[as.integer(input$SelectedYears)<=time]#  %>% 
           # .[geo %in% input$SelectedGeos]
           if (length(unique(dta$time))==1)
             return(return(renderUI(div(class="red-frame",
                                        paste0(single_indic,': No time series, only one time point is available')))))
           plot_ly(x=dta$time, y=dta[[var..]],
                   color=dta$geo,
                   colors=kit::nif(
                     grepl('^EU',dta$geo), 'blue',
                     grepl('^EA',dta$geo), 'lightblue',
                     dta$geo==input$SelectedGeos, 'black',
                     default='#e6e6e6'
                   ) %>% set_names(dta$geo),
                   text = round(dta[[var..]],1), # Add data labels
                   textposition = 'outside',
                   type="scatter", mode='lines',
                   height=700
           ) %>% 
             layout(title=paste0(names(INDICATORS)[INDICATORS==single_indic],
                                 ifelse(length(input$SelectedGeos)==1,
                                        paste0(', ',names(GEOS)[GEOS==input$SelectedGeos]),"")),
                    xaxis=list(title=NULL,
                               dtick = 1, # Set gridlines at every integer
                               tickmode = "linear",
                               range=c(min(dta$time),max(dta$time))),
                    yaxis=list(title=list(text=paste0(ifScoresSelected(input, 'Score ', 'Indicator value '),
                                                      ifLevelsSelected(input, '(level)', '(change)')),
                                          font=list(size=18))),
                    margin = list(t=60), # more space at the top for the title
                    legend = list(font = list(size = 8))
             ) %>%
             renderPlotly() %>% 
             div(style="height:710px;",.) # needed to avoid the vertical truncations in a list of plots
         })
}

colorPalette <- colorRamp(c("darkred","white","green"))

mode. <- function(x) {
  data.table(value = x) %>%
    .[, .N, by = value] %>%
    .[N == max(N)] %>%
    .$value %>%
    as.numeric() %>% 
    `if`(length(.)==1,.,
         max(.))
}

annot. <- function(num_vec)
  num_vec %>% 
  paste0('<sup><sub><br>',.,'</sub></sup>')
# {ifelse(.<0, strrep("<sup><sub>*</sub></sup>",abs(.)), strrep("<sup><sub>#</sub></sup>",abs(.)))}

heatmapGrid. <- function(input) {
  var.. <-
    selectedVarname(input)
  dta <-
    filteredDATA(input$SelectedIndics,input) %>%
    .[!is.na(.[[var..]])] %>% 
    .[, .SD[time==max(time)], by=.(geo,JAF_KEY)]
  dta_with_ordered_indics <-
    copy(dta) %>% 
    .[, geo := as.character(geo)] %>% 
    .[geo %in% input$SelectedGeos] %>% 
    merge(DATA$JAF_SCORES %>% 
            .[,.(JAF_KEY,Description)] %>% 
            unique() %>% 
            .[, JAF_KEY__Description :=
                paste0('[',JAF_KEY,'] ',Description)],
          by='JAF_KEY') %>% 
    # .[, JAF_KEY__Description := ifelse(time==max(time),
    #                                    paste0(JAF_KEY__Description,' '),
    #                                    paste0(JAF_KEY__Description,', ',time,' '))] %>% 
    .[, JAF_KEY := factor(JAF_KEY__Description,
                          levels=.[,.(JAF_KEY__Description,JAF_KEY)] %>% 
                            unique() %>% 
                            order_by_JAF_KEY() %>% 
                            .$JAF_KEY__Description,
                          ordered=TRUE)] %>% 
    .[, mode := mode.(time)]
  plot_ly(y=dta_with_ordered_indics$JAF_KEY,
          x=dta_with_ordered_indics$geo,
          z=dta_with_ordered_indics[[var..]],
          colors = colorPalette,
          text = round(dta_with_ordered_indics[[var..]],1) %>% # Add data labels
            paste0(ifelse(dta_with_ordered_indics$time!=dta_with_ordered_indics$mode,
                          # annot.(dta_with_ordered_indics$time-dta_with_ordered_indics$mode),
                          annot.(dta_with_ordered_indics$time),
                          "")),
          texttemplate = "%{text}",
          textfont = list(size = 8),
          # textposition = 'outside',
          type="heatmap",
          # marker = list(color='#D3D3D3'),
          # name="Selected countries",
          height=100+22*length(unique(dta_with_ordered_indics$JAF_KEY))) %>% 
    layout(title=paste('"Heatmap": the greener the colour, the better; the more red the colour, the worse\n',
                       mode.(dta_with_ordered_indics$time),
                       'unless indicated otherwise inside the plot cells'),
           yaxis=list(title=NULL, showgrid=TRUE, gridcolor='#f5f5f5', gridwidth=1),
           xaxis=list(title=list(text=paste0(ifScoresSelected(input, 'Score ', 'Indicator value '),
                                             ifLevelsSelected(input, '(level)', '(change)')),
                                 font=list(size=18))),
           margin = list(t=60), # more space at the top for the title
           coloraxis = list(
             colorbar = list(
               bordercolor = "transparent",
               borderlinewidth = 0,
               outlinewidth = 0
             ))
    ) %>%
    renderPlotly()  
}

renderMsg <- function(txt)
  renderPlotly({
    # Create a blank plot
    plot_ly(height=100) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        annotations = list(
          list(
            text = txt,
            x = 0,
            y = 0,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(
              size = 14
            )
          )
        )
      ) %>% 
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("sendDataToCloud", "toImage", "select2d", "lasso2d", "resetScale2d"), showLink = FALSE)
  })

escapeHtml <- function(text)
  text %>%
  as.character() %>%
  gsub("&", "&amp;", ., fixed = TRUE) %>%
  gsub("<", "&lt;", ., fixed = TRUE) %>%
  gsub(">", "&gt;", ., fixed = TRUE) %>%
  gsub("\"", "&quot;", ., fixed = TRUE) %>%
  gsub("'", "&#039;", ., fixed = TRUE)

renderTbl <- function(input) {
  i <- length(input$SelectedIndics)
  s <- identical(input$SelectedScore,'TRUE')
  g <- length(input$SelectedGeos)
  y <- input$SelectedYears < max(YEARS)
  if (input$toggle)
    return(renderMsg('&#9888; App suspended by the user'))
  if (i==0 || g==0 ||
      i==1 && grepl('Remove all the selected',input$SelectedIndics) ||
      g==1 && grepl('Remove all the selected',input$SelectedGeos)) 
    return(renderMsg('&#9432; Please select one or more indicators and one or more countries'))
  var.. <-
    selectedVarname(input)
  dt <-
    filteredDATA(input$SelectedIndics,input) %>%
    .[geo %in% input$SelectedGeos] %>%
    `if`('Description' %in% colnames(.),
         .,
         merge(.,DATA$JAF_SCORES %>% 
                 .[,.(JAF_KEY,Description)] %>% 
                 .[,Description := gsub(' ,',",",Description,fixed=TRUE)] %>% 
                 unique(),
               by='JAF_KEY')) %>% 
    {`if`(!identical(input$SelectedScore,'TRUE'), 
          .[input$SelectedYears <= time],
          .[, .SD[time==max(time)], by=.(JAF_KEY,geo)])} %>% 
    .[, c('JAF_KEY','Description','geo','time',var..), with=FALSE] %>% 
    .[, (var..) := round(get(var..),1)]
  if (nrow(dt)==0)
    return(renderMsg('&#9432; Empty table: please select a lower time-series start year or tick "show standardised scores"'))
  dt2 <-
    dcast(dt, JAF_KEY + Description + geo ~ time) %>% 
    # setnames(c('JAF_KEY','Description','geo','time',var..),
    #          c('Indicator Code','Indicator Description','Country','Year',
    #            paste(ifelse(identical(input$SelectedScore,'TRUE'),'Score','Value'),
    #                  ifelse(identical(input$SelectedLevel,'TRUE'),'Level','Change'))))
    setnames(c('JAF_KEY','Description','geo'),
             c('Indicator Code','Indicator Description','Country'))
  plot_ly(
    type = 'table',
    height=800, # 100+30*max(nrow(dt2),50),
    header = list(
      values = names(dt2) %>% paste0('<b>',.,'</b>'),
      # font=list(style='bold')
      # align = c('left', rep('center', ncol(dt))),
      # line = list(width = 1, color = 'black'),
      fill = list(color = '#efefef')
      # font = list(family = "Arial", size = 14, color = "white")
    ),
    cells = list(
      values = t(as.matrix(unname(head(dt2,100)))) %>% ifelse(is.na(.),"",.) #,
      # align = c('left', rep('center', ncol(dt))),
      # line = list(color = "black", width = 1),
      # fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
      # font = list(family = "Arial", size = 12, color = c("black"))
    )) %>% 
    layout(title=paste(ifelse(identical(input$SelectedScore,'TRUE'),'Score','Value'),
                       ifelse(identical(input$SelectedLevel,'TRUE'),'Level','Change'))) %>% 
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("sendDataToCloud", "toImage", "select2d", "lasso2d", "resetScale2d"), showLink = FALSE) %>% 
    {div(
      div(if (nrow(dt2)>100) HTML('<br>&#9888; Large table: only top 100 rows will be shown, but you will be able to download the full table')),
      div(renderPlotly(.)))}
}

dataForExcel. <- function(input) {
  var.. <-
    selectedVarname(input)
  filteredDATA(input$SelectedIndics,input) %>%
    .[geo %in% input$SelectedGeos] %>%
    `if`('Description' %in% colnames(.),
         .,
         merge(.,DATA$JAF_SCORES %>% 
                 .[,.(JAF_KEY,Description)] %>% 
                 .[,Description := gsub(' ,',",",Description,fixed=TRUE)] %>% 
                 unique(),
               by='JAF_KEY')) %>% 
    {`if`(!identical(input$SelectedScore,'TRUE'), 
          .[input$SelectedYears <= time],
          .[, .SD[time==max(time)], by=.(JAF_KEY,geo)])} %>% 
    .[, c('JAF_KEY','Description','geo','time',var..), with=FALSE] %>% 
    .[, (var..) := round(get(var..),1)] %>% 
    dcast(JAF_KEY + Description + geo ~ time) %>% 
    setnames(c('JAF_KEY','Description','geo'),
             c('Indicator Code','Indicator Description','Country'))
}

selectPlots <- function(input) {
  i <- length(input$SelectedIndics)
  s <- identical(input$SelectedScore,'TRUE')
  g <- length(input$SelectedGeos)
  y <- input$SelectedYears < max(YEARS)
  if (input$toggle) return(renderMsg('&#9888; App suspended by the user'))
  if (i==0 || g==0 ||
      i==1 && grepl('Remove all the selected',input$SelectedIndics) ||
      g==1 && grepl('Remove all the selected',input$SelectedGeos) 
  ) return(renderMsg('&#9432; Please select one or more indicators and one or more countries'))
  if(i==1 && s && g==1 && !y) return(hist.(input)) # 
  if(i> 1 &&  s && g==1 && !y) return(sortedHorizBarChart.(input)) # 
  if(i==1 &&  s && g> 1 && !y) return(sortedBarChart.(input)) # 
  if(i> 1 &&  s && g> 1 && !y) return(heatmapGrid.(input)) # 
  if(i==1 &&  s && g==1 &&  y) return(hist.(input)) # multiple years ignored -- showing only the latest year
  if(i> 1 &&  s && g==1 &&  y) return(sortedHorizBarChart.(input)) # multiple years ignored -- showing only the latest year
  if(i==1 &&  s && g> 1 &&  y) return(sortedBarChart.(input)) # multiple years ignored -- showing only the latest year
  if(i> 1 &&  s && g> 1 &&  y) return(heatmapGrid.(input)) # multiple years ignored -- showing only the latest year
  if(i==1 && !s && g==1 && !y) return(hist.(input)) # 
  if(i> 1 && !s && g==1 && !y) return(hist.(input)) # lapply(by indicator) due to different units
  if(i==1 && !s && g> 1 && !y) return(sortedBarChart.(input)) # 
  if(i> 1 && !s && g> 1 && !y) return(sortedBarChart.(input)) # lapply(by indicator) due to different units
  if(i==1 && !s && g==1 &&  y) return(linePlotGeoEU.(input)) # lapply(by indicator) due to different units
  if(i> 1 && !s && g==1 &&  y) return(linePlotGeoEU.(input)) # lapply(by indicator) due to different units
  if(i==1 && !s && g> 1 &&  y) return(basicLinePlot.(input)) # 
  if(i> 1 && !s && g> 1 &&  y) return(basicLinePlot.(input)) # lapply(by indicator) due to different units
}

kbd_info <-
  'use <kbd>Delete</kbd> or <kbd>Backspace</kbd> keyboard keys to delete, use <kbd>Esc</kbd> to hide the drop-down menu.'

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
    .irs-bar, .irs-bar-edge {
        background: transparent !important;
        border-color: transparent !important;
    }
            #loading {
                position: fixed;
                top: 50%;
                left: 50%;
                margin-top: -50px;
                margin-left: -50px;
                z-index: 100;
                display: none;
            }

            .spinner {
                width: 100px;
                height: 100px;
                border: 16px solid #f3f3f3;
                border-top: 16px solid #3498db;
                border-radius: 50%;
                animation: spin 2s linear infinite;
            }

            @keyframes spin {
                0% { transform: rotate(0deg); }
                100% { transform: rotate(360deg); }
            }

            .switch {
              position: relative;
              display: inline-block;
              width: 60px; /* Width of the switch */
              height: 34px; /* Height of the switch */
            }

            .switch input {
              opacity: 0;
              width: 0;
              height: 0;
            }

            .slider {
              position: absolute;
              cursor: pointer;
              top: 0;
              left: 0;
              right: 0;
              bottom: 0;
              background-color: #ccc;
              -webkit-transition: .4s;
              transition: .4s;
              border-radius: 34px; /* Rounded corners of the switch */
            }

            .slider:before {
              position: absolute;
              content: '';
              height: 20px; /* Height of the circle */
              width: 20px;  /* Width of the circle */
              left: 7px;    /* Position from the left */
              bottom: 5px;  /* Position from the bottom */
              background-color: black;
              -webkit-transition: .4s;
              transition: .4s;
              border-radius: 50%; /* Makes the circle round */
            }

            input:checked + .slider {
              background-color: red;
            }

            input:checked + .slider:before {
              -webkit-transform: translateX(26px);
              -ms-transform: translateX(26px);
              transform: translateX(26px);
            }
    
            .red-frame {
                border: 1px solid red;  /* Red border */
                border-radius: 10px;   /* Rounded corners */
                padding: 10px;         /* Some padding around the text */
                margin: 10px;          /* Margin around the div */
                color: red;            /* Red text color */
                display: inline-block; /* Make the div only as wide as its content */
            }
    
    ")),
    tags$script(HTML("
            //var isShinyBusy = false;         
            
            $(document).on('shiny:busy', function(event) {
                //isShinyBusy = true;
                $('#loading').show();
            });

           // $(document).on('shiny:idle', function(event) {
          //      isShinyBusy = false;
          //  });


            $(document).on('plotly_afterplot', '.js-plotly-plot', function(event) {
                //isShinyBusy = false;
                $('#loading').hide();
            });
            
            function EmptyTable() {
                // Get all SVG elements in the document
                const svgs = document.querySelectorAll('svg');
                for (const svg of svgs) {
                    // Find all <text> elements within the current SVG
                    const textElements = svg.querySelectorAll('text');
                    for (const textElement of textElements) {
                        if (textElement.textContent.includes('Empty table')) {
                            return true; // Return true as soon as we find a match
                        }
                    }
                }
                return false;
            }
            
            function isLoadingSpinnerVisible() {
              const elem = document.getElementById('loading');
              // Check if the element exists
              if (!elem) {
                return false; // Element does not exist
              }
              // Get computed styles of the element
              const style = window.getComputedStyle(elem);
              // Check if the element is not hidden
              return style.display !== 'none' && style.visibility !== 'hidden';
            }
          
            function hideLoadingSpinnerFun() {
                var element = document.getElementById('loading');
                if (element) {
                    $('#loading').hide();
                }
            }
            setTimeout(hideLoadingSpinnerFun, 30000);

            
            $(document).on('shiny:connected', function(e) {
              Shiny.addCustomMessageHandler('hideLoadingSpinner', function(message) {
                $('#loading').hide();
              });
            });

        "))
  ),
  tags$div(id = "loading", class = "spinner"),
  HTML(paste0('
    <div class="flex-container">
      <div class="left-text"><h1>JAF Indicators</h1></div>
      <div class="right-text">',ifelse(IS_SHINYLIVE,'Shinylive','Shiny'),'
      <img src="https://raw.githubusercontent.com/alekrutkowski/JAF2R/main/JAF2R_logo_v3.png" alt="JAF2R project logo" height="40"/>
      </div>
    </div>')), # TODO add above 'shiny' or 'shinylive' before the logo
  # Checkbox input styled as a toggle switch
  tags$div(class = "switch",
           tags$input(id = "toggle", type = "checkbox", class = "switch-input"),
           tags$label(`for` = "toggle", class = "slider round"),
  ),
  HTML('<sub><big><sub><big><big><strong>&nbsp;A switch to <span style="color:red;">stop</span> "reactivity" i.e. stop all the calculations and pick many multiple selections below without immediate recalculation of the output</strong></big></big></sub></big></sub>'),
  # textOutput('tmp'),
  selectInput(
    inputId = "SelectedIndics",
    label = HTML("<strong><big><big>&#8505;&#65039;</big></big> Select one or more indicators: start typing codes or names,",kbd_info,"<br></strong>",
                 '<small>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Hint: Type <code>.O</code> to see "Overall" indicators or <code>.S</code> for "Subindicators" or <code>.C</code> for "Context" indicators in the dropdown menu list.',
                 '&nbsp;Similarly, type e.g. <code>PA6a</code> to  see Policy Area 6a indicators.</small>'),
    choices = INDICATORS,
    multiple=TRUE,
    width = "100%"
  ),
  selectInput(
    inputId = "SelectedGeos",
    label = HTML("<strong><big><big>&#x1F310;</big></big> Select countries or country aggregates,",kbd_info,'</strong>'),
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
      label = strong("Select a start year for time series of values (if you want to see a longer period, but only the available years will be displayed)."),
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
             uiOutput("ThePlotPlace")),
    tabPanel("Table",
             conditionalPanel('input.SelectedIndics.length>0 && input.SelectedGeos.length>0 && (!EmptyTable() || !isLoadingSpinnerVisible())',
                              br(),
                              downloadLink("TheDownloadLink", "\u2B73 Download the selected data as Excel file")),
             br(),
             uiOutput("TheTable"))
  )
  
)

server <- function(input, output, session) {
  
  output$tmp <- renderText(input$toggle)
  
  observe({
    input$SelectedIndics %>% {
      if(!is.null(.) && '\u2295 Select all Main Indicators (can be slow!)' %in% .)
        updateSelectInput(session,
                          inputId = "SelectedIndics",
                          selected=Main_Indicators_Codes)
      if (!is.null(.) && '\u2296 Remove all the selected indicators' %in% .)
        updateSelectInput(session,
                          inputId = "SelectedIndics",
                          selected=character(0))
    }})
  
  observe({
    input$SelectedGeos %>% {
      if (!is.null(.) && '\u2295 Select all the Member States' %in% .)
        updateSelectInput(session,
                          inputId = "SelectedGeos",
                          selected=DATA$EU_Members_geo_names$geo %>% .[nchar(.)==2])
      if (!is.null(.) && '\u2296 Remove all the selected countries' %in% .)
        updateSelectInput(session,
                          inputId = "SelectedGeos",
                          selected=character(0))
    }})
  
  output$TheTable <- renderUI({
    renderTbl(input)
  })
  
  output$ThePlotPlace <- renderUI({
    selectPlots(input)
  })
  
  output$TheDownloadLink <- downloadHandler(
    filename = function() 
      paste0("Custom JAF extraction ",
             Sys.time() %>% gsub(':',".",.,fixed=TRUE) %>% substr(1,19),
             ".xlsx"),
    content = function(filename) {
      dta <-
        dataForExcel.(input)
      col_nums <-
        seq_along(colnames(dta))
      wb <- 
        createWorkbook(creator='DG EMPL F4 Statistical Team')
      addWorksheet(wb, zoom=75,
                   sheetName=
                     paste('JAF',
                           ifelse(identical(input$SelectedScore,'TRUE'),'Score','Value'),
                           ifelse(identical(input$SelectedLevel,'TRUE'),'Level','Change')))
      writeData(wb, 1, startRow = 1, startCol = 1,
                x=paste(ifelse(identical(input$SelectedScore,'TRUE'),
                               'Standardised Indicator Score (not Value)',
                               'Actual Indicator Value (not Score)'),
                        ifelse(identical(input$SelectedLevel,'TRUE'),
                               '\u2014 Level (not Change)','\u2014 Change (not Level)')))
      writeData(wb, 1, startRow = 2, startCol = 1,
                x = dta)
      addStyle(wb, 1, style = createStyle(textDecoration = "bold"), 
               rows=1:2, cols=col_nums, gridExpand=TRUE)
      addStyle(wb, 1, style = createStyle(halign = "right"), stack=TRUE,
               rows=2, cols=col_nums %>% setdiff(1:3), gridExpand=TRUE)
      freezePane(wb,1, firstActiveRow = 3, firstActiveCol = 4)
      setColWidths(wb,1, cols=col_nums,
                   widths=c(15, .82*max(nchar(dta$`Indicator Description`)), 10,
                            rep.int(6,length(dta)-3)))
      saveWorkbook(wb, filename, overwrite = TRUE)
      session$sendCustomMessage("hideLoadingSpinner",list(message=TRUE))
    }
  )
  
}

shinyApp(ui, server)
