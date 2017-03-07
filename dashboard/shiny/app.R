# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8

rm(list=ls()) # очистим все переменные

library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(magrittr)
#library(leaflet)
library(DT)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)
library(ggmap)
library(dplyr)
library(tidyr)
library(readr)  #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
#library(purrr)
library(reshape2)
library(ggthemes)
library(ggdendro) # для пустой темы
library(wesanderson) # https://github.com/karthik/wesanderson
library(RColorBrewer)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
# library(KernSmooth)
library(akima)
library(curl)
library(httr)
library(jsonlite)
library(arules)
library(htmltools)
library(futile.logger)
library(Cairo)
library(hrbrthemes)

# devtools::install_github("iMissile/dvtiot")
unloadNamespace("dvtiot") # выгружаем старый релиз
library(dvtiot)
## на этапе отладки каждый раз перегружаем пакет, который отдельно дорабатывается.
# tmp <- getwd()
# setwd("d:/iwork.GH/dvtiot")
# devtools::load_all()
# setwd(tmp)
# getwd()

# задаем фиксированный порт для shiny (http://shiny.rstudio.com/reference/shiny/latest/runApp.html)
# options(shiny.host = "127.0.0.1")
# options(shiny.port = 7775)
# options(shiny.trace = TRUE)
# options(shiny.error = browser)
# options(shiny.reactlog = TRUE)
options(shiny.usecairo=TRUE)

# library(rgl)
# настраиваем кастомный логгер
# t <- paste0("iot_", format(now(), "%Y%m%d_%H%M%S"), ".log")
# flog.appender(appender.file(t), name = 'iotlog')
flog.appender(appender.file('iot-dashboard.log'))
flog.threshold(TRACE)
flog.info("PoC dashboard started")
flog.info("Working directory is %s", getwd())

# source("../common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# это вместо source
# How to source() .R file saved using UTF-8 encoding?
# http://stackoverflow.com/questions/5031630/how-to-source-r-file-saved-using-utf-8-encoding
eval(parse("../common_funcs.R", encoding="UTF-8"))
# browser()

# ================================================================
ui <- 
  navbarPage("DVT IoT",
  title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  # windowTitle="CC4L",
  collapsible=TRUE,
  id="tsp",
  theme=shinytheme("flatly"),
  # shinythemes::themeSelector(),
  
  # includeCSS("styles.css"),
  
  tabPanel("Поле", value="field"),
  tabPanel("About", value="about"),
  
  
  # titlePanel("Контроль орошения полей"),
  # ----------------
  conditionalPanel(
    "input.tsp=='field'",
    fluidRow(
      tags$style(type='text/css', '#cweather_text {white-space:pre;}'), 
      column(6, h2("Контроль орошения полей"), h3(textOutput("cweather_text", inline=TRUE))),
      column(6,
             fluidRow(
               column(4, selectInput("history_days", "Глубина истории (дни)", 
                                     choices = c(0, 1, 3, 5, 7), selected = 5)),
               column(4, selectInput("predict_days", "Горизонт прогноза (дни)",
                                     choices = c(1, 2, 3, 5), selected = 2)),
               column(4, selectInput("time_bin", "Группировка (часы)",
                                     choices = c(0.5, 1, 2, 3, 4, 6, 12), selected = 1))
               )
             )
      ),
    
    fluidRow(
        column(6, plotOutput('temp_plot', height = "600px")), # 
        column(6, plotOutput('weather_plot', height = "600px")) # , height = "300px"
      ),
    
    fluidRow(
      column(2,checkboxInput(inputId = "sync_graphs",
                             label = strong("Прогноз влажности почвы"),
                             value = TRUE)),
      column(2, checkboxInput(inputId = "expand_y",
                              label = strong("Расширить ось Y"),
                              value = FALSE))
    )    
  )
)

# ================================================================
server <- function(input, output, session) {
  
  # создаем инстанс текущих данных
  # data.frame -- подмножество для анализа и отображения
  # по-хорошему, надо reactive values использовать немного по другому
  # см. https://cdn.rawgit.com/jcheng5/user2016-tutorial-shiny/master/slides.html Слайд с 1-м упражнением
  # Takeaway: Prefer using reactive expressions to model calculations, over using observers to set (reactive) variables.
  
  # See:  http://shiny.rstudio.com/reference/shiny/latest/reactiveTimer.html
  # Also: http://rpackages.ianhowson.com/cran/shiny/man/reactiveTimer.html
  autoInvalidate <- reactiveTimer(1000 * 60, session) # раз в минуту
  
  cweather_df <- reactive({
    # Invalidate and re-execute this reactive expression every time the timer fires.
    autoInvalidate()
    req(getCurrentWeather())
  })  


  field_df <- reactive({
    autoInvalidate()
    raw_field <- req(getSensorData())
    # специально завязали на кнопку
    flog.info(paste0("field_df invalidated. ", input$update_btn, " - ", Sys.time()))    
    
    # сдвинем данные к настоящему моменту времени
    dshift <- now() - max(raw_field$timestamp)
    
    # при последующей аналитике и отображении используются небольшие массивы, 
    # поэтому мы принудительно обрежем данные [-30; +10] дней от текущей даты
    timeframe <- getTimeframe(30, 10)
    
    raw_field %>%
      mutate(timestamp = timestamp + dshift) %>%
      filter(timestamp >= timeframe[1]) %>%
      filter(timestamp <= timeframe[2])
  })  
  
  raw_weather_df <- reactive({
    autoInvalidate()
    req(gatherRawWeatherData())
  })  
  
  rain_df <- reactive({
    calcRainPerDate(raw_weather_df())
  })  

  weather_df <- reactive({
    # при последующей аналитике и отображении используются небольшие массивы, 
    # поэтому мы принудительно обрежем данные [-30; +10] дней от текущей даты
    timeframe <- getTimeframe(30, 10)
    extractWeather(raw_weather_df(), timeframe)
  })  
  
  observeEvent(input$logdata_btn, {
    flog.info("Сброс глобальных данных")
    flog.info("rvars$field_df")
  })
  
  observe({
    # в одном месте следим и за таймером и за нажатием на кнопку
    # Invalidate and re-execute this reactive expression every time the timer fires.
    autoInvalidate()
    # смотрим, требуется ли обновление данных
    flog.info(paste0("autoInvalidate. ", input$update_btn, " - ", Sys.time()))

    # отобразили время последнего обновления
    output$time_updated <- renderText({ 
      paste0(Sys.time())
    })
  })
  
  # виджет текущей погоды
  output$cweather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    invalidateLater(1000 * 60) # обновляем в автономном режиме раз в N минут
    plot_cweather()
    # plot_cweather_scaled()
  })

  # текстовая сводка по текущей погоде
  output$cweather_text <- renderText({
    # на выходе должен получиться текст!!!
    data <- cweather_df() # reactive value
    
    res <- sprintf(
      "%s   >   %2.1f C, %d мм рт. ст., %d %%",
      format(data$timestamp, "%e %b. %H:%M"),
      data$temp,
      data$pressure,
      data$humidity
    )

    flog.info("cweather_text redraw")
    res
    #HTML(paste0("<pre>", res,"</pre>"))
  })

  output$temp_plot <- renderPlot({

    # может быть ситуация, когда нет данных от сенсоров. 
    # в этом случае попробуем растянуть данные до последней даты, когда видели показания
    # вперед ставим не 0, иначе округление будет до нижней даты, т.е. до 0:00 текущего дня
    timeframe <- getTimeframe(days_back=as.numeric(input$history_days),
                              days_forward=ifelse(input$sync_graphs, as.numeric(input$predict_days), 0)) 
    
    # flog.info(paste0("sensors_plot timeframe: ", capture.output(str(timeframe))))
    flog.info(paste0("sensors_plot redraw. timeframe: ", timeframe))
    # на выходе должен получиться ggplot!!!
    plotSensorData(field_df(), timeframe, as.numeric(input$time_bin), expand_y=input$expand_y)
  })

  output$weather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    timeframe <- getTimeframe(days_back=as.numeric(input$history_days),
                             days_forward=as.numeric(input$predict_days))
    
    #flog.info(paste0("weather_plot timeframe: ", capture.output(str(timeframe))))
    flog.info(paste0("weather_plot timeframe: ", timeframe))
    # browser()
    gp <- plotWeatherData(req(weather_df()), req(rain_df()), timeframe)
    grid.draw(gp)
  })
  
  output$data_tbl <- DT::renderDataTable({
    df <- field_df() %>% 
      filter(type == 'MOISTURE') %>%
      select(name, measurement, work.status, timestamp, type) %>% 
      arrange(desc(timestamp))
    # изменим значения на русский
    df$work.status <- ifelse(df$work.status, "Ок", "Неисправен")
    
    DT::datatable(df,
                  # colnames = c('время' = 'timestamp'),
                  colnames = c('# сенсора', 'V', 'статус', 'время', 'тип'), # https://rstudio.github.io/DT/, п.2.4
                  options = list(lengthChange = FALSE, pageLength = 6)) %>%
      formatDate('timestamp', method = "toLocaleString") # см. https://rstudio.github.io/DT/functions.html, https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#Conversion_getter 
    })

  output$map_plot <- renderPlot({
    
    slicetime <- now()
    #slicetime <- dmy_hm("29.04.2016 5:00", tz = "Europe/Moscow")
    # input_df <- field_df.old
    input_df <- field_df()
    
    sensors_df <- prepare_sensors_mapdf(input_df, slicetime)
    
    flog.info("sensors_df")
    # flog.info(capture.output(print(sensors_df)))
    gm <- draw_field_ggmap(sensors_df, heatmap = FALSE)
    # benchplot(gm)
    gm
  })

}

shinyApp(ui = ui, server = server)
# http://shiny.rstudio.com/reference/shiny/latest/runApp.html
# app <- shinyApp(ui = ui, server = server)
# runApp(app, port = "8080")
