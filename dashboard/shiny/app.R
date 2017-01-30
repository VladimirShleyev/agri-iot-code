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
library(futile.logger)
library(Cairo)

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
  shinythemes::themeSelector(),
  tabPanel("Поле", value="field"),
  tabPanel("About", value="about"),
  
  # titlePanel("Контроль орошения полей"),
  # ----------------
  conditionalPanel("input.tsp=='field'",
                   fluidRow(
                     column(4, h2("Контроль орошения полей"), h3("Консоль агронома")),
                     column(8,
                            fluidRow(
                              column(4, selectInput("history_days", "Глубина истории (дни)", 
                                                    choices = c(0, 1, 3, 5, 7), selected = 5)),
                              column(4, selectInput("predict_days", "Горизонт прогноза (дни)",
                                                    choices = c(1, 2, 3, 5), selected = 2)),
                              column(4, selectInput("time_bin", "Период группировки (часы)",
                                                    choices = c(0.5, 1, 2, 3, 4, 6, 12), selected = 1))
                              ),
                            fluidRow(
                              column(4,checkboxInput(inputId = "sync_graphs",
                                                     label = strong("Синхронизация на графиках оси X"),
                                                     value = FALSE)),
                              column(4, checkboxInput(inputId = "expand_y",
                                                      label = strong("Расширить ось Y"),
                                                      value = FALSE))
                            ),
                            fluidRow(
                              column(8, plotOutput('temp_plot')), # , height = "300px"
                              column(4, plotOutput('temp_plot2')) # , height = "300px"
                            )
                     )
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
  
  rvars <- reactiveValues(field_df = NA,
                          weather_df = NA, 
                          rain_df = NA)   # Anything that calls autoInvalidate will automatically invalidate every 5 seconds.
  # See:  http://shiny.rstudio.com/reference/shiny/latest/reactiveTimer.html
  # Also: http://rpackages.ianhowson.com/cran/shiny/man/reactiveTimer.html
  autoInvalidate <- reactiveTimer(1000 * 60, session) # раз в минуту

#  observe({
#    rvars$should_update <- rvars$should_update + 1 # поставили флаг на обновление данных
#  })
  
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

    # подгрузим и посчитаем данные по погоде и с датчиковт
    # и только если по ходу не возникнет проблем, то мы их обновляем для отображения
    # при последующей аналитике и отображении используются небольшие массивы, 
    # то мы принудительно обрежем данные [-30; +10] дней от текущей даты
    timeframe <- getTimeframe(30, 10)
    
    raw_weather <- gatherRawWeatherData()
    if (!is.na(raw_weather)) {
      rvars$weather_df <- extractWeather(raw_weather, timeframe)
      rvars$rain_df <- calcRainPerDate(raw_weather)
      }

    raw_field <- getSensorData()
    if (!is.na(raw_field)) {
      # сдвинем данные к настоящему моменту времени
      dshift <- now() - max(raw_field$timestamp)
      
      rvars$field_df <- raw_field %>%
        mutate(timestamp=timestamp + dshift) %>%
        filter(timestamp >= timeframe[1]) %>%
        filter(timestamp <= timeframe[2])
    }
    
    # принудительно меняем 
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
  
  output$temp_plot <- renderPlot({
    # invalidateLater(5000, session) # обновляем график раз в 5 секунд
    # flog.info(paste0(input$update_btn, ": temp_plot")) # формально используем
    # игнорируем update_btn, используем косвенное обновление, через reactiveValues

    flog.info(paste0("temp_plot, filed_df: ", capture.output(str(rvars$field_df))))
    
    if (is.na(rvars$field_df)) return(NA) # игнорируем первичную инициализацию или ошибки
        
    # параметры select передаются как character vector!!!!!!!!
    # может быть ситуация, когда нет данных от сенсоров. 
    # в этом случае попробуем растянуть данные до последней даты, когда видели показания
    # вперед ставим не 0, иначе округление будет до нижней даты, т.е. до 0:00 текущего дня
    timeframe <- getTimeframe(days_back=as.numeric(input$history_days),
                              days_forward=ifelse(input$sync_graphs, as.numeric(input$predict_days), 0)) 
    
    # flog.info(paste0("sensors_plot timeframe: ", capture.output(str(timeframe))))
    flog.info(paste0("sensors_plot timeframe: ", timeframe))
    # на выходе должен получиться ggplot!!!

    plotSensorData(rvars$field_df, timeframe, as.numeric(input$time_bin), expand_y=input$expand_y)
  })

  output$weather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    # параметры select передаются как character vector!!!!!!!!
    # browser() 
    if (is.na(rvars$weather_df)[[1]]) return(NULL) # игнорируем первичную инициализацию или ошибки
      
    timeframe = getTimeframe(days_back=as.numeric(input$history_days),
                             days_forward=as.numeric(input$predict_days))
    
    flog.info(paste0("weather_plot timeframe: ", capture.output(str(timeframe))))
    gp <- plotWeatherData(rvars$weather_df, rvars$rain_df, timeframe)
    grid.draw(gp)
  })
  
  output$data_tbl <- DT::renderDataTable({
    df <- rvars$field_df %>% 
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
    input_df <- rvars$field_df
    
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
