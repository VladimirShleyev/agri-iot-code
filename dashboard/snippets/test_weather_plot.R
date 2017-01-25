library(tidyverse)
library(lubridate)
library(Cairo)
library(futile.logger)

# plotRealWeatherData(rvars$weather.df, rvars$rain.df, timeframe)

flog.appender(appender.file('iot-dashboard.log'))
flog.threshold(TRACE)
flog.info("plot started")
getwd()

source("common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

loadWeatherHistory <- function(data_url){
  # получаем из гитхаба предобработанные исторические данные по погоде -------------------------------------------------
  # пока мы не детализируем, как эти данные получены, прямой ли загрузкой, либо через фоновый pull и открытие файла
  
  # weather_hist <- 
  #   safely(read_csv)("https://raw.githubusercontent.com/iot-rus/agri-iot-data/master/weather_history.csv1") %>%
  #   '[['("result")

  resp <- safely(read_csv)(data_url)
  if(!is.null(resp$error)){
    flog.error(resp$error)
  }
    
  # возвращаем данные о погоде, которая будет либо таблицей, либо NULL в случае ошибки, обеспечивается посредством safely
  resp$result  
}

#load_weather_history("https://raw.githubusercontent.com/iot-rus/agri-iot-data/master/weather_history.csv1")

# без лишних заморочек загружаем исторические данные по погоде
weather_hist <- 
  safely(read_csv)("https://raw.githubusercontent.com/iot-rus/agri-iot-data/master/weather_history.csv") %>%
  '[['("result")
timeframe <- getTimeframe()
