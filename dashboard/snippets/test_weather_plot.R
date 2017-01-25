library(tidyverse)

# plot_real_weather2_data(rvars$weather.df, rvars$rain.df, timeframe)

getwd()

source("common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

load_weather_history <- function(data_url){
  # получаем из гитхаба предобработанные исторические данные по погоде -------------------------------------------------
  # пока мы не детализируем, как эти данные получены, прямой ли загрузкой, либо через фоновый pull и открытие файла
  
  # weather_hist <- 
  #   safely(read_csv)("https://raw.githubusercontent.com/iot-rus/agri-iot-data/master/weather_history.csv1") %>%
  #   '[['("result")

  resp <- safely(read_csv)(data_url)
  if(!is.null(resp$error))
  
  # возвращаем погоду, которая будет либо таблицей, либо NULL в случае ошибки  
}

load_weather_history("https://raw.githubusercontent.com/iot-rus/agri-iot-data/master/weather_history.csv1")