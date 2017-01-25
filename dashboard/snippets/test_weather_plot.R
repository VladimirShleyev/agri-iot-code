library(tidyverse)
library(lubridate)
library(httr)
library(reshape2)
library(Cairo)
library(futile.logger)

flog.appender(appender.file('iot-dashboard.log'))
flog.threshold(TRACE)
flog.info("plot started")
getwd()

source("common_funcs.R") # сюда выносим все вычислительные и рисовательные функции


combineRawWeatherData <- function() {
  
  # получаем из гитхаба предобработанные исторические данные по погоде -----------------
  # пока мы не детализируем, как эти данные получены, пр€мой ли загрузкой, либо через фоновый git pull и открытие файла
  # на выходе либо данные, либо NA в случае ошибки
  
  history_url <- "https://raw.githubusercontent.com/iot-rus/agri-iot-data/master/weather_history.csv"
  # %>% '[['("result")

  resp <- safely(read_csv)(history_url)
  if(!is.null(resp$error)){
    flog.error(resp$error)
    return(NA)
  }
  weather_hist <- resp$result %>%
    select(-starts_with("human_")) # удалим все данные-дубликаты, предназначенные дл€ человеческого представлени€
  
  # получаем прогноз через API --------------------------------------------------------
  url <- "api.openweathermap.org/data/2.5/"   
  MoscowID <- '524901'
  APPID <- '19deaa2837b6ae0e41e4a140329a1809'
  reqstring <- paste0(url, "forecast?id=", MoscowID, "&APPID=", APPID)# "weather?id="
  
  resp <-  safely(GET)(reqstring)
  
  if(!is.null(resp$error)){
    flog.error(resp$error)
    return(NA)
  }
  
  flog.debug("Calling api.openweathermap.org")
  # парсим погодные данные
  m <- content(resp$result)$list
  
  # заменили на FP подход
  ll <- m %>%
    map(function(x){ 
      ldate <- getElement(x, 'main')
      ldate$timestamp <- getElement(x, 'dt')
      # мм осадков за предыдущие 3 часа (Rain volume for last 3 hours, mm)
      # http://openweathermap.org/forecast5#parameter
      ldate$rain3h <- getElement(x, 'rain')[['3h']]
      ldate
    })

  l2 <- melt(ll)
  # нормализуем под колонки, которые есть в исторических данных
  l3 <- tidyr::spread(l2, L2, value) %>% 
    select(-L1, -temp_kf) %>%
    mutate(timestamp = as.integer(timestamp))
  
  # объедин€ем и вычищаем --------------------------------------------------------
  
  weather_df <- weather_hist %>%
    mutate(rain3h=NA) %>% # зимой дожд€ может не быть, а колонка нужна
    bind_rows(l3) %>%
    select(-temp_max, -temp_min, -sea_level, -grnd_level) %>%
    distinct() %>% # удал€ем дубли, которые навыдавал API
    mutate(temp = round(temp - 273.15, 1)) %>% # пересчитываем из кельвинов в градусы цельси€
    mutate(pressure = round(pressure * 0.75006375541921, 0)) %>% # пересчитываем из гектопаскалей (hPa) в мм рт. столба
    mutate(humidity = round(humidity, 0)) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01')) %>%
    mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) %>% # сделаем почасовую группировку
    # разметим данные на прошлое и будущее. будем использовать дл€ цветовой группировки
    mutate(time.pos = if_else(timestamp < now(), "PAST", "FUTURE"))

  weather_df
}

calcWeatherDF <- function(raw_weather, timeframe) {
  # timeframe -- [POSIXct min, POSIXct max]
  # дл€ устранени€ обращений к внешним источникам, теперь на вход 
  # получаем предварительно скомпонованные предобработанные данные 
  # raw_weather <- combineRawWeatherData()
  
  # browser()
  # причешем данные дл€ графика у ѕаши + проведем усреднение по часовым группам
  # есть нюансы, св€занные с выдачей данных из прогноза. 
  # rain3h соотв. прогнозу осадков в мм, на предыдущих три часа
  # за консистентность информации (нарезка тиков 3-х часовыми интервалами) отвечает API.
  # поэтому что mean, что sum -- все одно. timegroup дл€ каждого прогнозного измерени€ должна быть ровно одна
  res_DF <- raw_weather %>%
    filter(timegroup >= timeframe[1]) %>%
    filter(timegroup <= timeframe[2]) %>%
    group_by(timegroup, time.pos) %>%
    summarise(temp = mean(temp), 
              pressure = mean(pressure), 
              humidity = mean(humidity), 
              rain3h_av = mean(rain3h)) %>%
    ungroup
  
  # чтобы график не был разорванным, надо продублировать максимальную точку из PAST в группу FUTURE
  POI_df <- res_DF %>%
    filter(time.pos == 'PAST') %>%
    filter(timegroup == max(timegroup)) %>%
    mutate(time.pos = 'FUTURE')
  
  res_DF <- res_DF %>%
    bind_rows(POI_df) %>%
    arrange(timegroup)
  
  res_DF
}


timeframe <- getTimeframe()
raw_weather <- combineRawWeatherData()

weather_DF <- calcWeatherDF(raw_weather, timeframe)

#plotRealWeatherData(weather_hist, NULL, timeframe)


