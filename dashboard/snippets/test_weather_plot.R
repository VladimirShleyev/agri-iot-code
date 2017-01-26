rm(list=ls()) # очистим все переменные

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(ggthemes)
# library(ggthemr)
library(httr)
library(reshape2)
library(RColorBrewer)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
library(Cairo)
library(futile.logger)

# на этапе отладки каждый раз перегружаем пакет, который отдельно дорабатывается.
tmp <- getwd()
setwd("d:/iwork.GH/dvtiot")
devtools::load_all()
setwd(tmp)


flog.appender(appender.file('iot-dashboard.log'))
flog.threshold(TRACE)
flog.info("plot started")

# source("common_funcs.R") # сюда выносим все вычислительные и рисовательные функции


combineRawWeatherData <- function() {
  
  # получаем из гитхаба предобработанные исторические данные по погоде -----------------
  # пока мы не детализируем, как эти данные получены, прямой ли загрузкой, либо через фоновый git pull и открытие файла
  # на выходе либо данные, либо NA в случае ошибки
  
  history_url <- "https://raw.githubusercontent.com/iot-rus/agri-iot-data/master/weather_history.csv"
  # %>% '[['("result")

  resp <- safely(read_csv)(history_url)
  if(!is.null(resp$error)){
    flog.error(resp$error)
    return(NA)
  }
  weather_hist <- resp$result %>%
    select(-starts_with("human_")) # удалим все данные-дубликаты, предназначенные для человеческого представления
  flog.debug("History weather data loaded successfully")

  # получаем прогноз через API --------------------------------------------------------
  reqstring <- paste0("api.openweathermap.org/data/2.5/", 
                      "forecast?id=", 
                      '524901', # MoscowID 
                      "&APPID=", 
                      '19deaa2837b6ae0e41e4a140329a1809') # "weather?id="
  
  resp <-  safely(GET)(reqstring)
  
  if(!is.null(resp$error)){
    flog.error(resp$error)
    return(NA)
  }
  flog.debug("Predicted weather data loaded successfully")
  
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
  
  # объединяем и вычищаем --------------------------------------------------------
  
  weather_df <- weather_hist %>%
    mutate(rain3h=NA) %>% # зимой дождя может не быть, а колонка нужна
    bind_rows(l3) %>%
    select(-temp_max, -temp_min, -sea_level, -grnd_level) %>%
    distinct() %>% # удаляем дубли, которые навыдавал API
    mutate(temp = round(temp - 273.15, 1)) %>% # пересчитываем из кельвинов в градусы цельсия
    mutate(pressure = round(pressure * 0.75006375541921, 0)) %>% # пересчитываем из гектопаскалей (hPa) в мм рт. столба
    mutate(humidity = round(humidity, 0)) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01')) %>%
    mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) %>% # сделаем почасовую группировку
    # разметим данные на прошлое и будущее. будем использовать для цветовой группировки
    mutate(time.pos = if_else(timestamp < now(), "PAST", "FUTURE"))

  weather_df
}

extractWeather <- function(raw_weather, timeframe) {
  # timeframe -- [POSIXct min, POSIXct max]
  # для устранения обращений к внешним источникам, теперь на вход 
  # получаем предварительно скомпонованные предобработанные данные 
  # raw_weather <- combineRawWeatherData()
  
  # browser()
  # причешем данные для графика у Паши + проведем усреднение по часовым группам
  # есть нюансы, связанные с выдачей данных из прогноза. 
  # rain3h соотв. прогнозу осадков в мм, на предыдущих три часа
  # за консистентность информации (нарезка тиков 3-х часовыми интервалами) отвечает API.
  # поэтому что mean, что sum -- все одно. timegroup для каждого прогнозного измерения должна быть ровно одна
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

calcRainPerDate <- function(raw_weather) {
  # считаем осадки за сутки ------------------------------
  # timestamp temp.min pressure humidity precipitation temp.max     temp           timegroup
  #    (time)    (dbl)    (dbl)    (dbl)         (dbl)    (dbl)    (dbl)              (time)

  # полагаем, что идентичность выпавших осадков с точностью до третьего знака просто означает дублирование показаний!!!!
  dfw0 <- raw_weather %>%
    select(timestamp, rain3h) %>%
    filter(!is.na(rain3h)) %>% # записи без дождя нас вообще не интересуют
    distinct() %>% # полностью дублирующиеся записи также неинтересны
    # mutate(date = lubridate::date(timestamp)) %>%
    mutate(date=as.Date(timestamp)) %>%
    group_by(date, rain3h) %>% # собираем агрегаты по суткам, а потом по повторяющимся значениям, 
    # может быть погрешность по переходам через сутки, 
    # но при группировке по значениям можем случайно объединить данных с разных дат
    # в каждой группе посчитаем временную протяженность события
    arrange(timestamp) %>%
    mutate (dtime=as.numeric(difftime(timestamp, min(timestamp), unit="min")))
  
  # теперь мы можем проверить, чтобы максимальное значение в группе не превышало 180 мин (3 часа)
  # поглядел на данные, таких групп нет за месяц не нашел, решил пока для простоты забить
  dfw1 <- dfw0 %>% 
    # в каждой группе выберем значение с минимальным временем измерения
    filter(timestamp==min(timestamp)) %>% # см. допущение об идентичности показаний
    ungroup() %>%
    arrange(timestamp)
  
  # а теперь посчитаем агрегаты по суткам
  dfw2 <- dfw1 %>%
    select(-dtime) %>%
    group_by(date) %>%
    summarise(rain=sum(rain3h)) %>% # пытаемся высчитать агрегат за сутки
    ungroup %>%
    mutate(rain=as.numeric(rain)) %>% # но если все будет NA, то надо явно привести к NA_real_
    #mutate(timegroup = as.numeric(as.POSIXct(date, origin='1970-01-01'))) %>%
    mutate(human_timestamp=force_tz(with_tz(as.POSIXct(date), tz = "GMT"), tz = "Europe/Moscow")) %>%
    mutate(timestamp=as.numeric(human_timestamp)) %>%
    arrange(date)
  
  flog.info("Rain calculation finished")
  flog.info(capture.output(print(dfw2)))
  
  dfw2
}

plotRealWeatherData <- function(weather_df, rain_df, timeframe) {
  # timeframe -- [POSIXct min, POSIXct max]
  # агрегат осадков за сутки
  # чтобы график нарисовался столбиками строго по дням, необходимо пропущенные дни добить нулями
  dft <- tibble(date=seq.Date(as.Date(timeframe[1]), as.Date(timeframe[2]), by="1 day"))
  
  df2 <- dft %>%
    left_join(rain_df, by="date") %>%
    mutate(rain=if_else(is.na(rain), 0, rain)) %>%
    select(date, rain) %>%
    mutate(timestamp=force_tz(with_tz(as.POSIXct(date), tz="GMT"), tz="Europe/Moscow")) %>%
    filter(timestamp>=timeframe[1]) %>%
    filter(timestamp<=timeframe[2])
  
  # погода
  df <- weather_df %>%
    filter(timegroup>=timeframe[1]) %>%
    filter(timegroup<=timeframe[2])
  
  lims <- timeframe
  # схлопнем рисование графика
  ## brewer.pal.info
  # https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
  pp <- ggplot(df) +
    # ggtitle("График температуры") +
    # scale_fill_brewer(palette="Set1") +
    # scale_fill_brewer(palette = "Paired") +
    # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
    # geom_point(shape = 1, size = 3) +
    # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
    scale_x_datetime(labels=date_format("%d.%m", tz="Europe/Moscow"), 
                     breaks=date_breaks("1 days"), 
                     #minor_breaks = date_breaks("6 hours"),
                     limits=lims) +
    theme_igray() +
    theme(legend.position="none",
          axis.title.y=element_text(vjust=4)
          ) +
    geom_vline(xintercept=as.numeric(now()), linetype="dotted", color="yellowgreen", lwd=1.1) +
    xlab("Дата")
  
  p1 <- pp +
    geom_line(aes(timegroup, temp, colour=time.pos), lwd=1.2) +
    scale_color_manual(values=brewer.pal(n=9, name="Oranges")[c(3, 7)]) +
    ylab("Температура,\n град. C")
  p2 <- pp +
    geom_line(aes(timegroup, humidity, colour=time.pos), lwd=1.2) +
    scale_color_manual(values=brewer.pal(n=9, name="Blues")[c(4, 7)]) +
    ylim(0, 100) +
    ylab("Влажность\nвоздуха, %")
  # по просьбе Игоря даем сдвижку к столбику + 12 часов для попадания столбика ровно в сутки
  p3 <- pp + 
    geom_bar(data=df2 %>% mutate(timestamp=timestamp + hours(12)), 
             aes(timestamp, rain), fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
    ylim(0, NA) +
    ylab("Осадки\n(дождь), мм")
  
  # grid.arrange(p1, p2, p3, ncol=1) # возвращаем ggplot
  grid.newpage()
  #grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size="first"))
  rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size="first")
  
}

timeframe <- getTimeframe()
raw_weather <- combineRawWeatherData()

weather_df <- extractWeather(raw_weather, timeframe)
rain_df <- calcRainPerDate(raw_weather)



gp <- plotRealWeatherData(weather_df, rain_df, timeframe)

png(filename="render_w_cairo.png", type="cairo", #pointsize=24, 
    units="cm", height=15, width=20, res=150, pointsize=8, antialias="default")
grid.draw(gp)
dev.off()
grid.draw(gp)

