rm(list=ls()) # очистим все переменные

library(readr)
library(curl)
library(jsonlite)
library(futile.logger)

# To emulate the command line input I would use with Rscript, I entered this in RStudio:
commandArgs <- function(trailingOnly=TRUE) c("D:/iwork.GH/agri-IoT/data/weather_history.txt")



args <- commandArgs(trailingOnly=TRUE)
print(args)
print(args[1])

ifname <- args[1]


processWHistoryData <- function(ifname='') {
  # преобразуем исторические данные по погоде из репозитория Гарика в человеческий csv--------------------------------------------------------
  # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
  
  # на выходе либо данные, либо NA в случае ошибки
  
  browser()
  print(as.list(sys.call(-1)))
  callingFun <- as.list(sys.call(-1))[[1]]
  calledFun <- deparse(sys.call()) # as.list(sys.call())[[1]]  
  
  resp <- try({
    curl_fetch_memory("https://raw.githubusercontent.com/iot-rus/Moscow-Lab/master/weather.txt")
  })
  
  # проверим только 1-ый элемент класса, поскльку при разных ответах получается разное кол-во элементов
  if(class(resp)[[1]] == "try-error" || resp$status_code != 200) {
    # http://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine
    flog.error(paste0("Error in ", calledFun, " called from ", callingFun, ". Class(resp) = ", class(resp)))
    flog.error(paste0("resp = ", resp))
    # в противном случае мы сигнализируем о невозможности обновить данные
    return(NA)
  }
  
  # дебажный вывод 
  flog.debug(paste0("Debug info in ", calledFun, " called from ", callingFun, 
                    ". Class(resp) = ", class(resp), ". Status_code = ", resp$status_code))
  flog.debug(capture.output(str(resp)))
  
  # ответ есть, и он корректен. В этом случае осуществляем пребразование 
  wrecs <- rawToChar(resp$content) # weather history
  # wh_json <- gsub('\\\"', "'", txt, perl = TRUE) 
  # заменим концы строк на , и добавим шапочку и окончание для формирования семантически правильного json
  # последнюю ',' надо удалить, может такое встретиться (перевод строки)
  tmp <- paste0('{"res":[', gsub("\\n", ",\n", wrecs, perl = TRUE), ']}')
  wh_json <- gsub("},\n]}", "}]}", tmp)
  # t <- cat(wh_json)
  # write(wh_json, file="./export/wh_json.txt")
  data <- fromJSON(wh_json)
  
  whist.df <- data$res$main
  whist.df$timestamp <- data$res$dt
  # поскольку историю мы сохраняем сами из данных текущих запросов, то
  # rain$3h -- Rain volume for the last 3 hours (http://openweathermap.org/current#parameter)
  whist.df$rain3h <- data$res$rain[['3h']]
  whist.df$human_time <- as.POSIXct(whist.df$timestamp, origin='1970-01-01')
  # browser()  
  
  # t0 <- '{"coord":{"lon":37.61,"lat":55.76},"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"base":"cmc stations","main":{"temp":291.77,"pressure":1012,"humidity":72,"temp_min":290.15,"temp_max":295.35},"wind":{"speed":4,"deg":340},"clouds":{"all":0},"dt":1464008912,"sys":{"type":1,"id":7323,"message":0.0031,"country":"RU","sunrise":1463965411,"sunset":1464025820},"id":524894,"name":"Moskva","cod":200}'
  # t1 <- '{"coord":{"lon":37.61,"lat":55.76},"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"base":"stations","main":{"temp":291.01,"pressure":1012,"humidity":72,"temp_min":289.15,"temp_max":292.15},"visibility":10000,"wind":{"speed":4,"deg":330},"clouds":{"all":0},"dt":1464007798,"sys":{"type":1,"id":7323,"message":0.0354,"country":"RU","sunrise":1463965412,"sunset":1464025819},"id":524894,"name":"Moskva","cod":200}'
  # t <- paste0('{"results":[', t0, ',', t1, ']}')
  # mdata <- fromJSON(t)
  
  # head(wh_json)
}

# сначала открываем файл просто как набор строк. 
# Запрос через API выдает очень много идентичных строк, поэтому убиваем двух зайцев:
# - прореживаем исходный файл и сохраняем его
# - сокращаем последующий процессинг


# processWHistoryData(args[1])

# http://adv-r.had.co.nz/Exceptions-Debugging.html
# при проблемах с открытием файла нет смысла продолжать скрипт
tryCatch(w_raw_data <- readr::read_lines(ifname),
         error = function(c) {
           c$message <- paste0(c$message, " (in ", ifname, ")")
           stop(c)
           })
# удаляем дубликаты строк
# используем пакет stringi, детали по скорости (x10) см. здесь: http://stackoverflow.com/a/25466867/6835084
