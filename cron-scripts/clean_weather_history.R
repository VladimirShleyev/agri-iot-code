rm(list=ls()) # очистим все переменные

library(dplyr)
library(tibble)
library(readr)
library(purrr)
library(curl)
library(jsonlite)
library(futile.logger)
library(digest)
library(tools) # для работы с именами файлов


# To emulate the command line input I would use with Rscript, I entered this in RStudio:
#commandArgs <- function(trailingOnly=TRUE) c("D:/iwork.GH/agri-IoT/data/weather_history.txt")
args <- commandArgs(trailingOnly=TRUE)

print(args)

ifname <- args[1]
# добавил строчку в RStudio Server


parseWHistoryData <- function(wrecs) {
  # преобразуем исторические данные по погоде из репозитория Гарика в человеческий csv--------------------------------------------------------

  # заменим концы строк на `,`` и добавим шапочку и окончание для формирования семантически правильного json
  # последнюю ',' надо удалить, может такое встретиться (перевод строки)
  tmp <- paste0('{"res":[', gsub("\\n", ",\n", wrecs, perl = TRUE), ']}')
  wh_json <- gsub("},\n]}", "}]}", tmp)
  # t <- cat(wh_json)
  # write(wh_json, file="./export/wh_json.txt")
  data <- fromJSON(wh_json)
  
  res <- as_tibble(data$res$main)
  res$timestamp <- data$res$dt # время, в которое были проведены измерения
  # поскольку историю мы сохраняем сами из данных текущих запросов, то
  # rain$3h -- Rain volume for the last 3 hours (http://openweathermap.org/current#parameter)
  res$rain3h <- data$res$rain[['3h']]
  # res$POSIX_time <- as.POSIXct(res$timestamp, origin='1970-01-01')
  POSIX_time <- as.POSIXct(res$timestamp, origin='1970-01-01')
  # см хелп: Date-time Conversion Functions to and from Character
  res$human_time <- format(POSIX_time, format="%F %T %Z")
  # browser()  
  
  # t0 <- '{"coord":{"lon":37.61,"lat":55.76},"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"base":"cmc stations","main":{"temp":291.77,"pressure":1012,"humidity":72,"temp_min":290.15,"temp_max":295.35},"wind":{"speed":4,"deg":340},"clouds":{"all":0},"dt":1464008912,"sys":{"type":1,"id":7323,"message":0.0031,"country":"RU","sunrise":1463965411,"sunset":1464025820},"id":524894,"name":"Moskva","cod":200}'
  # t1 <- '{"coord":{"lon":37.61,"lat":55.76},"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"base":"stations","main":{"temp":291.01,"pressure":1012,"humidity":72,"temp_min":289.15,"temp_max":292.15},"visibility":10000,"wind":{"speed":4,"deg":330},"clouds":{"all":0},"dt":1464007798,"sys":{"type":1,"id":7323,"message":0.0354,"country":"RU","sunrise":1463965412,"sunset":1464025819},"id":524894,"name":"Moskva","cod":200}'
  # t <- paste0('{"results":[', t0, ',', t1, ']}')
  # mdata <- fromJSON(t)
  
  # head(wh_json)
  # возвращаем результат с переставленными колонками
  # http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe
  # http://stackoverflow.com/questions/37171891/how-does-dplyrs-select-helper-function-everything-differ-from-copying
  res %>%
    mutate(human_temp = round(temp - 273.15, 1)) %>% # пересчитываем из кельвинов в градусы цельсия
    mutate(human_pressure = round(pressure * 0.75006375541921, 0)) %>% # пересчитываем из гектопаскалей (hPa) в мм рт. столба
    select(human_time, timestamp, everything())
}

# сначала открываем файл просто как набор строк. 
# Запрос через API выдает очень много идентичных строк, поэтому убиваем двух зайцев:
# - прореживаем исходный файл и сохраняем его
# - сокращаем последующий процессинг


# processWHistoryData(args[1])

# http://adv-r.had.co.nz/Exceptions-Debugging.html
# при проблемах с открытием файла нет смысла продолжать скрипт
tryCatch(tmp <- readr::read_lines(ifname),
         error = function(c) {
           c$message <- paste0(c$message, " (in ", ifname, ")")
           stop(c)
           })
# удаляем дубликаты строк
# 297 -> 163
if(FALSE){
# считаем хеши (digest не векторизирован) и создаем фрейм
hash <- purrr::map(tmp, function(x) digest(x, algo="crc32", serialize=FALSE, raw=TRUE))
w_raw_data <- tibble(txt = tmp) %>%
  mutate(IDV = digest(txt, algo="crc32", serialize=TRUE)) %>%
  group_by(txt) %>% 
  filter(n()>1) %>%
  summarize(n=n())

w_raw_data <- tmp %>% 
  tibble(txt=., hash=unlist(purrr::map(., function(x) digest(x, algo="crc32", serialize=FALSE, raw=TRUE)))) %>%
  group_by(hash) %>% 
  summarize(n=n())

# а это пока вообще не проверял
# используем пакет stringi, детали по скорости (x10) см. здесь: http://stackoverflow.com/a/25466867/6835084
# stringi <- function(x){
#   x[!sapply(seq_along(x), function(i) any(stri_detect_fixed(x[-i], x[i])))]
# }

}

# Updated for dplyr 0.5
# http://stackoverflow.com/questions/22959635/remove-duplicated-rows-using-dplyr
w_raw_data <- tibble(txt = tmp) %>%
  distinct(.keep_all = TRUE)

# require(microbenchmark)
# microbenchmark(
#   w_raw_data <- tibble(txt = tmp) %>%
#     distinct(.keep_all = TRUE)
# )

# перезаписываем исходный файл с устраненными дубликатами
# к сожалению, есть небольшой дребезг со временем восхода\захода, из-за чего одинаковые измерения выглядят разными строчками
write_lines(w_raw_data$txt, 
           paste0(tools::file_path_sans_ext(ifname), "_mod.txt"), 
           append = FALSE)

# передаем данные обратно на json парсинг
w_clean_data <- parseWHistoryData(paste0(w_raw_data$txt, collapse="\n")) %>%
  distinct(.keep_all = TRUE) %>% # теперь еще раз устраним дребезг
  arrange(timestamp)

# сохраняем распарсенный файл
write_csv(w_clean_data, paste0(tools::file_path_sans_ext(ifname), ".csv"), na = "NA", append = FALSE, col_names = TRUE)



