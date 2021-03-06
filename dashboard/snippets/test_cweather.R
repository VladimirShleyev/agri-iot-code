rm(list=ls()) # ������� ��� ����������

library(lubridate)
library(ggplot2)
library(scales)
library(ggthemes)
# library(ggthemr)
library(httr)
library(reshape2)
library(tidyverse)
library(magrittr)
library(arules)
library(RColorBrewer)
library(wesanderson)
library(gtable)
library(grid) # ��� grid.newpage()
library(gridExtra) # ��� grid.arrange()
library(Cairo)
library(futile.logger)
library(profvis)
library(htmltools)


# �� ����� ������� ������ ��� ����������� �����, ������� �������� ��������������.
tmp <- getwd()
setwd("d:/iwork.GH/dvtiot")
devtools::load_all()
setwd(tmp)
getwd()

data <- getCurrentWeather()

res <- paste0(data$timestamp, ": ", 
              data$temp, " C, ",
              data$pressure, " ��, ",
              data$humidity, " %")


# res <- sprintf("%s&nbsp;&nbsp;&nbsp;&nbsp;%2.1f C, %d �� ��. ��., %d %%", format(data$timestamp, "%e %b. %H:%M."), 
#                data$temp, data$pressure, data$humidity)

res <- sprintf("%s   %2.1f C, %d �� ��. ��., %d %%", format(data$timestamp, "%e %b. %H:%M."), 
               data$temp, data$pressure, data$humidity)

res
htmlPreserve(res)
