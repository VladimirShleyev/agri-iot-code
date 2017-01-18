#!/bin/bash

ADC_01_LAT=55.765232
ADC_01_LON=37.580322

ADC_02_LAT=55.761185
ADC_02_LON=37.578364


WEATHER=`curl --get "http://api.openweathermap.org/data/2.5/weather?lat=55.765232&lon=37.580322&appid=285552c0e3e33b51991e71b9e9d527dc"`

while [[ $WEATHER == failed* || $WEATHER == \<h* || $WEATHER == null* ]]
do
WEATHER=`curl --get "http://api.openweathermap.org/data/2.5/weather?lat=55.765232&lon=37.580322&appid=285552c0e3e33b51991e71b9e9d527dc"`
done

# дебаты насчет выявления пути размещения скрипта: http://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
actual_path=$(readlink -f "${BASH_SOURCE[0]}")
script_dir=$(dirname "$actual_path")
log_file="${script_dir}/weather.log"
# If you want to do it without following any symlinks, then try using realpath with option -s:
# http://stackoverflow.com/questions/6643853/how-to-convert-in-path-names-to-absolute-name-in-a-bash-script
data_file=$(realpath -sm "${script_dir}/../data/weather.txt")

echo "log file name = " ${log_file}
echo "data file name = " ${data_file}

echo $WEATHER >> $log_file
echo $WEATHER >> $data_file

cat $log_file | grep "coord" | grep -v failed > $data_file

#Работа с GIT
cd $(dirname ${data_file})

git config user.name "iot-rus"
git config user.email "ilya.tsas@gmail.com"
git config credential.helper "cache --timeout=9999999"
echo "Запровижинены параметры GIT"

git add weather.txt
echo "Выполнена команда GIT ADD"

cmessage="Добавлены данные о текущей погоде: $(date)"
git commit -m "${cmessage}"
echo "Выполнена команда GIT COMMIT"
git push --repo https://iot-rus@github.com/iot-rus/agri-iot-data
echo "Выполнена команда GIT PUSH"
