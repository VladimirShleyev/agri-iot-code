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

echo $WEATHER >> /home/iot-rus/lab/weather.log

cat /home/iot-rus/lab/weather.log | grep "coord" | grep -v failed > /home/iot-rus/lab/result/Moscow-Lab/weather.txt

#Работа с GIT
cd /home/iot-rus/lab/result/Moscow-Lab/ && /usr/bin/git config user.name "iot-rus"
cd /home/iot-rus/lab/result/Moscow-Lab/ && /usr/bin/git config user.email "ilya.shutov@devoteam.com"
cd /home/iot-rus/lab/result/Moscow-Lab/ && /usr/bin/git config credential.helper "cache --timeout=9999999"
echo "Запровижинены параметры GIT"

cd /home/iot-rus/lab/result/Moscow-Lab/ && /usr/bin/git add weather.txt
echo "Выполнена команда GIT ADD"

cd /home/iot-rus/lab/result/Moscow-Lab/ && /usr/bin/git commit -m 'Добавлены данные о текущей погоде' -a
echo "Выполнена команда GIT COMMIT"

cd /home/iot-rus/lab/result/Moscow-Lab/ && /usr/bin/git push --repo https://iot-rus:F0rever%21@github.com/iot-rus/Moscow-Lab.git
echo "Выполнена команда GIT PUSH"
