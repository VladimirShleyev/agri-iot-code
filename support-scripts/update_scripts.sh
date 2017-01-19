# выгрузка файла из репозитория средствами git
# git archive --remote=git://git.foo.com/project.git HEAD:path/to/directory filename | tar -x

# прямой доступ с web
# https://raw.githubusercontent.com/iMissile/agri-iot-code/master/cron-scripts/weather.sh

# дебаты насчет выявления пути размещения скрипта: http://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
actual_path=$(readlink -f "${BASH_SOURCE[0]}")
script_dir=$(dirname "$actual_path")
script_name="${script_dir}/scripts/weather.sh"

echo $script_name
curl https://raw.githubusercontent.com/iMissile/agri-iot-code/master/cron-scripts/weather.sh > $script_name
sudo chmod +x $script_name
