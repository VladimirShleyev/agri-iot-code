# выгрузка файла из репозитория средствами git
# git archive --remote=git://git.foo.com/project.git HEAD:path/to/directory filename | tar -x

# прямой доступ с web
# https://raw.githubusercontent.com/iMissile/agri-iot-code/master/cron-scripts/weather.sh

# дебаты насчет выявления пути размещения скрипта: http://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
actual_path=$(readlink -f "${BASH_SOURCE[0]}")
script_dir=$(dirname "$actual_path")

# http://stackoverflow.com/questions/8880603/loop-through-array-of-strings-in-bash-script
script_list=(
    "request_weather.sh"    #script_list[0]
    "clean_weather_history.R"    #script_list[1]
)

#Loop
for script_name in "${script_list[@]}"
do
    local_name="${script_dir}/scripts/${script_name}"
    curl https://raw.githubusercontent.com/iMissile/agri-iot-code/master/cron-scripts/${script_name} > ${local_name}
    echo "${script_name}"
    sudo chmod +x $script_name

done
