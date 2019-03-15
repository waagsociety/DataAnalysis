declare integer NR_MIN=10
declare TIMERANGE=$(( 1000 * 60 * ${NR_MIN} ))
URLA="https://lkvis.rivm.nl/api/datasources/proxy/23/query"
#URLA="data.waag.org"

WHAT="PM10,PM25,Temp,Hum,Pres"
FROM="FROM  autogen.vuurwerk"
WHERE="WHERE time >= (now() - ${TIMERANGE}ms)"
GRP_BY="GROUP BY id"

mycurl() {
  curl -G -s \
  --data-urlencode "db=waag" \
  --data-urlencode "q=${1}" \
  --data-urlencode "epoch=ms" \
  -H 'cache-control: no-cache' \
  "${URLA}"

}

match_sensor() {
  case "${i}" in
			1340640)
        NAME="Buiksloterham - Greenhost"
        ;;
      1730246)
        NAME="Buiksloterham - Energy docks"
        ;;
      2183229)
        NAME="Zeeburger - Nautilus"
        ;;
      2183371)
        NAME="Waag?"
        ;;
      694997)
        NAME="Waag?"
        ;;
      697435)
        NAME="Zeeburg - Acropolis"
        ;;
      9732434)
        NAME="Buiksloterham - Klaproosweg"
        ;;
      9734042)
        NAME="Buiksloterham - In de tas"
        ;;
      *)
        echo "Sensor ${i} NOT FOUND!!"
        ;;
  esac
  echo ${NAME}

}
clear

SENSORS="1340640 1730246 2183229 2183371 694997 697435 9732434 9734042"


for i in ${SENSORS}
do
  echo -e "\n**************************************************************"
  CMD="mycurl \"SELECT ${WHAT} ${FROM} ${WHERE} AND id = '${i}'\" | jq '.results[] | length'"
  RST="$(eval ${CMD})"
  if [ "${RST} " == "0 " ]
  then
    echo "Sensor ${i} $(match_sensor ${i}) is NOT ONLINE"
  else
    echo "Sensor ${i} $(match_sensor ${i}) is ONLINE"
    # i=${i%%\"}
    # i=${i##\"}
    for j in $(echo ${WHAT} | tr ',' ' ')
    do
      # echo "mycurl \"SELECT ${j} ${FROM} ${WHERE} AND id = '${i}' AND ${j} > 0 \" | jq '.results[] | length'"
      CMD="mycurl \"SELECT ${j} ${FROM} ${WHERE} AND id = '${i}' AND ${j} > 0 \" | jq '.results[] | length'"
      RST="$(eval ${CMD})"
      if [ "${RST} " == "0 " ]
      then
        echo -e "\tSensor does not send ${j}"
      fi
    done
  fi
  # echo -e "**************************************************************"
done

echo -e "\nAll online sensors:"
echo $(mycurl "SELECT ${WHAT} ${FROM} ${WHERE} ${GRP_BY}" | jq ".results[].series[].tags.id")
echo -e "\nDONE!!!!"