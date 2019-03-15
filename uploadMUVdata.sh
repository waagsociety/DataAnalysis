# head mosquitto.json | sed "s/{\"id\":\([0-9]*\),\
# \"ts\":\"\([0-9]*\):\([0-9]*\):\([0-9]*\)\/\([0-9]*\):\([0-9]*\):\([0-9]*\)\",\
# \"t\":\([0-9]*.[0-9]*\),\
# \"h\":\([0-9]*.[0-9]*\),\
# \"p\":\([0-9]*.[0-9]*\),\
# \"p25\":\([0-9]*.[0-9]*\),\
# \"p10\":\([0-9]*.[0-9]*\),\
# \"no2op1\":\([0-9]*\),\
# \"no2op2\":\([0-9]*\),\
# \"dB\":\([0-9]*\)}\
# /muvsensors,id=\1 t=\8,h=\9,p=\10,p25=\11,p10=\12,no2op1=\13,no2op2=\14,db=\15 /g"


# echo "created: 02/05/2013 16:14:49" |  sed -e 's/^\([[:alpha:]]*: \)//' -e 's/\([0-9]\{2\}\)\(\/\)\([0-9]\{2\}\)\(\/\)\([0-9]\{4\}\)\( \)/\5\1\3/' -e 's/\([0-9]\{2\}\)\(\:\)\([0-9]\{2\}\)\(\:\)\([0-9]\{2\}\)/\1\3\5/'


for i in $(cat mosquitto.json | jq '. | "muvsensors,id=\(.id)_t=\(.t),h=\(.h),p=\(.p),p25=\(.p25),p10=\(.p10),no2op1=\(.no2op1),no2op2=\(.no2op2),dB=\(.dB)_\(.ts)"')
do
    MY_DATE=$(echo ${i} | sed "s/.*_\(.*\)\"/\1/g")
    MY_LINE=$(echo ${i} | sed "s/\"\(.*\)_.*/\1/g" | tr '_' ' ')
    MY_DATE=$(date -j -f "%Y:%m:%d/%H:%M:%S" +"%s" "${MY_DATE}")
    MY_LINE=$(echo ${MY_LINE} ${MY_DATE})
    echo ${MY_LINE}
done > mosquitto.lp


curl -X POST \
  'localhost:8086/write?db=sensors&precision=s' \
  --data @mosquitto.lp

