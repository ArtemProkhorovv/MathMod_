
#Прохоров Артем Анатольевич ДХ-121 -10 Вариант — для региона 8 рассчитайте урожайность пшеницы в 2000 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 7 лет
#с 10 ближайших метеостанций но убирая из рассчета активных температур дни с температурой выше 27 градусов

library(tidyverse)
library(rnoaa)

#Ознакомимся со справкой команды meteo_nearby_stations
# meteo_nearby_stations
# #Необходимо выбрать  число метеостанций ,которые имеют необходимые данные
# meteo_nearby_stations
#Считывание списка метеостанций с диска
station_data = read.csv("stations.csv")
Elista = data.frame(id = "Elista", latitude = 52.73169,longitude = 41.44326)
#Ознакомимся со справкой команды meteo_nearby_stations
# #Необходимо выбрать конечное число метеостанций ,которые имеют необходимые данные
# meteo_nearby_stations
Elista_around=meteo_nearby_stations(lat_lon_df = Elista,station_data = station_data,limit=10,var=c("PRCP","TAVG"),year_min=2000,year_max=2000)
Elista_around
#вспомним, как работать со списками
#1)очевидно что первым элементом таблицы будет
#идентификатор метеостанции Тамбова, его то мы и попытаемся получить
Elista_id = Elista_around[["Elista"]][["id"]] [1]
Elista_id

all_Elista_data = meteo_tidy_ghcnd(stationid = Elista_id)
all_Elista_data

#Подумаем, какие из этих данных нам нужны
summary(all_Elista_data)

#Цикл для всех метеостанций
Elista_around = Elista_around[[1]]
Elista_around
station_names = Elista_around[["id"]]
station_names
for(station_name in station_names)
{
  station_data=meteo_tidy_ghcnd(stationid = station_name,var="TAVG", date_min = "1993-01-01", date_max = "2000-12-31")
  all_Elista_data=rbind(all_Elista_data)
}
#Запись полученных данных в файл
write.csv (all_Elista_data,"all_Elista_data.csv")
all_Elista_data

#2 часть
# считываем данные из файла all_Elista_data
all_Elista_data = read.csv("all_Elista_data")
str(all_Elista_data)
#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)

# вытащить год
#проверим, что работает
y = year(all_Elista_data$date); y
all_Elista_data[, "year"] = year(all_Elista_data$ date)
#добавим месяц
all_Elista_data [,"month"]= month(all_Elista_data$date)
#вытащить день от начала года
all_Elista_data [,"day_of_the_year"]= yday(all_Elista_data$date)
#проверим результат
str(all_Elista_data)

##Приведение средней суммы температур в подходящую форму, при помощи деления на 10
all_Elista_data[,"tavg"] = all_Elista_data$tavg/10
all_Elista_data

#Превращение всех NA и tavg <5 в нули
all_Elista_data[is.na(all_Elista_data$tavg),"tavg"] = 0
all_Elista_data[all_Elista_data$tavg<5, "tavg"] = 0
all_Elista_data[all_Elista_data$tavg>27,"tavg"]=0
summary (all_Elista_data)

##Cуммарная температура за месяц за год для всех станций
#Группировка по метеостанциям, годам и месяцам при помощи функции group_by
alldays = group_by(all_Elista_data, id, year, month)
alldays

#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_Elista = summarize(alldays, tsum = sum(tavg))

sumT_alldays_Elista


summary(sumT_alldays_Elista)

# Сгруппируем данные по месяцам
groups_Elista_months = group_by(sumT_alldays_Elista,month)
# найдем для всех метеостанций среднее по месяцам
sumT_months = summarize(groups_Elista_months, St = mean(tsum))
sumT_months

# Рассчет урожая
# Константы
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# отношение числа дней i-го месяца,
#входящих в период вегетации культуры, к общему
#числу дней в месяце,константа по табл. 1.
y = 1.0
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf = 300
# Коэффициент использования ФАР посевом
Qj = 1600
# калорийность урожая культуры
Lj = 2.2
# сумма частей основной и побочной продукции
Ej = 25
# стандартная влажность культуры
# Рассчитаем Fi по месяца
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
# Расчитываем урожай как сумму по месяцам ц/га
Yield = sum(sumT_months$Yi)
Yield

