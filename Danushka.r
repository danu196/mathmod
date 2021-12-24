library(tidyverse)
library(lubridate)
library(rnoaa)
library(raster)
library(sp)
library(sf)
library(elevatr)
library(rvest)
library(curl)
library(RStoolbox)
library(RCurl)
library(MODISTools)
library(exactextractr)
options(timeout = 4000000) 


#все метеостанции
stations = ghcnd_stations(refresh = FALSE)


#данные ближайших метеостаций к парку Чистяковская роща г.Краснодар
Krasnodar = data.frame(id="KRR",
                    latitude = c(45.058179),
                    longitude= c(38.993064))


station_list = meteo_nearby_stations(lat_lon_df = Krasnodar, 
                                     station_data = stations,
                                     radius = 150, var = c("PRCP", "TAVG"),
                                     year_min = 2008, year_max = 2019)


station_list =station_list[[1]] 
station_list = station_list %>% filter(name %in% c("KRASNODAR")) #названия станций


#Чтобы получить все данные с метеостации, нужно 
one_station = meteo_tidy_ghcnd("RSM00034927") 

all_data = one_station %>% mutate(year = year(date))%>% 
  filter(year > 2008 & year < 2019) %>% 
  mutate(tavg = tavg/10, prcp = prcp/10)

all_data = all_data %>% dplyr::select(-tmax, -tmin, -snwd)

all_data$prcp[is.na(all_data$prcp)] = 0 
Krasnodar_cum = all_data %>% mutate(month = month(date)) %>% 
  filter(month > 4 & month < 10) %>% 
  group_by(year) %>% 
  mutate(prcp_cum = cumsum(prcp))

Krasnodar_cum %>% summarise(prcp_avg = max(prcp_cum), n = n())

#Выбрать год для дальнейшей работы, по наибольшему кол-ву полученых данных 
###     #   <dbl>   <dbl>   <int>
###     #    2016     388.   153 


# Загрузите kml файл с полигоном вашего парка из любых онлайн карт
park_sf <- read_sf(str_interp("${getwd()}/GROVE.geojson"))


# Сконевртируем объект в sp и загрузим для ншаей местности ЦМР из пакета elevatr
park_sp = as_Spatial(st_zm(park_sf), 
                     cast=TRUE, 
                     IDs = paste0("ID", seq_along(from)))
prj = proj4string(park_sp)
park_dem = elevatr::get_elev_raster(park_sp, 14, prj)
plot(park_dem)
plot(st_geometry(park_sf), add = TRUE)

park_dem_mask = crop(park_dem, park_sp)
plot(park_dem_mask)
plot(st_geometry(park_sf), add = TRUE)
#qmap(park_dem_mask, park_sp)


#MODISTools
#С помощью продуктов  MODIS попытаемся оценить эвапотранспирацию в парке
# Для начала посмотрим какие показатели мы можем получить от продуктов MODIS
prods = MODISTools::mt_products()
# Нам подходит MOD16A2 -  эвапотранспирация
# Посмотрим какие каналы мы можем получить по данному продукту
bands = MODISTools::mt_bands(product = "MOD16A2")
# Канал ET_500m содержит накопленные за 8 дней данные по эвапотраспирации в kg/m^2/8d
# Но мы так же должны учитывать scale factor = 0.1, что значит, что данные представлены
# в десятых долях килограммов и их нужно домножить на 0.1. Кроме того мы видим диапазон 
# допустимых значений величины, из которого следует, что значения выше 32700 надо отбросить.
# 
# Проверим для каких дат есть данные для нашей территории
dates = MODISTools::mt_dates(product = "MOD16A2", lat = 45.058179, lon = 38.993064)


# Так как данные для интересующих нас дат для изучаемой территории имеются перейдем к их получению.
# Для этого в функцию mt_subset мы должны ввести название продукта, координаты территории, канал, 
# дату начала и конца мониторинга, а также параметры km_lr и km_ab, которые будут означать в каком 
# радиусе от указаной точки будут браться пиксели с данными. У нас указано 2, что значит 2км, т.е.
# данные будут браться из окружности радиусом 4 пикселя, т.к. разрешения пикселся 500м
Krasnodar_ET =  MODISTools::mt_subset(product = "MOD16A2",
                                      lat = 45.058179,
                                      lon = 38.993064,
                                      band = "ET_500m",
                                      start = "2016-05-01", 
                                      end = "2016-10-01",
                                      km_lr = 2,
                                      km_ab = 2,
                                      site_name = "Krasnodar",
                                      internal = TRUE,
                                      progress = TRUE)

# В результате мы получили таблицу со значениями из нескольких пикселей за интересующий нас 
# промежуток времени с шагом в 8 дней. Отбросим пропуски в данных и усредним значения 
# пикселей на каждую дату, добавив переменную день года
# 
Krasnodar_ET = Krasnodar_ET %>% filter(value < 32700) %>% dplyr::select(units,calendar_date,value) %>%
  mutate(doy = yday(calendar_date), year=year(calendar_date)) %>% group_by(doy,year,units) %>%
  summarise(ET = mean(value))

# Т.к. данные у нас идут с шагом в 8 дней, построим их сглаженное графическое
# представление с помощью loess сглаживания в ggplot2
ggplot(Krasnodar_ET, aes(x=doy,y=ET))+
  geom_point()+
  geom_smooth()+
  geom_ribbon(aes(ymin = 0,ymax = predict(loess(ET ~ doy))),
              alpha = 0.3,fill = 'blue')+
  ylim(c(0,300))+
  theme_bw()

# Выглядит неплохо. Было бы здорово получить площадь под кривой, т.к. она 
# будет соответствовать усредненной сумме эвапотраспирации за вегетационный
# период. Для окончательных рассчетов нам также надо вспомнить площадь парка
# и площадь зеленых насаждений в нем 

park_area =st_area(park_sf) %>% as.integer() # площадь парка
green_square = park_area * 0.8 # площадь под зелеными насаждениями

# А также данные по осадкам
Prcp_cum = Krasnodar_cum %>% filter(year == 2016) %>% mutate(doy = yday(date)) %>% 
  dplyr::select(doy,prcp_cum) %>% mutate(water_cum = prcp_cum*park_area/1000)
start_day = min(Prcp_cum$doy)
end_day = max(Prcp_cum$doy)

# Тогда общая эвапотраспирация будет рассчитана как
curve = loess(ET ~ doy, Krasnodar_ET) # модель 
ET = (predict(curve,data.frame(doy = start_day:end_day), se = F))#0.1 * kg/m^2/8d
ET[is.na(ET)]=0
ETcum = cumsum(ET)* green_square*0.1/8/1000 #t/m2/d - вспоминаем scale factor
# делим на 8, т.к. данные это сумма за 8 дней и переводим в тонны или м3 воды

# Сводим данные по осадкам и эвапотраспирации в одну таблицу
Prcp_cum$ETcum = ETcum                        
#Посчитаем полив как разницу между накопленной с осадками влагой и 
# эвапотранспирацией, усреднив эту разницу на площадь зеленых насаждений
Prcp_cum = Prcp_cum %>% mutate(irrigation = (ETcum - water_cum)/green_square)

# Кумуляты накопленных осадков и эвапотранспирации
ggplot(Prcp_cum, aes(x = doy,y = ETcum))+
  geom_line( color="green")+
  geom_line(aes(x=doy,y=water_cum))+
  ylab("ET vs Precipitation,m3 for Krasnodar park, 2016")+ 
  theme_bw()
# Необходимый полив - большую часть времени полив не нужен
ggplot(Prcp_cum, aes(x = doy,y = irrigation*1000))+
  geom_line( color="red")+
  geom_hline(yintercept = 0)+
  ylab("Irrigation needed,l/m2 for Krasnodar park, 2016")+
  theme_bw()
# Оставим только ту часть, где полив нужен
ggplot(Prcp_cum, aes(x = doy,y = irrigation*1000))+
  geom_line( color="red")+
  geom_hline(yintercept = 0)+
  ylim(c(-20,20))+ # Эти параметры вам надо подобрать исходя из ваших данных
  ylab("Irrigation needed,l/m2 for Krasnodar park, 2016")+
  theme_bw()