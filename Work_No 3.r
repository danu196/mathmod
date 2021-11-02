library(tidyverse)
library(readr)
library(dplyr)


greendb = read_csv('~/urban_modeling/greendb.csv')

data = greendb
rad = data$d_trunk_m / 2
basal = rad * rad *pi
basal

data$basal = basal

data$basal = (data$d_trunk_m /2)*(data$d_trunk_m/2)*pi

data$height_m

data$v = data$basal*data$height_m

data

sum_table = greendb %>% group_by(species_ru) %>%
  summarise(
    diam_m = mean(d_trunk_m, na.rm=T),
    num = n(),
    height_m = mean(height_m, na.rm=T)
  )  

sum_table = summarise(group_by(greendb, species_ru),
                      diam_m = mean(d_trunk_m, na.rm=T), 
                      num = n(),
                      height_m = mean(height_m, na.rm=T)
    )
sum_table = greendb %>% group_by(species_ru) %>%
  summarise(
    diam_m = mean(d_trunk_m, na.rm=T),
    num = n(),
    height_m = mean(height_m, na.rm=T)
  )

divers = greendb %>% group_by(adm_region, species_ru) %>%
  summarise(nspecies = n()) %>% select(-nspecies) %>%
    ungroup() %>% group_by(adm_region) %>% summarise(nspecies = n())

divers

# По району -- топ 3 вида и по каждому виду количество
region_top_species = greendb %>% 
  group_by(adm_region, species_ru) %>% # Груп. по району и виду
  summarise(adm_region = adm_region) %>% # Суммируем по району
  ungroup() %>% 
  group_by(adm_region, species_ru) %>% # Груп. по району и виду
  tally() %>% # Делаем колонку n по количеству повторений вида
  group_by(adm_region) %>% # Груп. по району
  top_n(n = 3) # Берем последние топ 3 вида

# Результат с сортировкой по количеству деревьев
result <- region_top_species[order(region_top_species$adm_region,-region_top_species$n),]


library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)

transp = greendb %>% group_by(adm_region, species_ru) %>%
  summarise(
    nspecies = n()
  ) %>% pivot_wider(names_from = species_ru, values_from = nspecies) %>%
      select(starts_with("Липа"))

library(sf)
library(ggplot2)
library(ggthemes)

map = st_read("~/Downloads/boundary-polygon-lvl8.geojson", options = "ENCODING=UTF-8")
plot(map) 

domin = greendb %>%group_by(adm_region, species_ru) %>% 
  summarise(
    nspecies = n()
  ) %>% group_by(adm_region) %>%
  arrange(adm_region, desc(nspecies)) %>%
  mutate(order = order(nspecies, decreasing = T)) %>%
  filter( order == 1) %>% select(-order, -nspecies) %>%
  rename(NAME = adm_region)

map = left_join(map,domin, by="NAME")

ggplot() + geom_sf(data = map, aes(fill=species_ru))+
  theme_foundation() + theme(legend.title = element_blank())


heights = greendb %>%group_by(adm_region) %>% 
  summarise(
    num = mean(height_m, na.rm=T),
    height_m = mean(height_m, na.rm=T)
  ) %>% group_by(adm_region) %>%
  arrange(adm_region, desc(height_m)) %>%
  mutate(order = order(height_m, decreasing = T)) %>%
  filter( order == 1) %>% select(-order, -height_m) %>%
  rename(NAME = adm_region)

map2 = left_join(map,heights, by="NAME")
ggplot() + geom_sf(data = map2, aes(fill=num))+
  theme_foundation() + theme(legend.title = element_blank())


