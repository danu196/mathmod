library(tidyverse)
library(readr)
greendb = read_csv(greendb_example.csv)

data = greendb
rad = data$d_trunk_m / 2
basal = rad * rad *pi
basal

data$basal = basal

data$basal = (data$d_trunk_m /2)*(data$d_trunk_m/2)*pi

data$height_m

data$v = data$basal*data$height_m

data
