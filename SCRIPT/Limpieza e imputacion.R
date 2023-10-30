################################################################################
#------------------------------------------------------------------------------#
#--------------------------------PROBLEM SET 2 --------------------------------#
#------------------------------------------------------------------------------#
################################################################################

#--------------------- LIMPIEZA E IMPUTACIÓN DE DATOS--------------------------#

library(pacman)
library(leaflet)
library(stringi)


# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Traer info geo espacial (limites bogta, etc) 
       tidymodels, #para modelos de ML
       skimr,
       stargazer, #Mostrar modelos de ML
       mapview, # Mapas estaticos
       VIM,
       discrim,
       kknn,
       stargazer) 

test <- read_csv("C:/Users/Hp/Documents/MeCA - Big Data and Machine Learning/Set_2/Set_2/test.csv")
train <- read_csv("C:/Users/Hp/Documents/MeCA - Big Data and Machine Learning/Set_2/Set_2/train.csv")

dim(test)
dim(train)

glimpse(test)
glimpse(train)

#Se hace un análisis provisional de la base de datos, en el cual se corrobora que, efectivamente los datos
#corresponden a la ciudad de Bogotá, para casas y apartamentos que están en venta en la localidad de
#chapinero

train %>%
  count(property_type) 
test %>%
  count(property_type)
train %>%
  count(operation_type) 
test %>%
  count(operation_type)
train %>%
  count(rooms)
test %>%
  count(rooms)

# Se analizan los NA's de la base para imputación

skim(train)
skim(test)

test %>%
  summarise_all(~sum(is.na(.)))
train %>%
  summarise_all(~sum(is.na(.)))

#
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

## Imputación de datos faltantes por mediana y moda

table(train$rooms) #Se evidencia que la mayoría de viviendas tienen 3 cuartos, por lo tanto se imputan los datos con moda = 3
table(test$price) #Se evidencia que la mayoría de viviendas tienen 3 dormitorios, por lo tanto se imputan los datos con moda = 3
table(train$bathrooms) #Se evidencia que la mayoría de viviendas tienen 3 cuartos, por lo tanto se imputan los datos con moda = 3

#El mismo análisis se hace con los datos en la base test

m_sup_cubierta <- median(train$surface_covered, na.rm = T) 
m_sup_total <- median(train$surface_total, na.rm = T)

train <- train %>%
  mutate(rooms= replace_na(rooms, 3), #No se coloca bedrooms, toda vez que, para train no se evidencian NA's
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, m_sup_cubierta),
         surface_total = replace_na(surface_total, m_sup_total))

m_test_cubierta <- median(test$surface_covered, na.rm = T) 
m_test_total <- median(test$surface_total, na.rm = T)

test <- test %>%
  mutate(rooms= replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, m_test_cubierta),
         surface_total = replace_na(surface_total, m_test_total))

# Análisis de la distribución de precios

summary(train$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1=scales::dollar(V1)) # precios de $1.650.000.000 en Chapi?

train <- train %>% mutate (precio_mt2= price / surface_total)

summary(train$precio_mt2) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1=scales::dollar(V1)) #Se evidencia que precio por metro cuadrado minimo $20 mil y max $40 millones

Ploty_log <- ggplot(train, aes(x = precio_mt2)) + geom_histogram(fill="gold2", alpha= 0.9) +
  labs(x="Valor de venta (log-scale)", y="Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(Ploty_log)

Ploty <- ggplot(train, aes(x = price)) + geom_histogram(fill="gold2", alpha= 0.9) +
  labs(x="Valor de venta (log-scale)", y="Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(Ploty)

leaflet() %>%
  addTiles() %>%
  addCircles(lng= train$lon,
             lat= train$lat)

limites <- getbb("Localidad Chapinero") #b box

train <- train %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) &
      between(lat, limites[2, "min"], limites[2, "max"])
  )

train <- train %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "red",
                           property_type == "Casa"~ "blue"))
#Plot Map

latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Centrar precio metro cuadrado

train <- train %>%
  mutate(precio_por_mt2_sc=((precio_mt2 - min(precio_mt2))/max(precio_mt2) - min(precio_mt2)))

html <- paste0("<b>Precio:</b>",
               scales::dollar(train$price),
               "<br> <b>Area:</b>",
               as.integer(train$surface_total), "mt2",
               "<br> <b>Tipo de inmueble:</br>",
               train$property_type,
               "<br> <b>Numero de alcobas:</b>",
               as.integer(train$rooms),
               "<br><b>Numero de baños:</b>",
               as.integer(train$bathrooms))

m <- leaflet() %>%
  addTiles() %>%
  setView(lng= longitud_central, lat = latitud_central, zoom= 12) %>%
  addCircles(lng = train$lon,
             lat = train$lat,
             col = train$color,
             fillOpacity = 0.5,
             opacity = 0.5,
             radius = train$precio_por_mt2_sc*10,
             popup = html)

