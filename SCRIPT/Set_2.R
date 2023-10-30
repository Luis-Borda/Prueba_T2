################################################################################
################################ PROBLEM SET 2 #################################
################################################################################

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

#Se visualizan los datos de manera preliminar

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

#Se hace un análisis provisional de la base de datos, en el cual se corrobora que, efectivamente los datos
#corresponden a la ciudad de Bogotá, para casas y apartamentos que están en venta en la localidad de
#chapinero

#Limpiando base

skim(train)
skim(test)

# Se analizan los NA's de la base para imputación

test %>%
  summarise_all(~sum(is.na(.)))

train %>%
  summarise_all(~sum(is.na(.)))

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

#Revisar luego:
#------------------------------------------------------------------------------#
help(kNN)

train <-  kNN(train, variable = c("bathrooms"), k = 6)
train$bathrooms <- round(train$bathrooms,0)
summary(train$bathrooms)

test <-  kNN(test, variable = c("bathrooms"), k = 6)
test$bathrooms <- round(test$bathrooms,0)
summary(test$bathrooms)
#------------------------------------------------------------------------------#

# Cifra en pesos

summary(train$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1=scales::dollar(V1)) # precios de $1.650.000.000 en Chapi?

train <- train %>% mutate (precio_mt2= price / surface_total)

summary(train$precio_mt2) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1=scales::dollar(V1)) #Se evidencia que precio por metro cuadrado minimo $20 mil y max $40 millones

#filtro
#train <- train %>%
# filter(between(precio_mt2, 100000, 30e6))

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


# Se realiza mapa observable de lo que se tiene

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
  mutate(color = case_when(property_type == "Apartamento" ~ "springgreen4",
                           property_type == "Casa"~ "turquoise3"))

train <- train %>%
  filter(surface_covered > 20)

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


# Generando variables

#------------------------------------------------------------------------------#

# Se crea la variable "parques"

# Se extrae la información de todos los parques de Bogotá

parques <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 

# Se cambia el formato para que sea un objeto sf (simple features)

parques_sf <- osmdata_sf(parques)

# De las features del parque, interesa su geomoetría y donde estan ubicados 

parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar su ubciacion como un solo punto 

centroides <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)

# Creamos el mapa de Bogotá 

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "darkblue", opacity = 1, radius = 1)

# Ahora se calcula la distancia de cada apartamento al centroide de cada parque
# Primero, se toman los datos geoespaciales y se convierten al formato sf (simple features)
# Esto para que esten en el mismo formato de los parques y poder sacar distancia. 

db_sf <- st_as_sf(train, coords = c("lon", "lat"))

# Especificamos el sistema de coordenadas.

st_crs(db_sf) <- 4326

# convertimos los scontroides a formato sf(simple features)

centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))

# Esto va a ser demorado!

# Calculamos las diatnacias para cada combinacion immueble - parque

dist_matrix <- st_distance(x = db_sf, y = centroides_sf)

# Encontramos la distancia mínima a un parque

dist_min <- apply(dist_matrix, 1, min)

# La agregamos como variablea nuestra base de datos original # *****************

train <- train %>% mutate(distancia_parque = dist_min)

p <- ggplot(train, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "gold", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p)

#Relación del precio vs la distancia al parque 

p <- ggplot(train%>%sample_n(1000), aes(x = distancia_parque, y = price)) +
  geom_point(col = "gold2", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a un parque y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

# Se evalua si el tamaño del parque más cercano influye #

posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))

# De la geometria de los parques extraemos el aréa

areas <- st_area(parques_geometria)

#Agregamos la variable a nuestra base de datos original - SE INCLUYE VARIABLE POSICION -

train <- train %>%
  mutate(area_parque = as.numeric(areas[posicion]))

# Se realiza el gráfico (plot) de la relación 

p <- ggplot(train%>%sample_n(1000), aes(x = area_parque, y = price)) +
  geom_point(col = "gold2", alpha = 0.4) +
  labs(x = "Área del parque más cercano (log-scale)", 
       y = "Valor del arriendo (log-scale)",
       title = "Relación entre área de un parque y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

####### *VARIABLE VIAS* (mismo procedimiento) ############

# Extraemos la info de las estaciones del Transmilenio

parada_de_bus <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'bus_station') 

# Cambiamos el formato para que sea un objeto sf (simple features) (mismo proccedimiento anteriormente descrito)

parada_de_bus_sf <- osmdata_sf(parada_de_bus)

# De las features de estaciones nos interesa su geomoetría y donde están ubicados 

parada_de_bus_sf_geometria <- parada_de_bus_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada estacione para aproximar su ubicación como un solo punto 

centroides <- gCentroid(as(parada_de_bus_sf_geometria$geometry, "Spatial"), byid = T)
#** aquí tenias este código en comentario*#

centroides <-st_centroid(parada_de_bus_sf_geometria$geometry)

# Creamos el mapa de Bogotá  con los paraderos

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parada_de_bus_sf_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = parada_de_bus_sf_geometria$name) %>%
  addCircles(data=centroides,col = 'blue' , opacity = 0.5, radius = 1)

#lng = centroides$x, Por qué no pusimos esto?
#            lat = centroides$y, 
#            col = '#698B69' , opacity = 0.5, radius = 1)

# Primero tomamos nuestros datos y los convertimos al formato sf (simple features)
# Esto para que esten en el mismo formato de los paraderos y poder calcuar distancias. 

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs=4326)

# convertimos los centroides a formato sf(simple features) 

centroides_sf <- st_as_sf(centroides, coords = c("lon", "lat"), crs=4326)
#** aquí tenias este código en comentario* x2 ¿Why?#

centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)


# Esto va a ser demorado!

# Calculamos las distancias al paradero mas cercano

nearest <- st_nearest_feature(db_sf,centroides_sf)

train<- train %>% mutate(distancia_bus=st_distance(x = db_sf, y = centroides_sf[nearest,], by_element=TRUE))


p <- ggplot(train, aes(x = distancia_bus)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parad de bus en metros", y = "Cantidad",
       title = "Distribución de la distancia a las parada de bus") +
  theme_bw()
library(units)
ggplotly(p)



# Creando variable AREA DESDE LA COLUMNA DESCRIPCION #

##---->Para train

substr(train$description, 1, 500)

##Volvemos todo minuscula y eliminamos tildes tildes
train$description <- tolower(train$description)
train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")
substr(train$description, 1, 500)

Metros <- str_extract(train$description, "\\d+\\s*(mts|m2|metros)")
summary(Metros)

train <- cbind(train, area = Metros)

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

train <- cbind(train, area_m2 = Area_sin_texto)
sapply(train, function(x) sum(is.na(x)))
train$area_tot <- ifelse(is.na(train$surface_covered), train$area_m2, train$surface_covered)

sapply(train, function(x) sum(is.na(x)))

##---->Para test

substr(test$description, 1, 500)

##Volvemos todo minuscula y eliminamos tildes tildes
test$description <- tolower(test$description)
test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")
substr(test$description, 1, 500)

Metros <- str_extract(test$description, "\\d+\\s*(mts|m2|metros)")

test <- cbind(test, area = Metros)

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

test <- cbind(test, area_m2 = Area_sin_texto)
sapply(test, function(x) sum(is.na(x)))
test$area_tot <- ifelse(is.na(test$surface_covered), test$area_m2, test$surface_covered)

sapply(test, function(x) sum(is.na(x)))








#------------------------------------------------------------------------------#
train$rooms[is.na(train$rooms)] <- 0
train$rooms_tot <- apply(train[, c("rooms", "bedrooms")], 1, max)

test$rooms[is.na(test$rooms)] <- 0
test$rooms_tot <- apply(test[, c("rooms", "bedrooms")], 1, max)

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

# Creando variable area desde descripción

##---->Para train
substr(train$description, 1, 500)

library(stringi)

##Volvemos todo minuscula y eliminamos tildes
train$description <- tolower(train$description)
train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")
substr(train$description, 1, 500)

Metros <- str_extract(train$description, "\\d+\\s*(mts|m2|metros)")

train <- cbind(train, area = Metros)

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

train <- cbind(train, area_m2 = Area_sin_texto)
sapply(train, function(x) sum(is.na(x)))
train$area_tot <- ifelse(is.na(train$surface_covered), train$area_m2, train$surface_covered)

sapply(train, function(x) sum(is.na(x)))


##---->Para test

substr(test$description, 1, 500)

##Volvemos todo minuscula y eliminamos tildes

test$description <- tolower(test$description)
test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")
substr(test$description, 1, 500)

Metros <- str_extract(test$description, "\\d+\\s*(mts|m2|metros)")

test <- cbind(test, area = Metros)

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

test <- cbind(test, area_m2 = Area_sin_texto)
sapply(test, function(x) sum(is.na(x)))
test$area_tot <- ifelse(is.na(test$surface_covered), test$area_m2, test$surface_covered)

sapply(test, function(x) sum(is.na(x)))

# Se prefiere imputacion de datos por KNN en lugar de metodos convencionales de mediana y moda

train <-  kNN(train, variable = c("area_tot"), k = 6)

train$area_tot_num <- as.numeric(train$area_tot)
train$area_tot <- round(train$area_tot_num,0)

summary(train$area_tot)

test <-  kNN(test, variable = c("area_tot"), k = 6)

test$area_tot_num <- as.numeric(test$area_tot)
test$area_tot <- round(test$area_tot_num,0)
summary(test$area_tot)

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

#---------------------Selección de variables para el modelo--------------------#

train_modelo <- select(train, property_id,  bathrooms, area_tot, bathrooms_imp, 
                lat, property_type, lon, distancia_parque, distancia_universidad, 
                rooms_tot, area_tot_imp,  price, distancia_avenida_principal)

test_modelo <- select(test, property_id,  bathrooms, area_tot, bathrooms_imp, 
               lat, property_type, lon, distancia_parque, distancia_universidad, 
               rooms_tot, area_tot_imp,  price, distancia_avenida_principal)

#Eliminando outliers

# Calcular cuartiles y rango intercuartil

train_new <- train_modelo
test_new <- test_modelo

#-----> para train

iqr_area_tot <- IQR(train_new$area_tot)

lim_inf <- quantile(train_new$area_tot, 0.25) - 1.5 * iqr_area_tot
lim_sup <- quantile(train_new$area_tot, 0.75) + 1.5 * iqr_area_tot

outliers_area_tot <- train_new$area_tot < lim_inf | train_new$area_tot > lim_sup

train_sin_outliers <- train_new[!outliers_area_tot,]

#-----> para test

iqr_area_tot <- IQR(test_new$area_tot)

lim_inf <- quantile(test_new$area_tot, 0.25) - 1.5 * iqr_area_tot
lim_sup <- quantile(test_new$area_tot, 0.75) + 1.5 * iqr_area_tot

outliers_area_tot <- test_new$area_tot < lim_inf | test_new$area_tot > lim_sup

test_sin_outliers <- test_new[!outliers_area_tot,]

ggplot(train_sin_outliers, aes(x = area_tot)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia")

ggplot(test_sin_outliers, aes(x = property_type)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia")



ggplot(data = train_sin_outliers, aes(x = area_tot, y = price)) +
  geom_point()

##------------------------Estadísticas descriptivas--------------------------###

summary(train_sin_outliers)
p_load(stargazer)

stargazer(train_sin_outliers, header = FALSE, type = "text", title = "Variables en la base Train")

ggplot(data = train_sin_outliers, aes(x=area_tot, y = price)) + 
  geom_point(color = "#023e8a", size = 0.3) + 
  labs (x = "Area", y = "Precio") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)

ggplot(data = train_sin_outliers, aes(x=rooms_tot, y = price)) + 
  geom_point(color = "darkblue", size = 0.3) + 
  labs (x = "Area", y = "Precio") +
  scale_y_continuous(labels = scales::dollar)

ggplot(data = train_sin_outliers, aes(x=distancia_avenida_principal, y = price)) + 
  geom_point(color = "darkblue", size = 0.3) + 
  theme_grey() +
  labs (x = "Area", y = "Precio") +
  scale_y_continuous(labels = scales::dollar)

ggplot(data = train_sin_outliers, aes(y = price, x = as.factor(tipo_propiedad))) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  labs (x = "Tipo Propiedad", y = "Precio")

ggplot(data = train_sin_outliers, aes(x = area_tot)) +
  geom_histogram()

#---------------convertir los valores caracter a lógicos--------------------

tipo_propiedad <- model.matrix(~ property_type - 1, train_sin_outliers)

# unir las variables dummies al data frame original
train_sin_outliers <- cbind(train_sin_outliers, tipo_propiedad)

# verificar el nuevo data frame
train_sin_outliers

train_sin_outliers$tipo_propiedad <- 
  factor(ifelse(train_sin_outliers$property_typeCasa ==1, "Casa", "Apartamento"))

##---------------------------Regresión Lineal--------------------------------###

sapply(train_sin_outliers, function(x) sum(is.na(x)))

modelo <- lm(price ~area_tot + lat + lon + distancia_universidad + area_tot_imp +
               distancia_avenida_principal + bathrooms + bathrooms_imp + property_type + 
               distancia_parque + rooms_tot, data = train_sin_outliers)

head(modelo)

p_load("stargazer")

stargazer(modelo, title = "Resultados de la regresión lineal", type = "text")


##-----------Superlearner--------##

bd<- train_sin_outliers %>% mutate(logprice=log(price)) 

p_load("caret")
p_load("SuperLearner")

set.seed(1011)
inTrain <- createDataPartition(
  y = bd$logprice,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

bdtrain_is <- bd[ inTrain,]
bdtest_is  <- bd[-inTrain,]
colnames(bdtrain_is)

#Modelos disponibles
listWrappers()

ySL<- bdtrain_is$price
XSL<- bdtrain_is  %>% select(area_tot, lat, lon, distancia_universidad, area_tot_imp,
                             distancia_avenida_principal, bathrooms, bathrooms_imp, property_type,  
                             distancia_parque, rooms_tot)

sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

# Fit using the SuperLearner package, 
install.packages("randomForest")

fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY

bdtest_is <- bdtest_is  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(bdtest_is), onlySL = T)$pred)
head(bdtest_is$yhat_Sup)

test <- test  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(test), onlySL = T)$pred)
head(test$yhat_Sup)

MAE_S4 <- with(bdtest_is,mean(abs(price-yhat_Sup))) #MAE
MAE_S4

#Redondear precio predicho

bdtest_is$price_round <- round(bdtest_is$yhat_Sup/1e5)*1e5
head(bdtest_is$price_round)

MAE_S4R <- with(bdtest_is,mean(abs(price-price_round))) #MAE
MAE_S4R

# Redondear en test
test$price_round <- round(test$price/1e4)*1e4

head(test$price_round)

test$price <- ifelse(is.na(test$price), test$yhat_Sup, test$price)


##------------Submission----------#####

test<- test  %>% mutate(Pred=(yhat_Sup))
colnames(test)

Submission4 <- test %>%
  select(property_id, yhat_Sup)

Submission4 <- Submission4 %>%
  rename(Price = yhat_Sup)

setwd("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/")

write.csv(Submission4, file="submission4.csv", row.names = F)



