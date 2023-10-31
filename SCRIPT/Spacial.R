

# Cargamos librerias
require("pacman")

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un polígono
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample,# Muestreo espacial para modelos de aprendizaje automático
       tmaptools) 


available_features() %>% head(20)

available_tags("amenity")


test <- read_csv("E:/USB/MECA-CLASES/5. BIG DATA/TALLER 2/DATA/test.csv")
train <- read_csv("E:/USB/MECA-CLASES/5. BIG DATA/TALLER 2/DATA/train.csv")

bogota<-opq(bbox = getbb("Bogotá Colombia"))
bogota

################################################################################
####              CREAR VARIABLES                                           ####
################################################################################


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

#Plot Map

latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)
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


# Calculamos las distancias para cada combinacion immueble - parque

dist_matrix <- st_distance(x = db_sf, y = centroides_sf)

# Encontramos la distancia mínima a un parque

dist_min_parque <- apply(dist_matrix, 1, min)

# La agregamos como variablea nuestra base de datos original # *****************

train <- train %>% mutate(distancia_parque = dist_min_parque)

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

#########Ahora para test

latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)


db_sf <- st_as_sf(test, coords = c("lon", "lat"))

# Especificamos el sistema de coordenadas.

st_crs(db_sf) <- 4326

# convertimos los scontroides a formato sf(simple features)

centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))


# Calculamos las distancias para cada combinacion immueble - parque

dist_matrix <- st_distance(x = db_sf, y = centroides_sf)

# Encontramos la distancia mínima a un parque

dist_min_parque <- apply(dist_matrix, 1, min)

# La agregamos como variablea nuestra base de datos original # *****************

test <- test %>% mutate(distancia_parque = dist_min_parque)


# Se evalua si el tamaño del parque más cercano influye #

posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))

# De la geometria de los parques extraemos el aréa

areas <- st_area(parques_geometria)

#Agregamos la variable a nuestra base de datos original - SE INCLUYE VARIABLE POSICION -

test <- test %>%
  mutate(area_parque = as.numeric(areas[posicion]))


################################################################################
####### *VARIABLE ESTACIONES DE TRANSMILENIO* (mismo procedimiento) ############
################################################################################

# Extraemos la info de las estaciones del Transmilenio

transmilenio <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'bus_station') 

# Cambiamos el formato para que sea un objeto sf (simple features) (mismo proccedimiento anteriormente descrito)

transmilenio_sf <- osmdata_sf(transmilenio)

# De las features de estaciones nos interesa su geomoetría y donde están ubicados 

transmilenio_geometria <- transmilenio_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada estacione para aproximar su ubicación como un solo punto 

centroidest <- gCentroid(as(transmilenio_geometria$geometry, "Spatial"), byid = T)

# Creamos el mapa de Bogotá  con los paraderos

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = transmilenio_geometria, col = "red",weight = 5,
              opacity = 0.8, popup = transmilenio_geometria$name) %>%
  addCircles(data=centroidest,col = 'blue' , opacity = 0.5, radius = 1)


# Primero tomamos nuestros datos y los convertimos al formato sf (simple features)
# Esto para que esten en el mismo formato de los paraderos y poder calcuar distancias. 

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs=4326)

# convertimos los centroides a formato sf(simple features) 

centroides_sf <- st_as_sf(centroidest, coords = c("lon", "lat"), crs=4326)


# Calculamos las distancias a la estación mas cercana

nearest <- st_nearest_feature(train_sf,centroides_sf)

train<- train %>% mutate(distancia_bus=st_distance(x = train_sf, y = centroides_sf[nearest,], by_element=TRUE))


t <- ggplot(train, aes(x = distancia_bus)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de Transmilenio en metros", y = "Cantidad",
       title = "Distribución de la distancia a la estación de Transmilenio") +
  theme_bw()
library(units)
ggplotly(t)

#Se realiza el gráfico (plot) de la relación 

p <- ggplot(train%>%sample_n(1000), aes(x = area_parque, y = price)) +
  geom_point(col = "gold2", alpha = 0.4) +
  labs(x = "Área del parque más cercano (log-scale)", 
       y = "Valor del arriendo (log-scale)",
       title = "Relación entre área de un parque y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)




#######################TEST 
latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)


test_sf <- st_as_sf(test, coords = c("lon", "lat"))

# Especificamos el sistema de coordenadas.

st_crs(test_sf) <- 4326

# convertimos los scontroides a formato sf(simple features)

centroidest <- st_as_sf(centroidest, coords = c("lon", "lat"), crs=4326)


# Calculamos las distancias para cada combinacion immueble - parque

dist_matrix <- st_distance(x = test_sf, y = centroidest)

# Encontramos la distancia mínima a un parque

dist_min_transm <- apply(dist_matrix, 1, min)
nearestest <- st_nearest_feature(test_sf,centroidest)


# La agregamos como variablea nuestra base de datos original # *****************

test<- test %>% mutate(distancia_bus=st_distance(x = test_sf, y = centroidest[nearestest,], by_element=TRUE))



################################################################################
#################     DISTANCIA A AVENIDAS MAS CERCANAS         ################
################################################################################


train<-st_as_sf(train,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

avenidas <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "highway", value = "secondary")

avenidas_sf <- osmdata_sf(avenidas)

avenidas_geometria <- avenidas_sf$osm_lines%>%
  select (osm_id, name)

leaflet()%>%
  addTiles()%>%
  addPolygons(data = avenidas_geometria, col = "#F72585",
              opacity = 0.8, popup= avenidas_geometria)%>%
  addCircles(data=train)

#Busco la geometría más cercana
cercano <- st_nearest_feature(train,avenidas_geometria)
#calculo la distancia
dist <-st_distance(train, avenidas_geometria[cercano,], by_element=TRUE)
dist
train$distancia_avenida_principal<-dist

#########################TEST
test<-st_as_sf(test,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

avenidas <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "highway", value = "secondary")

avenidas_sf <- osmdata_sf(avenidas)

avenidas_geometria <- avenidas_sf$osm_lines%>%
  select (osm_id, name)

leaflet()%>%
  addTiles()%>%
  addPolygons(data = avenidas_geometria, col = "#F72585",
              opacity = 0.8, popup= avenidas_geometria)%>%
  addCircles(data=test)

#Busco la geometría más cercana
cercano <- st_nearest_feature(test,avenidas_geometria)
#calculo la distancia
dist <-st_distance(test, avenidas_geometria[cercano,], by_element=TRUE)
dist
test$distancia_avenida_principal<-dist



################################################################################
####              Obtenenemos las universidades                             ####
################################################################################


universidades<- bogota %>% 
  add_osm_feature(key="amenity",value="university") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_universidades<-universidades$osm_polygons
head(puntos_universidades)

ggplot()+
  geom_sf(data=puntos_universidades) +
  theme_bw()


################################################################################

# Calculamos el centroide de cada universidad para aproximar s ubciacion como un solo punto 
centroidesU <- gCentroid(as(puntos_universidades$geometry, "Spatial"), byid = T)


db_sf <- st_as_sf(train, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(db_sf) <- 4326
# convertimos los scontroides a formato sf(simple features)
centroides_U <- st_as_sf(centroidesU, coords = c("x", "y"))

# Calculamos las distancias para cada combinacion immueble - universidad
dist_matrix <- st_distance(x = db_sf, y = centroides_U)

# Encontramos la distancia mínima a una Universidad
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_universidad = dist_min)

p <- ggplot(train, aes(x = distancia_universidad)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una Universidad en metros", y = "Cantidad",
       title = "Distribución de la distancia a las Universidades") +
  theme_bw()
ggplotly(p)

#Relación del precio vs la distancia a la Universidad 
p <- ggplot(train%>%sample_n(1000), aes(x = distancia_universidad, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una Universidad en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a una Universidad y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

###############TEST

uni_sf <- st_as_sf(test, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(uni_sf) <- 4326
# convertimos los scontroides a formato sf(simple features)
centroides_U <- st_as_sf(centroidesU, coords = c("x", "y"))

# Calculamos las distancias para cada combinacion immueble - universidad
dist_matrix <- st_distance(x = uni_sf, y = centroides_U)

# Encontramos la distancia mínima a una Universidad
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(distancia_universidad = dist_min)



################################################################################
####              Obtenenemos Estaciones de policia                         ####
################################################################################



policia<- bogota %>% 
  add_osm_feature(key="amenity",value="police") %>% # de las amenities disponibles, seleccionamos la policia
  osmdata_sf() #transformamos a un objeto sf

puntos_policia<-policia$osm_point
head(puntos_policia)

ggplot()+
  geom_sf(data=puntos_policia) +
  theme_bw()

--------

pol_sf <- st_as_sf(train, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(db_sf) <- 4326

# Calculamos las distancias para cada combinacion immueble - policia
dist_matrix <- st_distance(x = pol_sf, y = puntos_policia)

# Encontramos la distancia mínima a la policia
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_policia = dist_min)

p <- ggplot(train, aes(x = distancia_policia)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a la policia en metros", y = "Cantidad",
       title = "Distribución de la distancia a la policia") +
  theme_bw()
ggplotly(p)

#Relación del precio vs la distancia a la policia 
pol <- ggplot(train%>%sample_n(1000), aes(x = distancia_policia, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a la policia en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a la policia y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(pol)

###############TEST


pol_sf <- st_as_sf(test, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(pol_sf) <- 4326

# Calculamos las distancias para cada combinacion immueble - universidad
dist_matrix <- st_distance(x = pol_sf, y = puntos_policia)

# Encontramos la distancia mínima a una Universidad
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(distancia_policia = dist_min)



################################################################################
####              Obtenenemos Restaurantes                                  ####
################################################################################


restaurant<- bogota %>% 
  add_osm_feature(key="amenity",value="restaurant") %>% # de las amenities disponibles, seleccionamos la policia
  osmdata_sf() #transformamos a un objeto sf

puntos_restaurant<-restaurant$osm_point
head(puntos_restaurant)

ggplot()+
  geom_sf(data=puntos_restaurant) +
  theme_bw()

--------
  
rest_sf <- st_as_sf(train, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(rest_sf) <- 4326

# Calculamos las distancias para cada combinacion immueble - restaurante
dist_matrix <- st_distance(x = rest_sf, y = puntos_restaurant)

# Encontramos la distancia mínima a los restaurantes
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(puntos_restaurant = dist_min)

p <- ggplot(train, aes(x = puntos_restaurant)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los restaurantes en metros", y = "Cantidad",
       title = "Distribución de la distancia a los restaurantes") +
  theme_bw()
ggplotly(p)

#Relación del precio vs la distancia a los restaurantes 
rest <- ggplot(train%>%sample_n(1000), aes(x = distancia_policia, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los restaurantes en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a los restaurantes y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(rest)

###############TEST


rest_sf <- st_as_sf(test, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(rest_sf) <- 4326

# Calculamos las distancias para cada combinacion immueble - restaurantes
dist_matrix <- st_distance(x = rest_sf, y = puntos_restaurant)

# Encontramos la distancia mínima a un restaurante
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(puntos_restaurant = dist_min)

################################################################################
####              Obtenenemos Jardines infantiles                                  ####
################################################################################


jardines<- bogota %>% 
  add_osm_feature(key="amenity",value="kindergarten") %>% 
  osmdata_sf() 

puntos_jar<-jardines$osm_point
head(puntos_jar)

ggplot()+
  geom_sf(data=puntos_jar) +
  theme_bw()

--------
  
jar_sf <- st_as_sf(train, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(jar_sf) <- 4326

# Calculamos las distancias para cada combinacion immueble - jardines inf
dist_matrixj <- st_distance(x = jar_sf, y = puntos_jar)

# Encontramos la distancia mínima a los jardines inf
dist_minj <- apply(dist_matrixj, 1, min)
# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(dist_jar = dist_minj)

jf <- ggplot(train, aes(x = dist_minj)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los jardines en metros", y = "Cantidad",
       title = "Distribución de la distancia a los jardines") +
  theme_bw()
ggplotly(jf)

#Relación del precio vs la distancia a los jardines infantiles 
jar <- ggplot(train%>%sample_n(1000), aes(x = dist_minj, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los restaurantes en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a los restaurantes y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(jar)

###############TEST


jar_sf <- st_as_sf(test, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(rest_sf) <- 4326

# Calculamos las distancias para cada combinacion immueble - jardines
dist_matrix <- st_distance(x = jar_sf, y = puntos_jar)

# Encontramos la distancia mínima a un restaurante
dist_min <- apply(dist_matrix, 1, min)
# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(puntos_jar = dist_min)


################################################################################
####              Obtenenemos colegios                                 ####
################################################################################


colegios<- bogota %>% 
  add_osm_feature(key="amenity",value="school") %>% 
  osmdata_sf() 

puntos_SC<-colegios$osm_point
head(puntos_SC)

ggplot()+
  geom_sf(data=puntos_colegios) +
  theme_bw()

--------
  
cole_sf <- st_as_sf(train, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(cole_sf) <- 4326

# Calculamos las distancias para cada combinacion immueble - colegios
dist_matrixc <- st_distance(x = cole_sf, y = puntos_SC)

# Encontramos la distancia mínima a los colegios
dist_minc <- apply(dist_matrixc, 1, min)
# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(puntos_SC = dist_minc)

cf <- ggplot(train, aes(x = dist_minc)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los colegios en metros", y = "Cantidad",
       title = "Distribución de la distancia a los colegios") +
  theme_bw()
ggplotly(cf)

#Relación del precio vs la distancia a los colegios 
cfp <- ggplot(train%>%sample_n(1000), aes(x = dist_minc, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los colegios en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a los colegios y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(cfp)

###############TEST


cole_sf <- st_as_sf(test, coords = c("lon", "lat"))
# Especificamos el sistema de coordenadas.
st_crs(cole_sf) <- 4326

# Calculamos las distancias para cada combinacion immueble - colegios
dist_matrixc <- st_distance(x = cole_sf, y = puntos_SC)

# Encontramos la distancia mínima a un restaurante
dist_minc <- apply(dist_matrixc, 1, min)
# La agregamos como variablea nuestra base de datos original 
test <- test %>% mutate(puntos_SC = dist_minc)


####CONVERTIMOS A DATA FRAME Y SALVAMOS EN CSV-------------------------------

###TRAIN------------
class(train)
p_load(sfheaders)
train_df<-sf_to_df(train, fill = TRUE)
class(train_df)

write_csv(train_df, file="train.csv")

##TEST--------------------------
class(test)
test_df<-sf_to_df(test, fill = TRUE)
class(test_df)

write_csv(test_df, file="test.csv")
####
##
