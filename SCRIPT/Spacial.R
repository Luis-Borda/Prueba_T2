

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

####### *VARIABLE ESTACIONES DE TRANSMILENIO* (mismo procedimiento) ############

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

centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)


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



################################################################################
####              Obtenenemos las universidades                             ####
################################################################################



universidades<- bogota %>% 
  add_osm_feature(key="amenity",value="university") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_universidades<-universidades$osm_point
head(puntos_universidades)

ggplot()+
  geom_sf(data=puntos_universidades) +
  theme_bw()


UN_BOG <-universidades$osm_polygons
UN_BOG

ggplot()+
  geom_sf(data=UN_BOG) +
  theme_bw()

################################################################################

# Calculamos el centroide de cada universidad para aproximar s ubciacion como un solo punto 
centroidesU <- gCentroid(as(UN_BOG$geometry, "Spatial"), byid = T)


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


UN_BOG <-policia$osm_polygons
UN_BOG

ggplot()+
  geom_sf(data=UN_BOG) +
  theme_bw()

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

UN_BOG <-policia$osm_points
UN_BOG

ggplot()+
  geom_sf(data=UN_BOG) +
  theme_bw()


################################################################################
####              Obtenenemos Parqueaderos                                  ####
################################################################################


parking<- bogota %>% 
  add_osm_feature(key="amenity",value="parking") %>% 
  osmdata_sf() #transformamos a un objeto sf

puntos_parking<-parking$osm_point
head(puntos_parking)

ggplot()+
  geom_sf(data=puntos_parking) +
  theme_bw()


################################################################################
####              Obtenenemos Jardines infantiles                                  ####
################################################################################


jardines<- bogota %>% 
  add_osm_feature(key="amenity",value="kindergarten") %>% 
  osmdata_sf() 

puntos_jardines<-jardines$osm_point
head(puntos_jardines)

ggplot()+
  geom_sf(data=puntos_jardines) +
  theme_bw()


################################################################################
####              Obtenenemos colegios                                 ####
################################################################################


colegios<- bogota %>% 
  add_osm_feature(key="amenity",value="school") %>% 
  osmdata_sf() 

puntos_colegios<-colegios$osm_point
head(puntos_SC)

ggplot()+
  geom_sf(data=puntos_colegios) +
  theme_bw()


################################################################################
####              Obtenenemos salones comunales                                 ####
################################################################################


salones_comunales<- bogota %>% 
  add_osm_feature(key="amenity",value="community_centre") %>% 
  osmdata_sf() 

puntos_SC<-community_centre$osm_point
head(puntos_SC)

ggplot()+
  geom_sf(data=puntos_SC) +
  theme_bw()

################################################################################
####              Obtenenemos bares                                 ####
################################################################################


bar<- bogota %>% 
  add_osm_feature(key="amenity",value="bar") %>% 
  osmdata_sf() 

pbar<-bar$osm_point
head(puntos_SC)

ggplot()+
  geom_sf(data=pbar) +
  theme_bw()

################################################################################
####              Obtenenemos postales                                 ####
################################################################################


hospital<- bogota %>% 
  add_osm_feature(key="amenity",value="hospital") %>% 
  osmdata_sf() 

phospital<-hospital$osm_point
head(puntos_SC)

ggplot()+
  geom_sf(data=phospital) +
  theme_bw()



####CONVERTIMOS A DATA FRAME Y SALVAMOS EN CSV-------------------------------

###TRAIN------------
class(train)
p_load(sfheaders)
train_df<-sf_to_df(train, fill = TRUE)
class(train_df)

write_csv(train_df, file="Bogota_train.csv")

##TEST--------------------------
class(test)
test_df<-sf_to_df(test, fill = TRUE)
class(test_df)

write_csv(test_df, file="Bogota_test.csv")
####
##



