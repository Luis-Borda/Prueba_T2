

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
# Esto va a ser demorado!
# Calculamos las diatnacias para cada combinacion immueble - parque
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





