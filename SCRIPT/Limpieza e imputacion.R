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
       stargazer,
       readr,
       yardstick,
       GGally,
       rattle,
       randomForest,
       C50)

test <- read_csv("C:/Users/lordb/Documents/Prueba_T2/SCRIPT/test.csv")
train <- read_csv("C:/Users/lordb/Documents/Prueba_T2/SCRIPT/train.csv")

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

train %>%
  summarise_all(~sum(is.na(.)))

test %>%
  summarise_all(~sum(is.na(.)))


#
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

## Imputación de datos faltantes por mediana y moda

table(train$rooms) #Se evidencia que la mayoría de viviendas tienen 3 cuartos, por lo tanto se imputan los datos con moda = 3
table(train$bathrooms) #Se evidencia que la mayoría de viviendas tienen 2 cuartos, por lo tanto se imputan los datos con moda = 2

table(test$rooms) #Se evidencia que la mayoría de viviendas tienen 3 cuartos, por lo tanto se imputan los datos con moda = 3
table(test$bathrooms) #Se evidencia que la mayoría de viviendas tienen 2 cuartos, por lo tanto se imputan los datos con moda = 2


#El mismo análisis se hace con los datos en la base test

m_sup_cubierta <- median(train$surface_covered, na.rm = T) 
m_sup_total <- median(train$surface_total, na.rm = T)


m_sup_cubierta <- median(test$surface_covered, na.rm = T) 
m_sup_total <- median(test$surface_total, na.rm = T)


train <- train %>%
  mutate(rooms= replace_na(rooms, 3), #No se coloca bedrooms, toda vez que, para train no se evidencian NA's
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, m_sup_cubierta),
         surface_total = replace_na(surface_total, m_sup_total))

test <- test %>%
  mutate(rooms= replace_na(rooms, 3), #No se coloca bedrooms, toda vez que, para train no se evidencian NA's
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, m_sup_cubierta),
         surface_total = replace_na(surface_total, m_sup_total))

m_test_cubierta <- median(train$surface_covered, na.rm = T) 
m_test_total <- median(train$surface_total, na.rm = T)

m_test_cubierta <- median(test$surface_covered, na.rm = T) 
m_test_total <- median(test$surface_total, na.rm = T)

test <- test %>%
  mutate(rooms= replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, m_test_cubierta),
         surface_total = replace_na(surface_total, m_test_total))

train <- train %>%
  mutate(rooms= replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, m_test_cubierta),
         surface_total = replace_na(surface_total, m_test_total))

# Análisis de la distribución de precios

summary(train$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1=scales::dollar(V1)) # precios de $1.650.000.000 en Chapi?

summary(test$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1=scales::dollar(V1)) # precios de $1.650.000.000 en Chapi?


train <- train %>% mutate (precio_mt2= price / surface_total)

test <- test %>% mutate (precio_mt2= price / surface_total)

summary(train$precio_mt2) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1=scales::dollar(V1)) #Se evidencia que precio por metro cuadrado minimo $20 mil y max $40 millones

summary(test$precio_mt2) %>%
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

###
Ploty_log <- ggplot(test, aes(x = precio_mt2)) + geom_histogram(fill="gold2", alpha= 0.9) +
  labs(x="Valor de venta (log-scale)", y="Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(Ploty_log)

Ploty <- ggplot(test, aes(x = price)) + geom_histogram(fill="gold2", alpha= 0.9) +
  labs(x="Valor de venta (log-scale)", y="Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(Ploty)




leaflet() %>%
  addTiles() %>%
  addCircles(lng= train$lon,
             lat= train$lat)


leaflet() %>%
  addTiles() %>%
  addCircles(lng= test$lon,
             lat= test$lat)


###
limites1 <- getbb("Bogotá Colombia") #b box
limites2 <- getbb("Localidad Chapinero") #b box



train <- train %>%
  filter(
    between(lon, limites1[1, "min"], limites1[1, "max"]) &
      between(lat, limites1[2, "min"], limites1[2, "max"])
  )

train <- train %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "red",
                           property_type == "Casa"~ "blue"))


test <- test %>%
  filter(
    between(lon, limites2[1, "min"], limites2[1, "max"]) &
      between(lat, limites2[2, "min"], limites2[2, "max"])
  )

test <- test %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "red",
                           property_type == "Casa"~ "blue"))



#Plot Map

latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)


latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)


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

#######TEST


# Centrar precio metro cuadrado

test <- test %>%
  mutate(precio_por_mt2_sc=((precio_mt2 - min(precio_mt2))/max(precio_mt2) - min(precio_mt2)))

html <- paste0("<b>Precio:</b>",
               scales::dollar(train$price),
               "<br> <b>Area:</b>",
               as.integer(train$surface_total), "mt2",
               "<br> <b>Tipo de inmueble:</br>",
               test$property_type,
               "<br> <b>Numero de alcobas:</b>",
               as.integer(test$rooms),
               "<br><b>Numero de baños:</b>",
               as.integer(test$bathrooms))

m2 <- leaflet() %>%
  addTiles() %>%
  setView(lng= longitud_central, lat = latitud_central, zoom= 12) %>%
  addCircles(lng = test$lon,
             lat = test$lat,
             col = test$color,
             fillOpacity = 0.5,
             opacity = 0.5,
             radius = test$precio_por_mt2_sc*10,
             popup = html)

train$property_type <- as.factor(train$property_type)
str(train$property_type)


ggpairs(train, columns = 2:8, aes(colour = property_type),
        lower = list(continuous = "points", combo = "facethist"),
        diag = list(continuous = "barDiag", combo = "densityDiag")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),  # Ocultar etiquetas de los ejes
        axis.title = element_blank())  # Ocultar títulos de los ejes



#<<<<<<< HEAD
#=======
colnames(test)[24] <- "dist_jar"
colnames(test)[23] <- "dist_cole"

#>>>>>>> 958ef5e6dc27e6ec712c45f97c2864b23f2625c4


####CONVERTIMOS A DATA FRAME Y SALVAMOS EN CSV-------------------------------


###TRAIN------------
class(train)
p_load(sfheaders)
train_df<-sf_to_df(train, fill = TRUE)
class(train_df)

write_csv(train_df, file="trainf.csv")

##TEST--------------------------
class(test)
test_df<-sf_to_df(test, fill = TRUE)
class(test_df)

write_csv(test_df, file="testf.csv")
####


