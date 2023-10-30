
#------------------------------------------------------------------------------
#                           
#                               Taller 2
#
#   Grupo 5:  Felipe Vargas.
#             Julián Muñoz .
#             Sebastián Potosí.
#             Luis Borda.
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                      2. Datos y pasos previos
#-------------------------------------------------------------------------------


#Limpieza de area de trabajo ---------------------------------------------------

rm(list = ls())

# Cargar pacman (contiene la función p_load)
library(pacman) 

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
       spatialsample) # Muestreo espacial para modelos de aprendizaje automático



#Importar los datos---------------------------------------------------------

train <- read.csv("E:/USB/MECA-CLASES/5. BIG DATA/TALLER 2/DATA/train.csv", row.names=1) 
                  
                  
train$description [1]

train <- train %>%
  mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))


numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
numeros_numericos <- as.character(1:10)

train <- train %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))

train <- train %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

train <- train %>%
  mutate(piso_numerico = ifelse(piso_numerico > 20, NA, piso_numerico))


train %>%
  filter(property_type_2 == "Apartamento") %>%
  count(piso_numerico)


train %>%
  filter(property_type_2 == "Casa") %>%
  count(piso_numerico)


train <- db %>%
  mutate(piso_numerico = replace_na(piso_numerico, 1))

#CREAMOS LA VARIABLE DE ÁREA TOTAL

##Volvemos todo minuscula y eliminamos tildes tildes
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

#CREAMOS LA VARIABLE DE PARQUEADEROS

#train <- train %>%
  mutate(parqueadero= str_extract(description, "(\\w+|\\d+) parqueadero (\\w+|\\d+)"))


#numeros_escritos <- c("parqueadero", "uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
#numeros_numericos <- as.character(1:10)

#train <- train %>%
  mutate(parqueadero = str_replace_all(parqueadero, setNames(numeros_numericos,numeros_escritos)))

#train <- train %>%
  mutate(parqueadero_numerico = as.integer(str_extract(parqueadero, "\\d+")))

#train <- train %>%
  mutate(parqueadero_numerico = ifelse(parqueadero_numerico > 20, NA, parqueadero_numerico))


#train %>%
  filter(property_type_2 == "Apartamento") %>%
  count(parqueadero_numerico)


train %>%
  filter(property_type_2 == "Casa") %>%
  count(piso_numerico)


train <- db %>%
  mutate(piso_numerico = replace_na(piso_numerico, 1))



# Determinamos el centro del mapa 
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)





