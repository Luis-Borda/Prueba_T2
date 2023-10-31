###############################################################################
############################# MODELO DE ARBOLES ###############################
###############################################################################


rm(list = ls())

library(readr)
library(pacman)
p_load(tidyverse, readr, skimr, fastDummies, caret, glmnet, MLmetrics)

#Importar datos

testf <- read_csv("C:/Users/Hp/Documents/MeCA - Big Data and Machine Learning/Set_2/Set_2/testf.csv")
trainf <- read_csv("C:/Users/Hp/Documents/MeCA - Big Data and Machine Learning/Set_2/Set_2/trainf.csv")

####DIVIDIMOS LA MUESTRA EN TRAIN Y TEST

set.seed(1022)
inTrain <- createDataPartition(
  y = trainf$price,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

train_muestra <- trainf[ inTrain,]
test_muestra  <- trainf[-inTrain,]


##ARBOLES---------------------------------------------------------------------

set.seed(123)


#Cross validation V=5

cv10<- trainControl(number = 10, method ="cv")

arbol_1<-train(price~surface_total+surface_covered+rooms+bedrooms+bathrooms+
                  distancia_parque+area_parque+distancia_bus+
                  distancia_avenida_principal+distancia_universidad+distancia_policia+
                  dist_jar+puntos_SC+dist_cole+property_type,
               data=trainf,
               method="rpart",
               trControl = cv10)

p_load(rattle)
fancyRpartPlot(arbol_1$finalModel)

pred_train_arbol_muestra<-predict(arbol_1, newdata=train_muestra)
pred_test_arbol_muestra<-predict(arbol_1, newdata=test_muestra)


#Error en porcentaje
MAPE(y_pred=pred_train_arbol_muestra, y_true = train_muestra$price)
#0.3619813

#Error promedio
MAE(y_pred=pred_train_arbol_muestra, y_true = train_muestra$price)
#213703743


## FUERA DE MUESTRA

#Error en porcentaje
MAPE(y_pred=pred_test_arbol_muestra, y_true = test_muestra$price)
#0.3605517

#Error promedio
MAE(y_pred=pred_test_arbol_muestra, y_true = test_muestra$price)
#213489943


# Se utilizaron modelos de arboles, dado que, estos aprenden las formas funcionales
# no lineales. A partir de particiones recursivas binarias, dividen el espacio variable
# por variable

# De los resultados, se evidencia que tras probar "hoja por hoja" se escoge el error
# estandar menor.

# Se busca el menor MAE.

# El MAE del modelo fue 213.489.943



##RANDOM FOREST------------------------------------------------------------

set.seed(123)

summary(train_muestra)

#Cross validation V=8

cv10<- trainControl(number = 10, method ="cv")

tunegrid_rf<-expand.grid(mtry=c(2,3,4,5, 8), #Predictores aleatorios
                         splitrule= "variance", ##Cambiar por gini y revisar
                         min.node.size=c(1,2,3,6))


rforest<-train(price~surface_total+surface_covered+rooms+bedrooms+bathrooms+
                 distancia_parque+area_parque+distancia_bus+
                 distancia_avenida_principal+distancia_universidad+distancia_policia+
                 dist_jar+puntos_SC+dist_cole+property_type,
               data=train_muestra, 
               trControl = cv10,
               metric = "RMSE",
               tuneGrid = tunegrid_rf,
               method ="ranger")


plot(rforest)

pred_train_2_muestra<-predict(rforest, newdata=train_muestra)
pred_test_2_muestra<-predict(rforest, newdata=test_muestra)

##EN MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_train_2_muestra, y_true = train_muestra$price)

#Error promedio
MAE(y_pred=pred_train_2_muestra, y_true = train_muestra$price)

## FUERA DE MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_test_2_muestra, y_true = test_muestra$price)

#Error promedio
MAE(y_pred=pred_test_2_muestra, y_true = test_muestra$price)