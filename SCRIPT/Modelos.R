
rm(list = ls())

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
       tmaptools,
       readr,
       skimr,
       caret) 


test <- read_csv("C:/Users/lordb/Documents/Prueba_T2/SCRIPT/testf.csv")
train <- read_csv("C:/Users/lordb/Documents/Prueba_T2/SCRIPT/trainf.csv")

colnames(test)[24] <- "dist_jar"
colnames(test)[23] <- "dist_cole"


train$property_type <- as.factor(train$property_type)

set.seed(123)
fitControl <- trainControl(
  method = "cv",
  number = 10)

fmla<-formula(price ~ surface_total+surface_covered+rooms+bedrooms+bathrooms+
                distancia_parque+area_parque+distancia_bus+
                distancia_avenida_principal+distancia_universidad+distancia_policia+
                dist_jar+puntos_SC+dist_cole+property_type)

linear_reg<-train(fmla,
                  data=train,
                  method = 'lm', 
                  trControl = fitControl,
                  preProcess = c("center", "scale")
) 



linear_reg
summary(linear_reg)

y_hat_reg <- predict(linear_reg, newdata = test)

#------ Modelo Ridge --------------------------#

ridge<-train(fmla,
             data=train,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 0, #Ridge
                                    lambda = seq(10000000, 20000000,by = 10000)),
             preProcess = c("center", "scale")
) 

plot(ridge$results$lambda,
     ridge$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE)"
)

ridge$bestTune

coef_ridge<-coef(ridge$finalModel, ridge$bestTune$lambda)
coef_ridge

modelo_ridge<-train(fmla,
                    data=train,
                    method = 'glmnet', 
                    trControl = fitControl,
                    tuneGrid = expand.grid(alpha = 0, #Ridge
                                           lambda = 14880000),
                    preProcess = c("center", "scale")
) 

y_hat_ridge <- predict(modelo_ridge, newdata = test)

## Modelo Lasso

lasso<-train(fmla,
             data=train,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 1, #lasso
                                    lambda = seq(10000,1000000,by = 1000)),
             preProcess = c("center", "scale")
) 

plot(lasso$results$lambda,
     lasso$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE) Lasso"
)

lasso$bestTune

coef_lasso<-coef(lasso$finalModel, lasso$bestTune$lambda)
coef_lasso

modelo_lasso<-train(fmla,
                    data=train,
                    method = 'glmnet', 
                    trControl = fitControl,
                    tuneGrid = expand.grid(alpha = 1, #lasso
                                           lambda = 320000),
                    preProcess = c("center", "scale")
) 

y_hat_lasso <- predict(modelo_lasso, newdata = test)

## Elastic Net

EN<-train(fmla,
          data=train,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1), #grilla de alpha
                                 lambda = seq(100000,10000000,by = 10000)),
          preProcess = c("center", "scale")
) 

EN$bestTune

coef_EN<-coef(EN$finalModel,EN$bestTune$lambda)
coef_EN

modelo_EN<-train(fmla,
                 data=train,
                 method = 'glmnet', 
                 trControl = fitControl,
                 tuneGrid = expand.grid(alpha = 0.9, #grilla de alpha
                                        lambda = 320000),
                 preProcess = c("center", "scale")
) 

y_hat_EN <- predict(modelo_EN, newdata = test)

## Tabla: Coeficientes de los modelos

coefs_df<-cbind(coef(linear_reg$finalModel),as.matrix(coef_ridge),as.matrix(coef_lasso),as.matrix(coef_EN))
colnames(coefs_df)<-c("OLS","RIDGE","LASSO","ELASTIC_NET")
round(coefs_df,4)

RMSE_df<-cbind(linear_reg$results$RMSE,ridge$results$RMSE[which.min(ridge$results$lambda)],lasso$results$RMSE[which.min(lasso$results$lambda)],EN$results$RMSE[which.min(EN$results$lambda)])
colnames(RMSE_df)<-c("OLS","RIDGE","LASSO","EN")
RMSE_df

# Para enviar valores de prediccion

# y_hat_reg
# y_hat_ridge
# y_hat_lasso
# y_hat_EN

price <- y_hat_lasso
submission_template <- select(submission_template, -price)
submission_template <- cbind(submission_template, price)
names(submission_template)[ncol(submission_template)] <- "price"

p4 <- ggplot(submission_template, aes(x = price)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "precio en pesos ($)", y = "Cantidad",
       title = "Distribución del precio") +
  theme_bw()
p4

summary(submission_template$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))


# ------------- Exportando archivo ------------------------

write.csv(submission_template, file="submission_template.csv",  row.names = F)
submission_template1 <- read_csv("submission_template1.csv")

##-----------Superlearner--------##

bd<- train %>% mutate(logprice=log(price)) 

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
XSL<- select(bdtrain_is,surface_total,surface_covered,rooms,bedrooms,bathrooms,
                               distancia_parque,area_parque,distancia_bus,
                               distancia_avenida_principal,distancia_universidad,distancia_policia,
                               dist_jar,puntos_SC,dist_cole,property_type)
sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

# Fit using the SuperLearner package, 
install.packages("randomForest")


fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS2", # combinación convexa
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




set.seed(123)
fitControl <- trainControl(
  method = "cv",
  number = 10)

fmla<-formula(price ~ surface_total+surface_covered+rooms+bedrooms+bathrooms+
                distancia_parque+area_parque+distancia_bus+
                distancia_avenida_principal+distancia_universidad+distancia_policia+
                dist_jar+puntos_SC+dist_cole+property_type)

linear_reg<-train(fmla,
                  data=train,
                  method = 'lm', 
                  trControl = fitControl,
                  preProcess = c("center", "scale")
) 



linear_reg
summary(linear_reg)

y_hat_reg <- predict(linear_reg, newdata = test)

#------ Modelo Ridge --------------------------#

ridge<-train(fmla,
             data=train,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 0, #Ridge
                                    lambda = seq(10000000, 20000000,by = 10000)),
             preProcess = c("center", "scale")
) 

plot(ridge$results$lambda,
     ridge$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE)"
)

ridge$bestTune

coef_ridge<-coef(ridge$finalModel, ridge$bestTune$lambda)
coef_ridge

modelo_ridge<-train(fmla,
                    data=train,
                    method = 'glmnet', 
                    trControl = fitControl,
                    tuneGrid = expand.grid(alpha = 0, #Ridge
                                           lambda = 14880000),
                    preProcess = c("center", "scale")
) 

y_hat_ridge <- predict(modelo_ridge, newdata = test)

## Modelo Lasso

lasso<-train(fmla,
             data=train,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 1, #lasso
                                    lambda = seq(10000,1000000,by = 1000)),
             preProcess = c("center", "scale")
) 

plot(lasso$results$lambda,
     lasso$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE) Lasso"
)

lasso$bestTune

coef_lasso<-coef(lasso$finalModel, lasso$bestTune$lambda)
coef_lasso

modelo_lasso<-train(fmla,
                    data=train,
                    method = 'glmnet', 
                    trControl = fitControl,
                    tuneGrid = expand.grid(alpha = 1, #lasso
                                           lambda = 320000),
                    preProcess = c("center", "scale")
) 

y_hat_lasso <- predict(modelo_lasso, newdata = test)

## Elastic Net

EN<-train(fmla,
          data=train,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1), #grilla de alpha
                                 lambda = seq(100000,10000000,by = 10000)),
          preProcess = c("center", "scale")
) 

EN$bestTune

coef_EN<-coef(EN$finalModel,EN$bestTune$lambda)
coef_EN

modelo_EN<-train(fmla,
                 data=train,
                 method = 'glmnet', 
                 trControl = fitControl,
                 tuneGrid = expand.grid(alpha = 0.9, #grilla de alpha
                                        lambda = 320000),
                 preProcess = c("center", "scale")
) 

y_hat_EN <- predict(modelo_EN, newdata = test)

## Tabla: Coeficientes de los modelos

coefs_df<-cbind(coef(linear_reg$finalModel),as.matrix(coef_ridge),as.matrix(coef_lasso),as.matrix(coef_EN))
colnames(coefs_df)<-c("OLS","RIDGE","LASSO","ELASTIC_NET")
round(coefs_df,4)

RMSE_df<-cbind(linear_reg$results$RMSE,ridge$results$RMSE[which.min(ridge$results$lambda)],lasso$results$RMSE[which.min(lasso$results$lambda)],EN$results$RMSE[which.min(EN$results$lambda)])
colnames(RMSE_df)<-c("OLS","RIDGE","LASSO","EN")
RMSE_df

# Para enviar valores de prediccion

# y_hat_reg
# y_hat_ridge
# y_hat_lasso
# y_hat_EN

price <- y_hat_lasso
submission_template <- select(submission_template, -price)
submission_template <- cbind(submission_template, price)
names(submission_template)[ncol(submission_template)] <- "price"

p4 <- ggplot(submission_template, aes(x = price)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "precio en pesos ($)", y = "Cantidad",
       title = "Distribución del precio") +
  theme_bw()
p4

summary(submission_template$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))


# ------------- Exportando archivo ------------------------

write.csv(submission_template, file="submission_template.csv",  row.names = F)
submission_template1 <- read_csv("submission_template1.csv")

##-----------Superlearner--------##

bd<- train %>% mutate(logprice=log(price)) 

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
ySL <- ySL[1:1000]
XSL<- bdtrain_is  %>% select(surface_total,surface_covered,rooms,bedrooms,bathrooms,
                               distancia_parque,area_parque,distancia_bus,
                               distancia_avenida_principal,distancia_universidad,distancia_policia,
                               dist_jar,puntos_SC,dist_cole,property_type)
XSL <- XSL[1:1000,]
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





