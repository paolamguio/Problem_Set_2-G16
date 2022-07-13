
# Clasificación de modelos 
# Problem_Set_2 
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
#--------------------------------------------------
  
## preparación del espacio
rm(list = ls())
setwd("C:/Users/ocaco/OneDrive/15. Maestria Economia/9. Big Data/3. GitHub/Problem_Set_2-G16/3. Stores")

## llamado librerías de la sesión

require(pacman)
p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  pastecs,
  PerformanceAnalytics,
  naniar,
  gtsummary,
  caret,
  modelsummary,
  gamlr,
  ROCR,
  pROC,
  smotefamily,
  rpart,
  randomForest,
  fastAdaboost,
  stargazer
)

### 1. llamado base de datos creada en 2.data_cleaning2.R ###
df_hogares <- import("df_hogares.rds") 
 



### 2. Ajustes base de datos ###
df_hogares <- df_hogares %>% select(c("id", "Clase", "Dominio", "Ingtotugarr","Nper", "Lp", "Pobre", "tipo_vivienda",
                                      "Nro_personas_cuartos", "arriendo", "edad_promedio", "jefe_hogar_mujer",
                                      "Nro_hijos", "edu_promedio", "horas_trabajadas_promedio", "Ingtotob_hogar",
                                      "porcentaje_mujeres", "porcentaje_trabajo_formal",
                                      "porcentaje_subsidio_familiar", "segundo_trabajo", "otros_ingresos",
                                      "otros_ingresos_instituciones", "tasa_ocupacion", "tasa_desempleo",
                                      "tasa_inactivas", "tasa_participacion"))

summary(df_hogares)

#Creación de variable
df_hogares <- df_hogares %>% mutate(edad_promedio2 = edad_promedio^2)
df_hogares <- df_hogares %>%mutate(Ingtotugarr=Ingtotob_hogar)
df_hogares <- df_hogares %>%mutate(ingmenorLP=ifelse(Ingtotugarr/Nper < Lp, 1, 0))

df_hogares<-df_hogares %>% mutate(ingmenorLP=factor(ingmenorLP,levels=c(1,0),labels=c("Si","No")))

compare<- select(filter(df_hogares),c(Pobre, ingmenorLP)) 
table_compare <- summary(compare)  
table_compare 
summary(compare) 

prop.table(table(df_hogares$ingmenorLP))

### 3.Partición base de datos en tres ###
# base de datos de entrenamiento
set.seed(777)
split1 <- createDataPartition(df_hogares$ingmenorLP , p = 0.7)[[1]]
length(split1)
training = df_hogares[split1,]
testing <- df_hogares[-split1,]


dim(training)
dim(testing)
dim(evaluation)

prop.table(table(df_hogares$ingmenorLP))
prop.table(table(training$ingmenorLP))
prop.table(table(testing$ingmenorLP))


predict <- stats::predict

colnames(training)

### 4.se definen dos modelos con distintas variables de interes ###
model1 <- as.formula("Ingtotugarr ~ tipo_vivienda + Dominio + Nro_personas_cuartos +
                    arriendo + edad_promedio + edad_promedio2 + jefe_hogar_mujer + Nro_hijos + edu_promedio +
                    horas_trabajadas_promedio + porcentaje_mujeres + porcentaje_trabajo_formal +
                    porcentaje_subsidio_familiar + segundo_trabajo + otros_ingresos + otros_ingresos_instituciones +
                    tasa_ocupacion + tasa_desempleo + tasa_participacion")

#model2 <- as.formula("Ingtotugarr ~ tipo_vivienda + Dominio + Nro_personas_cuartos +
#                    arriendo + edad_promedio + edad_promedio2 + jefe_hogar_mujer + Nro_hijos + edu_promedio +
#                    porcentaje_mujeres + porcentaje_trabajo_formal +
#                    tasa_ocupacion")



#fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

### 5. Modelos de predicción ###

### 5.1 Modelo Lineal

set.seed(777)

#Controles y Evaluaciones

ctrl<- trainControl(method = "cv",
                    number = 5,
                    #summaryFunction = fiveStats,
                    #classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Medtricas del cumplimiento del model
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )}

## 5.1. Modelo Lineal - datos de entrenamiento ###

lineal1 <- train(
  model1,
  data = training,
  method = "lm",
  trControl = ctrl,
  preProcess = c("center", "scale")
)

#lineal2 <- train(model2,  data = training,  method = "lm",  trControl = ctrl_lineal,  preProcess = c("center", "scale"))

lineal1
#lineal2

predLineal1 <- predict(lineal1 , testing)
#predLineal2 <- predict(lineal2 , testing)


#eval_results(testing$Ingtotugarr,predLineal2,testing)

## Clasificacion
testing <- testing %>%mutate(linealLP1=ifelse(predLineal1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(linealLP1=factor(linealLP1,levels=c(1,0),labels=c("Si","No")))

#testing <- testing %>%mutate(linealLP2=ifelse(predLineal2/Nper < Lp, 1, 0))
#testing<-testing %>% mutate(linealLP2=factor(linealLP2,levels=c(1,0),labels=c("Si","No")))


#confusionMatrix(data=testing$linealLP2, reference=testing$Pobre , mode="sens_spec" , positive="Si")





## 5.2. Modelo lasso - datos de entrenamiento ###

lambda_grid <- 10^seq(-4, 0.01, length = 300)

set.seed(777)

lasso1 <- train(
  model1,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)


#lasso2 <- train(model2,data = training,method = "glmnet",trControl = ctrl,metric = "RMSE",tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),  preProcess = c("center", "scale"))

predLasso1<-predict(lasso1,testing)
#predLasso2<-predict(lasso2,testing)


#eval_results(testing$Ingtotugarr,predLasso2,testing)

## Clasificacion
testing <- testing %>%mutate(lassoLP1=ifelse(predLasso1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(lassoLP1=factor(lassoLP1,levels=c(1,0),labels=c("Si","No")))

#testing <- testing %>%mutate(lassoLP2=ifelse(predLasso2/Nper < Lp, 1, 0))
#testing<-testing %>% mutate(lassoLP2=factor(lassoLP2,levels=c(1,0),labels=c("Si","No")))



#confusionMatrix(data=testing$lassoLP2, reference=testing$Pobre , mode="sens_spec" , positive="Si")

## 5.3. Modelo ridge - datos de entrenamiento ###

ridge1 <- train(
  model1,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)


#ridge2 <- train(  model2,  data = training,  method = "glmnet",  trControl = ctrl,  metric = "RMSE",  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),  preProcess = c("center", "scale"))

predRidge1<-predict(ridge1,testing)
#predRidge2<-predict(ridge2,testing)


#eval_results(testing$Ingtotugarr,predRidge2,testing)

## Clasificacion
testing <- testing %>%mutate(ridgeLP1=ifelse(predRidge1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(ridgeLP1=factor(ridgeLP1,levels=c(1,0),labels=c("Si","No")))

#testing <- testing %>%mutate(ridgeLP2=ifelse(predRidge2/Nper < Lp, 1, 0))
#testing<-testing %>% mutate(ridgeLP2=factor(ridgeLP2,levels=c(1,0),labels=c("Si","No")))


#confusionMatrix(data=testing$ridgeLP2,reference=testing$Pobre ,mode="sens_spec" , positive="Si")


## 5.4. Modelo elasticnet - datos de entrenamiento ###

elasticnet1 <- train(
  model1,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  preProcess = c("center", "scale")
)


#elasticnet2 <- train( model2,  data = training,  method = "glmnet",  trControl = ctrl,  metric = "RMSE",  preProcess = c("center", "scale"))


predElasticnet1<-predict(elasticnet1,testing)
#predElasticnet2<-predict(elasticnet2,testing)


#eval_results(testing$Ingtotugarr,predElasticnet2,testing)

## Clasificacion
testing <- testing %>%mutate(elasticnetLP1=ifelse(predElasticnet1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(elasticnetLP1=factor(elasticnetLP1,levels=c(1,0),labels=c("Si","No")))

#testing <- testing %>%mutate(elasticnetLP2=ifelse(predElasticnet2/Nper < Lp, 1, 0))
#testing<-testing %>% mutate(elasticnetLP2=factor(elasticnetLP2,levels=c(1,0),labels=c("Si","No")))


#confusionMatrix(data=testing$elasticnetLP2, reference=testing$Pobre , mode="sens_spec" , positive="Si")

## 5.5. Modelo lasso up sample - datos de entrenamiento ###

set.seed(777)

upSampledTrain <- upSample(x = training,
                           y = training$ingmenorLP,
                           yname = "Pobre")

table(upSampledTrain$ingmenorLP)

#Lasso

lassoUpsample1 <- train(
  model1,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

#lassoUpsample2 <- train( model2, data = upSampledTrain,method = "glmnet",trControl = ctrl, metric = "RMSE",tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),  preProcess = c("center", "scale"))

predlassoUpsample1 <-predict(lassoUpsample1 ,testing)
#predlassoUpsample2 <-predict(lassoUpsample2 ,testing)


#eval_results(testing$Ingtotugarr,predlassoUpsample2 ,testing)

## Clasificacion
testing <- testing %>%mutate(lassoUpsampleLP1=ifelse(predlassoUpsample1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(lassoUpsampleLP1=factor(lassoUpsampleLP1,levels=c(1,0),labels=c("Si","No")))

#testing <- testing %>%mutate(lassoUpsampleLP2=ifelse(predlassoUpsample2/Nper < Lp, 1, 0))
#testing<-testing %>% mutate(lassoUpsampleLP2=factor(lassoUpsampleLP2,levels=c(1,0),labels=c("Si","No")))


#confusionMatrix(data=testing$lassoUpsampleLP2,reference=testing$Pobre,mode="sens_spec" , positive="Si")

#Ridge

ridgeUpsample1 <- train(
  model1,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)


#ridgeUpsample2 <- train(model2, data = upSampledTrain,method = "glmnet",trControl = ctrl, metric = "RMSE",tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid), preProcess = c("center", "scale"))

predridgeUpsample1 <-predict(ridgeUpsample1 ,testing)
#predridgeUpsample2 <-predict(ridgeUpsample2 ,testing)


#eval_results(testing$Ingtotugarr,predridgeUpsample2 ,testing)

## Clasificacion
testing <- testing %>%mutate(ridgeUpsampleLP1=ifelse(predridgeUpsample1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(ridgeUpsampleLP1=factor(ridgeUpsampleLP1,levels=c(1,0),labels=c("Si","No")))

#testing <- testing %>%mutate(ridgeUpsampleLP2=ifelse(predridgeUpsample2/Nper < Lp, 1, 0))
#testing<-testing %>% mutate(ridgeUpsampleLP2=factor(ridgeUpsampleLP2,levels=c(1,0),labels=c("Si","No")))

#confusionMatrix(data=testing$ridgeUpsampleLP2, reference=testing$Pobre , mode="sens_spec" , positive="Si")

elasticnetupsample1 <- train(
  model1,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  preProcess = c("center", "scale")
)

#elasticnetUpsample2 <- train(model2, data = upSampledTrain,method = "glmnet",trControl = ctrl, metric = "RMSE",preProcess = c("center", "scale"))

predelasticnetUpsample1 <-predict(elasticnetupsample1 ,testing)
#predelasticnetUpsample2 <-predict(elasticnetUpsample2 ,testing)


#eval_results(testing$Ingtotugarr,predelasticnetUpsample2 ,testing)

## Clasificacion
testing <- testing %>%mutate(elasticnetUpsampleLP1=ifelse(predelasticnetUpsample1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(elasticnetUpsampleLP1=factor(elasticnetUpsampleLP1,levels=c(1,0),labels=c("Si","No")))

#testing <- testing %>%mutate(elasticnetUpsampleLP2=ifelse(predelasticnetUpsample2/Nper < Lp, 1, 0))
#testing<-testing %>% mutate(elasticnetUpsampleLP2=factor(elasticnetUpsampleLP2,levels=c(1,0),labels=c("Si","No")))


#confusionMatrix(data=testing$elasticnetUpsampleLP2, reference=testing$Pobre , mode="sens_spec" , positive="Si")

## 5.6. Modelo lasso down sample - datos de entrenamiento ###

set.seed(777)


downSampledTrain <- downSample(x = training,
                               y = training$ingmenorLP,
                               yname = "Pobre")

table(downSampledTrain$Pobre)


lassoDownsample1 <- train(
  model1,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

lassoDownsample1 
predlassoDownsample1 <-predict(lassoDownsample1 ,testing)




## Clasificacion
testing <- testing %>%mutate(lassoDownsampleLP1=ifelse(predlassoDownsample1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(lassoDownsampleLP1=factor(lassoDownsampleLP1,levels=c(1,0),labels=c("Si","No")))


ridgeDownsample1 <- train(
  model1,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)



predridgeDownsample1 <-predict(ridgeDownsample1 ,testing)




## Clasificacion
testing <- testing %>%mutate(ridgeDownsampleLP1=ifelse(predridgeDownsample1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(ridgeDownsampleLP1=factor(ridgeDownsampleLP1,levels=c(1,0),labels=c("Si","No")))


elasticnetDownsample1 <- train(
  model1,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  preProcess = c("center", "scale")
)

elasticnetDownsample1 
predelasticnetDownsample1 <-predict(elasticnetDownsample1 ,testing)




## Clasificacion
testing <- testing %>%mutate(elasticnetDownsampleLP1=ifelse(predelasticnetDownsample1/Nper < Lp, 1, 0))
testing<-testing %>% mutate(elasticnetDownsampleLP1=factor(elasticnetDownsampleLP1,levels=c(1,0),labels=c("Si","No")))


### 6. Evaluación de resultados ###
tabla<-matrix(rep(0,40),nrow=10,ncol=4)
colnames(tabla)<- c("Modelo","RMSE","Sens","Spec")
#Lineal
tabla[1,1]<-"OLS"
eval<-eval_results(testing$Ingtotugarr,predLineal1,testing)
tabla[1,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$linealLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[1,3]<-matriz$byClass[1]
tabla[1,4]<-matriz$byClass[2]


#Lasso
tabla[2,1]<-"Lasso"
eval<-eval_results(testing$Ingtotugarr,predLasso1,testing)
tabla[2,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$lassoLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[2,3]<-matriz$byClass[1]
tabla[2,4]<-matriz$byClass[2]


#Ridge
tabla[3,1]<-"Ridge"
eval<-eval_results(testing$Ingtotugarr,predRidge1,testing)
tabla[3,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$ridgeLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[3,3]<-matriz$byClass[1]
tabla[3,4]<-matriz$byClass[2]

#Elasticnet
tabla[4,1]<-"Elasticnet"
eval<-eval_results(testing$Ingtotugarr,predElasticnet1,testing)
tabla[4,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$elasticnetLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[4,3]<-matriz$byClass[1]
tabla[4,4]<-matriz$byClass[2]


#Lasso Upsample
tabla[5,1]<-"Lasso_Upsample"
eval<-eval_results(testing$Ingtotugarr,predlassoUpsample1 ,testing)
tabla[5,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$lassoUpsampleLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[5,3]<-matriz$byClass[1]
tabla[5,4]<-matriz$byClass[2]


#Ridge Upsample
tabla[6,1]<-"Ridge_Upsample"
eval<-eval_results(testing$Ingtotugarr,predridgeUpsample1 ,testing)
tabla[6,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$ridgeUpsampleLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[6,3]<-matriz$byClass[1]
tabla[6,4]<-matriz$byClass[2]


#Elasticnet Upsample
tabla[7,1]<-"Elasticnet_Upsample"
eval<-eval_results(testing$Ingtotugarr,predelasticnetUpsample1 ,testing)
tabla[7,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$elasticnetUpsampleLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[7,3]<-matriz$byClass[1]
tabla[7,4]<-matriz$byClass[2]


#Lasso Downsample
tabla[8,1]<-"Lasso_Downsample"
eval<-eval_results(testing$Ingtotugarr,predlassoDownsample1 ,testing)
tabla[8,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$lassoDownsampleLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[8,3]<-matriz$byClass[1]
tabla[8,4]<-matriz$byClass[2]


#Ridge Downsample
tabla[9,1]<-"Ridge_Downsample"
eval<-eval_results(testing$Ingtotugarr,predridgeDownsample1 ,testing)
tabla[9,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$ridgeDownsampleLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[9,3]<-matriz$byClass[1]
tabla[9,4]<-matriz$byClass[2]


#Elasticnet Downsample
tabla[10,1]<-"Elasticnet_Downsample"
eval<-eval_results(testing$Ingtotugarr,predelasticnetDownsample1 ,testing)
tabla[10,2]<-eval$RMSE
matriz<-confusionMatrix(data=testing$elasticnetDownsampleLP1, 
                        reference=testing$Pobre , 
                        mode="sens_spec" , positive="Si")
tabla[10,3]<-matriz$byClass[1]
tabla[10,4]<-matriz$byClass[2]

dtabla<-as.data.frame(tabla)
stargazer(tabla, type = "text")
write_xlsx(dtabla,"tabla_ingreso.xlsx")

#dibujar los modelos
model_list = list(modLin=lineal1,
                  modLasso=lasso1,
                  modRidge=ridge1,
                  modElast=elasticnet1,
                  modLassoUp=lassoUpsample1,
                  modRidgeUp=ridgeUpsample1,
                  modElastUp=elasticnetupsample1,
                  modLassoDown=lassoDownsample1,
                  modRidgeDown=ridgeDownsample1,
                  modElastDown=elasticnetDownsample1)
resamples <- resamples(model_list)
dotplot(resamples,metric = "RMSE")


##=== 7. submit ===##

## data test
data_submit <- import("df_test_hogares.rds")

df_coeficientes <- coef(ridgeDownsample1$finalModel,ridgeDownsample1$finalModel$lambdaOpt) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s1)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Elasticnet Upsample") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

write_xlsx(df_coeficientes,"coef_modeloIngreso.xlsx")
## prediction
data_submit$prediction <- predict(ridgeDownsample1, data_submit)

data_submit = data_submit %>% mutate(ingresoLP_model=ifelse(prediction/Nper < Lp, 1, 0))

submit = data_submit %>% select(id,ingresoLP_model)

prop.table(table(submit$ingresoLP_model))

## export results
saveRDS(submit, file = "ingreso_model_submit.rds")
