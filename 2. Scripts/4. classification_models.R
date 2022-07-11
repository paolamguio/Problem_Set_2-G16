
# Clasificación de modelos 
# Problem_Set_2 
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_2-G16/3. Stores")

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
  glmnet,
  rpart.plot
)

### 1. llamado base de datos creada en 2.data_cleaning2.R ###
df_hogares <- import("df_hogares.rds") 

### 2. Ajustes base de datos ###
df_hogares <- df_hogares %>% select(c("id", "Dominio", "Nper", "Lp", "Pobre", "tipo_vivienda", "Nro_cuartos", "Nro_personas_cuartos", "cuota_amortizacion", "arriendo", "Nro_mujeres", "edad_promedio", "jefe_hogar_mujer", "Nro_hijos", "edu_promedio", "horas_trabajadas_promedio", "Ingtotob_hogar", "porcentaje_mujeres", "porcentaje_trabajo_formal", "porcentaje_subsidio_familiar", "segundo_trabajo", "otros_ingresos", "otros_ingresos_instituciones", "tasa_ocupacion", "tasa_desempleo", "tasa_inactivas", "tasa_participacion"))
summary(df_hogares)

#Creación de variable
df_hogares <- df_hogares %>% mutate(edad_promedio2 = edad_promedio^2)

prop.table(table(df_hogares$Pobre))

### 3.Partición base de datos en tres ###
# base de datos de entrenamiento
set.seed(156)
split1 <- createDataPartition(df_hogares$Pobre , p = 0.7)[[1]]
length(split1)
training = df_hogares[split1,]
other <- df_hogares[-split1,]

# Creación de bases de evaluación y testeo 
set.seed(934)
split2 <- createDataPartition(other$Pobre , p = 1/3)[[1]]
evaluation <- other[split2,]
testing <- other[-split2,]

dim(training)
dim(testing)
dim(evaluation)

prop.table(table(training$Pobre))
prop.table(table(testing$Pobre))
prop.table(table(evaluation$Pobre))

predict <- stats::predict

colnames(training)

### 4.se definen dos modelos con distintas variables de interes ###
model <- as.formula("Pobre ~ tipo_vivienda + Dominio + Nro_personas_cuartos + cuota_amortizacion + arriendo + edad_promedio + jefe_hogar_mujer + Nro_hijos + edu_promedio + horas_trabajadas_promedio + porcentaje_mujeres + porcentaje_trabajo_formal + porcentaje_subsidio_familiar + segundo_trabajo + otros_ingresos + otros_ingresos_instituciones + tasa_ocupacion + tasa_desempleo + tasa_participacion")
model2 <- as.formula("Pobre ~ tipo_vivienda + Dominio + Nro_personas_cuartos + cuota_amortizacion + arriendo + edad_promedio + edad_promedio:porcentaje_mujeres + jefe_hogar_mujer + Nro_hijos + Nro_hijos:porcentaje_mujeres + edu_promedio + edu_promedio:porcentaje_mujeres + edu_promedio:Dominio + edu_promedio:jefe_hogar_mujer + horas_trabajadas_promedio + horas_trabajadas_promedio:porcentaje_mujeres + horas_trabajadas_promedio:jefe_hogar_mujer + porcentaje_mujeres + porcentaje_trabajo_formal + porcentaje_subsidio_familiar + segundo_trabajo + otros_ingresos + otros_ingresos_instituciones + tasa_ocupacion + tasa_desempleo + tasa_participacion")

trainX<-data.frame(model.matrix(model,data=training), Pobre = training$Pobre)[-1]
trainX2<-data.frame(model.matrix(model2,data=training), Pobre = training$Pobre)[-1]

evalX<-data.frame(model.matrix(model,data=evaluation), Pobre = evaluation$Pobre)[-1]
evalX2<-data.frame(model.matrix(model2,data=evaluation), Pobre = evaluation$Pobre)[-1]

testX<-data.frame(model.matrix(model,data=testing), Pobre = testing$Pobre)[-1]
testX2<-data.frame(model.matrix(model2,data=testing), Pobre = testing$Pobre)[-1]

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

### 4. Modelos de predicción ###

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)


## 4.1. Modelo Logit - datos de entrenamiento ###

set.seed(1410)
logit <- train(
  model,
  data = training,
  method = "glm",
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)

logit

logit2 <- train(
  model2,
  data = training,
  method = "glm",
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)

logit2

## 4.2. Modelo lasso - datos de entrenamiento ###

lambda_grid <- 10^seq(-4, 0.01, length = 100)

set.seed(1410)

logit_lasso <- train(
  model,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_lasso

logit_lasso2 <- train(
  model2,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_lasso2

## 4.3. Modelo logit ridge - datos de entrenamiento ###

logit_ridge <- train(
  model,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_ridge

logit_ridge2 <- train(
  model2,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_ridge2

## 4.4. Modelo logit EN - datos de entrenamiento ###

logit_elasticnet <- train(
  model,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

logit_elasticnet

logit_elasticnet2 <- train(
  model2,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

logit_elasticnet2

## 4.5. Modelo logit lasso up sample - datos de entrenamiento ###

set.seed(1410)

upSampledTrain <- upSample(x = training,
                           y = training$Pobre,
                           yname = "Pobre")

table(upSampledTrain$Pobre) # se balancean hacia arriba

set.seed(1410)

logit_lasso_upsample <- train(
  model,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_lasso_upsample

logit_lasso_upsample2 <- train(
  model2,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_lasso_upsample2

## 4.6. Modelo logit ridge up sample - datos de entrenamiento ###

logit_ridge_upsample <- train(
  model,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_ridge_upsample

logit_ridge_upsample2 <- train(
  model2,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_ridge_upsample2

## 4.7. Modelo logit EN up sample - datos de entrenamiento ###

logit_elasticnet_upsample <- train(
  model,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

logit_elasticnet_upsample

logit_elasticnet_upsample2 <- train(
  model2,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

logit_elasticnet_upsample2

## 4.8. Modelo logit lasso down sample - datos de entrenamiento ###

set.seed(1410)

downSampledTrain <- downSample(x = training,
                               y = training$Pobre,
                               yname = "Pobre")

table(downSampledTrain$Pobre)

set.seed(1410)

logit_lasso_downsample <- train(
  model,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_lasso_downsample

logit_lasso_downsample2 <- train(
  model2,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_lasso_downsample2

## 4.9. Modelo logit ridge down sample - datos de entrenamiento ###

logit_ridge_downsample <- train(
  model,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_ridge_downsample

logit_ridge_downsample2 <- train(
  model2,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_ridge_downsample2

## 4.10. Modelo logit EN down sample - datos de entrenamiento ###

logit_elasticnet_downsample <- train(
  model,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

logit_elasticnet_downsample

logit_elasticnet_downsample2 <- train(
  model2,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

logit_elasticnet_downsample2

## 4.11. Modelo SMOTE ###

trainX<-data.frame(model.matrix(model,data=training))[-1]
trainX2<-data.frame(model.matrix(model2,data=training))[-1]

smote_output = SMOTE(X = trainX,
                     target = training$Pobre)
smote_output2 = SMOTE(X = trainX2,
                     target = training$Pobre)

oversampled_data <- smote_output$data
oversampled_data2 <- smote_output2$data

table(oversampled_data$class)
table(oversampled_data2$class)

set.seed(1410)

logit_lasso_smote <- train(
  class~.,
  data = oversampled_data,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_lasso_smote

logit_lasso_smote2 <- train(
  class~.,
  data = oversampled_data2,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_lasso_smote2

logit_ridge_smote <- train(
  class~.,
  data = oversampled_data,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_ridge_smote

logit_ridge_smote2 <- train(
  class~.,
  data = oversampled_data2,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

logit_ridge_smote2

logit_elasticnet_smote <- train(
  class~.,
  data = oversampled_data,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

logit_elasticnet_smote

logit_elasticnet_smote2 <- train(
  class~.,
  data = oversampled_data2,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

logit_elasticnet_smote2

### 5. Evaluación de resultados ###

evalResults <- data.frame(Pobre = evaluation$Pobre)

evalResults$Roc_logit <- predict(logit,
                           newdata = evaluation,
                           type = "prob")[,1]

evalResults$Roc_logit2 <- predict(logit2,
                                 newdata = evaluation,
                                 type = "prob")[,1]

evalResults$Roc_logit_lasso <- predict(logit_lasso,
                                 newdata = evaluation,
                                 type = "prob")[,1]

evalResults$Roc_logit_lasso2 <- predict(logit_lasso2,
                                       newdata = evaluation,
                                       type = "prob")[,1]

evalResults$Roc_logit_ridge <- predict(logit_ridge,
                                       newdata = evaluation,
                                       type = "prob")[,1]

evalResults$Roc_logit_ridge2 <- predict(logit_ridge2,
                                       newdata = evaluation,
                                       type = "prob")[,1]

evalResults$Roc_logit_elasticnet <- predict(logit_elasticnet,
                                       newdata = evaluation,
                                       type = "prob")[,1]

evalResults$Roc_logit_elasticnet2 <- predict(logit_elasticnet2,
                                            newdata = evaluation,
                                            type = "prob")[,1]

evalResults$Roc_logit_lasso_upsample <- predict(logit_lasso_upsample,
                                       newdata = evaluation,
                                       type = "prob")[,1]

evalResults$Roc_logit_lasso_upsample2 <- predict(logit_lasso_upsample2,
                                                newdata = evaluation,
                                                type = "prob")[,1]

evalResults$Roc_logit_ridge_upsample <- predict(logit_ridge_upsample,
                                                newdata = evaluation,
                                                type = "prob")[,1]

evalResults$Roc_logit_ridge_upsample2 <- predict(logit_ridge_upsample2,
                                                newdata = evaluation,
                                                type = "prob")[,1]

evalResults$Roc_logit_elasticnet_upsample <- predict(logit_elasticnet_upsample,
                                                newdata = evaluation,
                                                type = "prob")[,1]

evalResults$Roc_logit_elasticnet_upsample2 <- predict(logit_elasticnet_upsample2,
                                                     newdata = evaluation,
                                                     type = "prob")[,1]

evalResults$Roc_logit_lasso_downsample <- predict(logit_lasso_downsample,
                                                newdata = evaluation,
                                                type = "prob")[,1]

evalResults$Roc_logit_lasso_downsample2 <- predict(logit_lasso_downsample2,
                                                  newdata = evaluation,
                                                  type = "prob")[,1]

evalResults$Roc_logit_ridge_downsample <- predict(logit_ridge_downsample,
                                                  newdata = evaluation,
                                                  type = "prob")[,1]

evalResults$Roc_logit_ridge_downsample2 <- predict(logit_ridge_downsample2,
                                                  newdata = evaluation,
                                                  type = "prob")[,1]

evalResults$Roc_logit_elasticnet_downsample <- predict(logit_elasticnet_downsample,
                                                  newdata = evaluation,
                                                  type = "prob")[,1]

evalResults$Roc_logit_elasticnet_downsample2 <- predict(logit_elasticnet_downsample2,
                                                       newdata = evaluation,
                                                       type = "prob")[,1]

evalResults$Roc_logit_laso_smote <- predict(logit_lasso_smote,
                                                  newdata = evalX,
                                                  type = "prob")[,1]

evalResults$Roc_logit_laso_smote2 <- predict(logit_lasso_smote2,
                                            newdata = evalX2,
                                            type = "prob")[,1]

evalResults$Roc_logit_ridge_smote <- predict(logit_ridge_smote,
                                            newdata = evalX,
                                            type = "prob")[,1]

evalResults$Roc_logit_ridge_smote2 <- predict(logit_ridge_smote2,
                                             newdata = evalX2,
                                             type = "prob")[,1]

evalResults$Roc_logit_elasticnet_smote <- predict(logit_elasticnet_smote,
                                             newdata = evalX,
                                             type = "prob")[,1]

evalResults$Roc_logit_elasticnet_smote2 <- predict(logit_elasticnet_smote2,
                                                  newdata = evalX2,
                                                  type = "prob")[,1]

# Cálculo de c

rfROC_logit <- roc(evalResults$Pobre, evalResults$Roc_logit, levels = rev(levels(evalResults$Pobre)))
rfROC_logit

rfROC_logit2 <- roc(evalResults$Pobre, evalResults$Roc_logit2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_lasso <- roc(evalResults$Pobre, evalResults$Roc_logit_lasso, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_lasso2 <- roc(evalResults$Pobre, evalResults$Roc_logit_lasso2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_ridge <- roc(evalResults$Pobre, evalResults$Roc_logit_ridge, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_ridge2 <- roc(evalResults$Pobre, evalResults$Roc_logit_ridge2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_elasticnet <- roc(evalResults$Pobre, evalResults$Roc_logit_elasticnet, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_elasticnet2 <- roc(evalResults$Pobre, evalResults$Roc_logit_elasticnet2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_lasso_upsample <- roc(evalResults$Pobre, evalResults$Roc_logit_lasso_upsample, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_lasso_upsample2 <- roc(evalResults$Pobre, evalResults$Roc_logit_lasso_upsample2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_ridge_upsample <- roc(evalResults$Pobre, evalResults$Roc_logit_ridge_upsample, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_ridge_upsample2 <- roc(evalResults$Pobre, evalResults$Roc_logit_ridge_upsample2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_elasticnet_upsample <- roc(evalResults$Pobre, evalResults$Roc_logit_elasticnet_upsample, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_elasticnet_upsample2 <- roc(evalResults$Pobre, evalResults$Roc_logit_elasticnet_upsample2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_lasso_downsample <- roc(evalResults$Pobre, evalResults$Roc_logit_lasso_downsample, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_lasso_downsample2 <- roc(evalResults$Pobre, evalResults$Roc_logit_lasso_downsample2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_ridge_downsample <- roc(evalResults$Pobre, evalResults$Roc_logit_ridge_downsample, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_ridge_downsample2 <- roc(evalResults$Pobre, evalResults$Roc_logit_ridge_downsample2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_elasticnet_downsample <- roc(evalResults$Pobre, evalResults_Roc_logit_elasticnet_downsample, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_elasticnet_downsample2 <- roc(evalResults$Pobre, evalResults_Roc_logit_elasticnet_downsample2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_lasso_smote <- roc(evalResults$Pobre, evalResults$Roc_logit_lasso_smote, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_lasso_smote2 <- roc(evalResults$Pobre, evalResults$Roc_logit_lasso_smote2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_ridge_smote <- roc(evalResults$Pobre, evalResults$Roc_logit_ridge_smote, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_ridge_smote2 <- roc(evalResults$Pobre, evalResults$Roc_logit_ridge_smote2, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_elasticnet_smote <- roc(evalResults$Pobre, evalResults$Roc_logit_elasticnet_smote, levels = rev(levels(evalResults$Pobre)))

rfROC_logit_elasticnet_smote2 <- roc(evalResults$Pobre, evalResults$Roc_logit_elasticnet_smote2, levels = rev(levels(evalResults$Pobre)))

# cálculo de c más cercano (threshold)

rfThresh_logit <- coords(rfROC_logit, x = "best", best.method = "closest.topleft")
rfThresh_logit

rfThresh_logit2 <- coords(rfROC_logit2, x = "best", best.method = "closest.topleft")
rfThresh_logit2

rfThresh_logit_lasso <- coords(rfROC_logit_lasso, x = "best", best.method = "closest.topleft")
rfThresh_logit_lasso

rfThresh_logit_lasso2 <- coords(rfROC_logit_lasso2, x = "best", best.method = "closest.topleft")
rfThresh_logit_lasso2

rfThresh_logit_ridge <- coords(rfROC_logit_ridge, x = "best", best.method = "closest.topleft")
rfThresh_logit_ridge

rfThresh_logit_ridge2 <- coords(rfROC_logit_ridge2, x = "best", best.method = "closest.topleft")
rfThresh_logit_ridge2

rfThresh_logit_elasticnet <- coords(rfROC_logit_elasticnet, x = "best", best.method = "closest.topleft")
rfThresh_logit_elasticnet

rfThresh_logit_elasticnet2 <- coords(rfROC_logit_elasticnet2, x = "best", best.method = "closest.topleft")
rfThresh_logit_elasticnet2

rfThresh_logit_lasso_upsample <- coords(rfROC_logit_lasso_upsample, x = "best", best.method = "closest.topleft")
rfThresh_logit_lasso_upsample

rfThresh_logit_lasso_upsample2 <- coords(rfROC_logit_lasso_upsample2, x = "best", best.method = "closest.topleft")
rfThresh_logit_lasso_upsample2

rfThresh_logit_ridge_upsample <- coords(rfROC_logit_ridge_upsample, x = "best", best.method = "closest.topleft")
rfThresh_logit_ridge_upsample

rfThresh_logit_ridge_upsample2 <- coords(rfROC_logit_ridge_upsample2, x = "best", best.method = "closest.topleft")
rfThresh_logit_ridge_upsample2

rfThresh_logit_elasticnet_upsample <- coords(rfROC_logit_elasticnet_upsample, x = "best", best.method = "closest.topleft")
rfThresh_logit_elasticnet_upsample

rfThresh_logit_elasticnet_upsample2 <- coords(rfROC_logit_elasticnet_upsample2, x = "best", best.method = "closest.topleft")
rfThresh_logit_elasticnet_upsample2

rfThresh_logit_lasso_downsample <- coords(rfROC_logit_lasso_downsample, x = "best", best.method = "closest.topleft")
rfThresh_logit_lasso_downsample

rfThresh_logit_lasso_downsample2 <- coords(rfROC_logit_lasso_downsample2, x = "best", best.method = "closest.topleft")
rfThresh_logit_lasso_downsample2

rfThresh_logit_ridge_downsample <- coords(rfROC_logit_ridge_downsample, x = "best", best.method = "closest.topleft")
rfThresh_logit_ridge_downsample

rfThresh_logit_ridge_downsample2 <- coords(rfROC_logit_ridge_downsample2, x = "best", best.method = "closest.topleft")
rfThresh_logit_ridge_downsample2

rfThresh_logit_elasticnet_downsample <- coords(rfROC_logit_elasticnet_downsample, x = "best", best.method = "closest.topleft")
rfThresh_logit_elasticnet_downsample

rfThresh_logit_elasticnet_downsample2 <- coords(rfROC_logit_elasticnet_downsample2, x = "best", best.method = "closest.topleft")
rfThresh_logit_elasticnet_downsample2

rfThresh_logit_lasso_smote <- coords(rfROC_logit_lasso_smote, x = "best", best.method = "closest.topleft")
rfThresh_logit_lasso_smote

rfThresh_logit_lasso_smote2 <- coords(rfROC_logit_lasso_smote2, x = "best", best.method = "closest.topleft")
rfThresh_logit_lasso_smote2

rfThresh_logit_ridge_smote <- coords(rfROC_logit_ridge_smote, x = "best", best.method = "closest.topleft")
rfThresh_logit_ridge_smote

rfThresh_logit_ridge_smote2 <- coords(rfROC_logit_ridge_smote2, x = "best", best.method = "closest.topleft")
rfThresh_logit_ridge_smote2

rfThresh_logit_elasticnet_smote <- coords(rfROC_logit_elasticnet_smote, x = "best", best.method = "closest.topleft")
rfThresh_logit_elasticnet_smote

rfThresh_logit_elasticnet_smote2 <- coords(rfROC_logit_elasticnet_smote2, x = "best", best.method = "closest.topleft")
rfThresh_logit_elasticnet_smote2

evalResults<-evalResults %>% mutate(hat_pobre_05_logit=ifelse(evalResults$Roc_logit>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit=ifelse(evalResults$Roc_logit>rfThresh_logit$threshold,"Si","No"),
                                    hat_pobre_05_logit2=ifelse(evalResults$Roc_logit2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit2=ifelse(evalResults$Roc_logit2>rfThresh_logit2$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso=ifelse(evalResults$Roc_logit_lasso>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso=ifelse(evalResults$Roc_logit_lasso>rfThresh_logit_lasso$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso2=ifelse(evalResults$Roc_logit_lasso2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso2=ifelse(evalResults$Roc_logit_lasso2>rfThresh_logit_lasso2$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge=ifelse(evalResults$Roc_logit_ridge>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge=ifelse(evalResults$Roc_logit_ridge>rfThresh_logit_ridge$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge2=ifelse(evalResults$Roc_logit_ridge2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge2=ifelse(evalResults$Roc_logit_ridge2>rfThresh_logit_ridge2$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet=ifelse(evalResults$Roc_logit_elasticnet>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet=ifelse(evalResults$Roc_logit_elasticnet>rfThresh_logit_elasticnet$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet2=ifelse(evalResults$Roc_logit_elasticnet2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet2=ifelse(evalResults$Roc_logit_elasticnet2>rfThresh_logit_elasticnet2$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_upsample=ifelse(evalResults$Roc_logit_lasso_upsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_upsample=ifelse(evalResults$Roc_logit_lasso_upsample>rfThresh_logit_lasso_upsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_upsample2=ifelse(evalResults$Roc_logit_lasso_upsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_upsample2=ifelse(evalResults$Roc_logit_lasso_upsample2>rfThresh_logit_lasso_upsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge_upsample=ifelse(evalResults$Roc_logit_ridge_upsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge_upsample=ifelse(evalResults$Roc_logit_ridge_upsample>rfThresh_logit_ridge_upsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge_upsample2=ifelse(evalResults$Roc_logit_ridge_upsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge_upsample2=ifelse(evalResults$Roc_logit_ridge_upsample2>rfThresh_logit_ridge_upsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet_upsample=ifelse(evalResults$Roc_logit_elasticnet_upsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet_upsample=ifelse(evalResults$Roc_logit_elasticnet_upsample>rfThresh_logit_elasticnet_upsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet_upsample2=ifelse(evalResults$Roc_logit_elasticnet_upsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet_upsample2=ifelse(evalResults$Roc_logit_elasticnet_upsample2>rfThresh_logit_elasticnet_upsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_downsample=ifelse(evalResults$Roc_logit_lasso_downsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_downsample=ifelse(evalResults$Roc_logit_lasso_downsample>rfThresh_logit_lasso_downsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_downsample2=ifelse(evalResults$Roc_logit_lasso_downsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_downsample2=ifelse(evalResults$Roc_logit_lasso_downsample2>rfThresh_logit_lasso_downsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge_downsample=ifelse(evalResults$Roc_logit_ridge_downsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge_downsample=ifelse(evalResults$Roc_logit_ridge_downsample>rfThresh_logit_ridge_downsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge_downsample2=ifelse(evalResults$Roc_logit_ridge_downsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge_downsample2=ifelse(evalResults$Roc_logit_ridge_downsample2>rfThresh_logit_ridge_downsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet_downsample=ifelse(evalResults$Roc_logit_elasticnet_downsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet_downsample=ifelse(evalResults$Roc_logit_elasticnet_downsample>rfThresh_logit_elasticnet_downsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet_downsample2=ifelse(evalResults$Roc_logit_elasticnet_downsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet_downsample2=ifelse(evalResults$Roc_logit_elasticnet_downsample2>rfThresh_logit_elasticnet_downsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_smote=ifelse(evalResults$Roc_logit_lasso_smote>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_smote=ifelse(evalResults$Roc_logit_lasso_smote>rfThresh_logit_lasso_smote$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_smote2=ifelse(evalResults$Roc_logit_lasso_smote2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_smote2=ifelse(evalResults$Roc_logit_lasso_smote2>rfThresh_logit_lasso_smote2$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge_smote=ifelse(evalResults$Roc_logit_ridge_smote>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge_smote=ifelse(evalResults$Roc_logit_ridge_smote>rfThresh_logit_ridge_smote$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge_smote2=ifelse(evalResults$Roc_logit_ridge_smote2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge_smote2=ifelse(evalResults$Roc_logit_ridge_smote2>rfThresh_logit_ridge_smote2$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet_smote=ifelse(evalResults$Roc_logit_elasticnet_smote>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet_smote=ifelse(evalResults$Roc_logit_elasticnet_smote>rfThresh_logit_elasticnet_smote$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet_smote2=ifelse(evalResults$Roc_logit_elasticnet_smote2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet_smote2=ifelse(evalResults$Roc_logit_elasticnet_smote2>rfThresh_logit_elasticnet_smote2$threshold,"Si","No"))


# Paola, si corre pero lo que sigue no 
evalResults<-evalResults %>% mutate(hat_pobre_05_logit=ifelse(evalResults$Roc_logit>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit=ifelse(evalResults$Roc_logit>rfThresh_logit$threshold,"Si","No"),
                                    hat_pobre_05_logit2=ifelse(evalResults$Roc_logit2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit2=ifelse(evalResults$Roc_logit2>rfThresh_logit2$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso=ifelse(evalResults$Roc_logit_lasso>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso=ifelse(evalResults$Roc_logit_lasso>rfThresh_logit_lasso$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso2=ifelse(evalResults$Roc_logit_lasso2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso2=ifelse(evalResults$Roc_logit_lasso2>rfThresh_logit_lasso2$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet=ifelse(evalResults$Roc_logit_elasticnet>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet=ifelse(evalResults$Roc_logit_elasticnet>rfThresh_logit_elasticnet$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet2=ifelse(evalResults$Roc_logit_elasticnet2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet2=ifelse(evalResults$Roc_logit_elasticnet2>rfThresh_logit_elasticnet2$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_upsample=ifelse(evalResults$Roc_logit_lasso_upsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_upsample=ifelse(evalResults$Roc_logit_lasso_upsample>rfThresh_logit_lasso_upsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_upsample2=ifelse(evalResults$Roc_logit_lasso_upsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_upsample2=ifelse(evalResults$Roc_logit_lasso_upsample2>rfThresh_logit_lasso_upsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet_upsample=ifelse(evalResults$Roc_logit_elasticnet_upsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet_upsample=ifelse(evalResults$Roc_logit_elasticnet_upsample>rfThresh_logit_elasticnet_upsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_elasticnet_upsample2=ifelse(evalResults$Roc_logit_elasticnet_upsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_elasticnet_upsample2=ifelse(evalResults$Roc_logit_elasticnet_upsample2>rfThresh_logit_elasticnet_upsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_downsample=ifelse(evalResults$Roc_logit_lasso_downsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_downsample=ifelse(evalResults$Roc_logit_lasso_downsample>rfThresh_logit_lasso_downsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_lasso_downsample2=ifelse(evalResults$Roc_logit_lasso_downsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_lasso_downsample2=ifelse(evalResults$Roc_logit_lasso_downsample2>rfThresh_logit_lasso_downsample2$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge_downsample=ifelse(evalResults$Roc_logit_ridge_downsample>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge_downsample=ifelse(evalResults$Roc_logit_ridge_downsample>rfThresh_logit_ridge_downsample$threshold,"Si","No"),
                                    hat_pobre_05_logit_ridge_downsample2=ifelse(evalResults$Roc_logit_ridge_downsample2>0.5,"Si","No"),
                                    hat_pobre_rfThresh_logit_ridge_downsample2=ifelse(evalResults$Roc_logit_ridge_downsample2>rfThresh_logit_ridge_downsample2$threshold,"Si","No"))

with(evalResults, table(Pobre, hat_pobre_05_logit))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit))

with(evalResults,table(Pobre,hat_pobre_05_logit2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit2))

with(evalResults,table(Pobre,hat_pobre_05_logit_lasso))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_lasso))

with(evalResults,table(Pobre,hat_pobre_05_logit_lasso2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_lasso2))

with(evalResults,table(Pobre,hat_pobre_05_logit_ridge))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_ridge))

with(evalResults,table(Pobre,hat_pobre_05_logit_ridge2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_ridge2))

with(evalResults,table(Pobre,hat_pobre_05_logit_elasticnet))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_elasticnet))

with(evalResults,table(Pobre,hat_pobre_05_logit_elasticnet2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_elasticnet2))

with(evalResults,table(Pobre,hat_pobre_05_logit_lasso_upsample))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_lasso_upsample))

with(evalResults,table(Pobre,hat_pobre_05_logit_lasso_upsample2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_lasso_upsample2))

with(evalResults,table(Pobre,hat_pobre_05_logit_ridge_upsample))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_ridge_upsample))

with(evalResults,table(Pobre,hat_pobre_05_logit_ridge_upsample2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_ridge_upsample2))

with(evalResults,table(Pobre,hat_pobre_05_logit_elasticnet_upsample))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_elasticnet_upsample))

with(evalResults,table(Pobre,hat_pobre_05_logit_elasticnet_upsample2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_elasticnet_upsample2))

with(evalResults,table(Pobre,hat_pobre_05_logit_lasso_downsample))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_lasso_downsample))

with(evalResults,table(Pobre,hat_pobre_05_logit_lasso_downsample2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_lasso_downsample2))

with(evalResults,table(Pobre,hat_pobre_05_logit_ridge_downsample))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_ridge_downsample))

with(evalResults,table(Pobre,hat_pobre_05_logit_ridge_downsample2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_ridge_downsample2))

with(evalResults,table(Pobre,hat_pobre_05_logit_elasticnet_downsample))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_elasticnet_downsample))

with(evalResults,table(Pobre,hat_pobre_05_logit_elasticnet_downsample2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_elasticnet_downsample2))

with(evalResults,table(Pobre,hat_pobre_05_logit_lasso_smote))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_lasso_smote))

with(evalResults,table(Pobre,hat_pobre_05_logit_lasso_smote2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_lasso_smote2))

with(evalResults,table(Pobre,hat_pobre_05_logit_ridge_smote))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_ridge_smote))

with(evalResults,table(Pobre,hat_pobre_05_logit_ridge_smote2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_ridge_smote2))

with(evalResults,table(Pobre,hat_pobre_05_logit_elasticnet_smote))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_elasticnet_smote))

with(evalResults,table(Pobre,hat_pobre_05_logit_elasticnet_smote2))

with(evalResults,table(Pobre,hat_pobre_rfThresh_logit_elasticnet_smote2))

### Modelo de árbol para clasificación ###
set.seed(1410)

tree <- train(
  x = trainX,
  y = training$Pobre,
  method = "rpart",
  trControl = ctrl,
  parms=list(split='Gini'),
  tuneLength=300
)

tree

tree2 <- train(
  x = trainX2,
  y = training$Pobre,
  method = "rpart",
  trControl = ctrl,
  parms=list(split='Gini'),
  tuneLength=300
)

tree2

rpart.plot::prp(tree$finalModel)

rpart.plot::prp(tree2$finalModel)

set.seed(1410)

forest <- train(
  model,
  data = training,
  method = "rf",
  trControl = ctrl,
  family = "binomial",
  metric="Sens"
)

forest

forest2 <- train(
  model2,
  data = training,
  method = "rf",
  trControl = ctrl,
  family = "binomial",
  metric="Sens"
)

forest2

varImp(forest,scale=TRUE)

varImp(forest2,scale=TRUE)

set.seed(1410)

adaboost <- train(
  model,
  data = upSampledTrain,
  method = "adaboost",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens"
)

adaboost

adaboost2 <- train(
  model2,
  data = upSampledTrain,
  method = "adaboost",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens"
)

adaboost2

testResults <- data.frame(Pobre = testing$Pobre)

testResults$logit<- predict(logit,
                            newdata = testing,
                            type = "prob")[,1]

testResults$logit2<- predict(logit2,
                            newdata = testing,
                            type = "prob")[,1]

testResults$logit_lasso<- predict(logit_lasso,
                            newdata = testing,
                            type = "prob")[,1]

testResults$logit_lasso2<- predict(logit_lasso2,
                                  newdata = testing,
                                  type = "prob")[,1]

testResults$logit_ridge<- predict(logit_ridge,
                                  newdata = testing,
                                  type = "prob")[,1]

testResults$logit_ridge2<- predict(logit_ridge2,
                                  newdata = testing,
                                  type = "prob")[,1]

testResults$logit_elasticnet<- predict(logit_elasticnet,
                                  newdata = testing,
                                  type = "prob")[,1]

testResults$logit_elasticnet2<- predict(logit_elasticnet2,
                                       newdata = testing,
                                       type = "prob")[,1]

testResults$logit_lasso_upsample<- predict(logit_lasso_upsample,
                                       newdata = testing,
                                       type = "prob")[,1]

testResults$logit_lasso_upsample2<- predict(logit_lasso_upsample2,
                                           newdata = testing,
                                           type = "prob")[,1]

testResults$logit_ridge_upsample<- predict(logit_ridge_upsample,
                                           newdata = testing,
                                           type = "prob")[,1]

testResults$logit_ridge_upsample2<- predict(logit_ridge_upsample2,
                                           newdata = testing,
                                           type = "prob")[,1]

testResults$logit_elasticnet_upsample<- predict(logit_elasticnet_upsample,
                                           newdata = testing,
                                           type = "prob")[,1]

testResults$logit_elasticnet_upsample2<- predict(logit_elasticnet_upsample2,
                                                newdata = testing,
                                                type = "prob")[,1]

testResults$logit_lasso_downsample<- predict(logit_lasso_downsample,
                                                newdata = testing,
                                                type = "prob")[,1]

testResults$logit_lasso_downsample2<- predict(logit_lasso_downsample2,
                                             newdata = testing,
                                             type = "prob")[,1]

testResults$logit_ridge_downsample<- predict(logit_ridge_downsample,
                                             newdata = testing,
                                             type = "prob")[,1]

testResults$logit_ridge_downsample2<- predict(logit_ridge_downsample2,
                                             newdata = testing,
                                             type = "prob")[,1]

testResults$logit_elasticnet_downsample<- predict(logit_elasticnet_downsample,
                                             newdata = testing,
                                             type = "prob")[,1]

testResults$logit_elasticnet_downsample2<- predict(logit_elasticnet_downsample2,
                                                  newdata = testing,
                                                  type = "prob")[,1]

testResults$logit_lasso_smote<- predict(logit_lasso_smote,
                                                  newdata = testX,
                                                  type = "prob")[,1]

testResults$logit_lasso_smote2<- predict(logit_lasso_smote2,
                                        newdata = testX2,
                                        type = "prob")[,1]

testResults$logit_ridge_smote<- predict(logit_ridge_smote,
                                        newdata = testX,
                                        type = "prob")[,1]

testResults$logit_ridge_smote2<- predict(logit_ridge_smote2,
                                        newdata = testX2,
                                        type = "prob")[,1]

testResults$logit_elasticnet_smote<- predict(logit_elasticnet_smote,
                                        newdata = testX,
                                        type = "prob")[,1]

testResults$logit_elasticnet_smote2<- predict(logit_elasticnet_smote2,
                                             newdata = testX2,
                                             type = "prob")[,1]

testResults <- testResults %>% mutate(logit_thresh = logit,
                                      logit_thresh2 = logit2,
                                      logit_lasso_thresh = logit_lasso,
                                      logit_lasso_thresh2 = logit_lasso2,
                                      logit_ridge_thresh = logit_ridge,
                                      logit_ridge_thresh2 = logit_ridge2,
                                      logit_elasticnet_thresh = logit_elasticnet,
                                      logit_elasticnet_thresh2 = logit_elasticnet2,
                                      logit_lasso_upsample_thresh = logit_lasso_upsample,
                                      logit_lasso_upsample_thresh2 = logit_lasso_upsample2,
                                      logit_ridge_upsample_thresh = logit_ridge_upsample,
                                      logit_ridge_upsample_thresh2 = logit_ridge_upsample2,
                                      logit_elasticnet_upsample_thresh = logit_elasticnet_upsample,
                                      logit_elasticnet_upsample_thresh2 = logit_elasticnet_upsample2,
                                      logit_lasso_downsample_thresh = logit_lasso_downsample,
                                      logit_lasso_downsample_thresh2 = logit_lasso_downsample2,
                                      logit_ridge_downsample_thresh = logit_ridge_downsample,
                                      logit_ridge_downsample_thresh2 = logit_ridge_downsample2,
                                      logit_elasticnet_downsample_thresh = logit_elasticnet_downsample,
                                      logit_elasticnet_downsample_thresh2 = logit_elasticnet_downsample2,
                                      logit_lasso_smote_thresh = logit_lasso_smote,
                                      logit_lasso_smote_thresh2 = logit_lasso_smote2,
                                      logit_ridge_smote_thresh = logit_ridge_smote,
                                      logit_ridge_smote_thresh2 = logit_ridge_smote2,
                                      logit_elasticnet_smote_thresh = logit_elasticnet_smote,
                                      logit_elasticnet_smote_thresh2 = logit_elasticnet_smote2)

testResults<-testResults %>%
  mutate(logit=ifelse(logit>0.5,"Si","No"),
         logit2=ifelse(logit2>0.5,"Si","No"),
         logit_lasso=ifelse(logit_lasso>0.5,"Si","No"),
         logit_lasso2=ifelse(logit_lasso2>0.5,"Si","No"),
         logit_ridge=ifelse(logit_ridge>0.5,"Si","No"),
         logit_ridge2=ifelse(logit_ridge2>0.5,"Si","No"),
         logit_elasticnet=ifelse(logit_elasticnet>0.5,"Si","No"),
         logit_elasticnet2=ifelse(logit_elasticnet2>0.5,"Si","No"),
         logit_thresh=ifelse(logit_thresh>rfThresh_logit$threshold,"Si","No"),
         logit_thresh2=ifelse(logit_thresh2>rfThresh_logit2$threshold,"Si","No"),
         logit_lasso_thresh=ifelse(logit_lasso_thresh>rfThresh_logit_lasso$threshold,"Si","No"),
         logit_lasso_thresh2=ifelse(logit_lasso_thresh2>rfThresh_logit_lasso2$threshold,"Si","No"),
         logit_ridge_thresh=ifelse(logit_ridge_thresh>rfThresh_logit_ridge$threshold,"Si","No"),
         logit_ridge_thresh2=ifelse(logit_ridge_thresh2>rfThresh_logit_ridge2$threshold,"Si","No"),
         logit_elasticnet_thresh=ifelse(logit_elasticnet_thresh>rfThresh_logit_elasticnet$threshold,"Si","No"),
         logit_elasticnet_thresh2=ifelse(logit_elasticnet_thresh2>rfThresh_logit_elasticnet2$threshold,"Si","No"),
         logit_lasso_upsample=ifelse(logit_lasso_upsample>0.5,"Si","No"),
         logit_lasso_upsample2=ifelse(logit_lasso_upsample2>0.5,"Si","No"),
         logit_ridge_upsample=ifelse(logit_ridge_upsample>0.5,"Si","No"),
         logit_ridge_upsample2=ifelse(logit_ridge_upsample2>0.5,"Si","No"),
         logit_elasticnet_upsample=ifelse(logit_elasticnet_upsample>0.5,"Si","No"),
         logit_elasticnet_upsample2=ifelse(logit_elasticnet_upsample2>0.5,"Si","No"),
         logit_lasso_downsample=ifelse(logit_lasso_downsample>0.5,"Si","No"),
         logit_lasso_downsample2=ifelse(logit_lasso_downsample2>0.5,"Si","No"),
         logit_ridge_downsample=ifelse(logit_ridge_downsample>0.5,"Si","No"),
         logit_ridge_downsample2=ifelse(logit_ridge_downsample2>0.5,"Si","No"),
         logit_elasticnet_downsample=ifelse(logit_elasticnet_downsample>0.5,"Si","No"),
         logit_elasticnet_downsample2=ifelse(logit_elasticnet_downsample2>0.5,"Si","No"),
         logit_lasso_smote=ifelse(logit_lasso_smote>0.5,"Si","No"),
         logit_lasso_smote2=ifelse(logit_lasso_smote2>0.5,"Si","No"),
         logit_ridge_smote=ifelse(logit_ridge_smote>0.5,"Si","No"),
         logit_ridge_smote2=ifelse(logit_ridge_smote2>0.5,"Si","No"),
         logit_elasticnet_smote=ifelse(logit_elasticnet_smote>0.5,"Si","No"),
         logit_elasticnet_smote2=ifelse(logit_elasticnet_smote2>0.5,"Si","No"),
         logit_lasso_upsample_thresh=ifelse(logit_lasso_upsample_thresh>rfThresh_logit_lasso_upsample$threshold,"Si","No"),
         logit_lasso_upsample_thresh2=ifelse(logit_lasso_upsample_thresh2>rfThresh_logit_lasso_upsample2$threshold,"Si","No"),
         logit_ridge_upsample_thresh=ifelse(logit_ridge_upsample_thresh>rfThresh_logit_ridge_upsample$threshold,"Si","No"),
         logit_ridge_upsample_thresh2=ifelse(logit_ridge_upsample_thresh2>rfThresh_logit_ridge_upsample2$threshold,"Si","No"),
         logit_elasticnet_upsample_thresh=ifelse(logit_elasticnet_upsample_thresh>rfThresh_logit_elasticnet_upsample$threshold,"Si","No"),
         logit_elasticnet_upsample_thresh2=ifelse(logit_elasticnet_upsample_thresh2>rfThresh_logit_elasticnet_upsample2$threshold,"Si","No"),
         logit_lasso_downsample_thresh=ifelse(logit_lasso_downsample_thresh>rfThresh_logit_lasso_downsample$threshold,"Si","No"),
         logit_lasso_downsample_thresh2=ifelse(logit_lasso_downsample_thresh2>rfThresh_logit_lasso_downsample2$threshold,"Si","No"),
         logit_ridge_downsample_thresh=ifelse(logit_ridge_downsample_thresh>rfThresh_logit_ridge_downsample$threshold,"Si","No"),
         logit_ridge_downsample_thresh2=ifelse(logit_ridge_downsample_thresh2>rfThresh_logit_ridge_downsample2$threshold,"Si","No"),
         logit_elasticnet_downsample_thresh=ifelse(logit_elasticnet_downsample_thresh>rfThresh_logit_elasticnet_downsample$threshold,"Si","No"),
         logit_elasticnet_downsample_thresh2=ifelse(logit_elasticnet_downsample_thresh2>rfThresh_logit_elasticnet_downsample2$threshold,"Si","No"),
         logit_lasso_smote_thresh=ifelse(logit_lasso_smote_thresh>rfThresh_logit_lasso_smote$threshold,"Si","No"),
         logit_lasso_smote_thresh2=ifelse(logit_lasso_smote_thresh2>rfThresh_logit_lasso_smote2$threshold,"Si","No"),
         logit_ridge_smote_thresh=ifelse(logit_ridge_smote_thresh>rfThresh_logit_ridge_smote$threshold,"Si","No"),
         logit_ridge_smote_thresh2=ifelse(logit_ridge_smote_thresh2>rfThresh_logit_ridge_smote2$threshold,"Si","No"),
         logit_elasticnet_smote_thresh=ifelse(logit_elasticnet_smote_thresh>rfThresh_logit_elasticnet_smote$threshold,"Si","No"),
         logit_elasticnet_smote_thresh2=ifelse(logit_elasticnet_smote_thresh2>rfThresh_logit_elasticnet_smote2$threshold,"Si","No"),
  )

pred_tree<-predict(tree,testX)

pred_tree2<-predict(tree2,testX2)

pred_rf<-predict(forest,testing)

pred_rf2<-predict(forest2,testing)

pred_ada<-predict(adaboost,testing)

pred_ada2<-predict(adaboost2,testing)

with(testResults,table(Pobre,logit))

with(testResults,table(Pobre,logit2))

with(testResults,table(Pobre,logit_thresh))

with(testResults,table(Pobre,logit_thresh2))

with(testResults,table(Pobre,logit_lasso))

with(testResults,table(Pobre,logit_lasso2))

with(testResults,table(Pobre,logit_lasso_thresh))

with(testResults,table(Pobre,logit_lasso_thresh2))

with(testResults,table(Pobre,logit_ridge))

with(testResults,table(Pobre,logit_ridge2))

with(testResults,table(Pobre,logit_ridge_thresh))

with(testResults,table(Pobre,logit_ridge_thresh2))

with(testResults,table(Pobre,logit_elasticnet))

with(testResults,table(Pobre,logit_elasticnet2))

with(testResults,table(Pobre,logit_elasticnet_thresh))

with(testResults,table(Pobre,logit_elasticnet_thresh2))

with(testResults,table(Pobre,logit_lasso_upsample))

with(testResults,table(Pobre,logit_lasso_upsample2))

with(testResults,table(Pobre,logit_lasso_upsample_thresh))

with(testResults,table(Pobre,logit_lasso_upsample_thresh2))

with(testResults,table(Pobre,logit_ridge_upsample))

with(testResults,table(Pobre,logit_ridge_upsample2))

with(testResults,table(Pobre,logit_ridge_upsample_thresh))

with(testResults,table(Pobre,logit_ridge_upsample_thresh2))

with(testResults,table(Pobre,logit_elasticnet_upsample))

with(testResults,table(Pobre,logit_elasticnet_upsample2))

with(testResults,table(Pobre,logit_elasticnet_upsample_thresh))

with(testResults,table(Pobre,logit_elasticnet_upsample_thresh2))

with(testResults,table(Pobre,logit_lasso_downsample))

with(testResults,table(Pobre,logit_lasso_downsample2))

with(testResults,table(Pobre,logit_lasso_downsample_thresh))

with(testResults,table(Pobre,logit_lasso_downsample_thresh2))

with(testResults,table(Pobre,logit_ridge_downsample))

with(testResults,table(Pobre,logit_ridge_downsample2))

with(testResults,table(Pobre,logit_ridge_downsample_thresh))

with(testResults,table(Pobre,logit_ridge_downsample_thresh2))

with(testResults,table(Pobre,logit_elasticnet_downsample))

with(testResults,table(Pobre,logit_elasticnet_downsample2))

with(testResults,table(Pobre,logit_elasticnet_downsample_thresh))

with(testResults,table(Pobre,logit_elasticnet_downsample_thresh2))

with(testResults,table(Pobre,logit_lasso_smote))

with(testResults,table(Pobre,logit_lasso_smote2))

with(testResults,table(Pobre,logit_lasso_smote_thresh))

with(testResults,table(Pobre,logit_lasso_smote_thresh2))

with(testResults,table(Pobre,logit_ridge_smote))

with(testResults,table(Pobre,logit_ridge_smote2))

with(testResults,table(Pobre,logit_ridge_smote_thresh))

with(testResults,table(Pobre,logit_ridge_smote_thresh2))

with(testResults,table(Pobre,logit_elasticnet_smote))

with(testResults,table(Pobre,logit_elasticnet_smote2))

with(testResults,table(Pobre,logit_elasticnet_smote_thresh))

with(testResults,table(Pobre,logit_elasticnet_smote_thresh2))

confusionMatrix(testing$Pobre,pred_tree)

confusionMatrix(testing$Pobre,pred_tree2)

confusionMatrix(testing$Pobre,pred_rf)

confusionMatrix(testing$Pobre,pred_rf2)

confusionMatrix(testing$Pobre,pred_ada)

confusionMatrix(testing$Pobre,pred_ada2)