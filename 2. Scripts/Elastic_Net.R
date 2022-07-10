

## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_2-G16/3. Stores")

require(pacman)

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  pastecs,
  stargazer,
  here
)

require("glmnet")
require("caret")

df_hogares<- readRDS("df_hogares.rds")

df <- df_hogares %>% select(c("Pobre", "Nper", "Nro_personas_trabajo_formal", "tipo_vivienda", "Dominio", "cuota_amortizacion", "arriendo", "edad_promedio", "jefe_hogar_mujer", "Nro_hijos", "edu_promedio", "horas_trabajadas_promedio", "porcentaje_mujeres", "porcentaje_trabajo_formal", "porcentaje_subsidio_familiar", "segundo_trabajo", "otros_ingresos", "otros_ingresos_instituciones", "tasa_ocupacion", "tasa_desempleo", "tasa_participacion", "Ingtotob_hogar"))


#Como se parte de forma aleatoria necesito una semilla ára que me de igual siempre
set.seed(1101)

###--- Demo regularización - Elastic Net ---###

#Lasso
lambda<- 10^seq(-2, 3, length = 300)
lasso<- train(
  Pobre~., data = df, method = "glmnet",
  trControl = trainControl ("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1,
                         lambda = lambda), preProcess = c("center", "scale")
)
lasso

#Ridge
ridge<- train(
  Pobre~., data = df, method = "glmnet",
  trControl = trainControl ("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0,
                         lambda = lambda), preProcess = c("center", "scale")
)
ridge

# combinación de lasso y rigde = Elastic Net
el<- train(
  Pobre~., data = df,
  method = "glmnet",
  trControl = trainControl ("cv", number = 10),
  preProcess = c("center", "scale")
)
el # este me indica cual es el mejor

# Generación de lista de modelos para comparar
models<- list( ridge = ridge, lasso = lasso, elastic = el)
models
resamples(models) %>% summary(metric = "RMSE") # eligiria el modelo elastic porque es el de menor media de rmse

