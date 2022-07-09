
# Estadísticas descriptivas
# Problem_Set_2 
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_2-G16/3. Stores")

## llamado librerías de la sesión
require(pacman)
require(gtsummary)

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  pastecs,
  stargazer
)

## se importan bases de datos creada en 2.data_cleaning
df_hogares<- readRDS("df_hogares.rds")
dim(df_hogares)
colnames(df_hogares)

### 1. estadísticas descriptivas ###
tbl_summary(df_hogares)

summary(df_hogares)

# estadísticas descriptivas por variable de interes

prop.table(table(df_hogares$Pobre))



model <- as.formula("Pobre ~ tipo_vivienda + Dominio + Nro_personas_cuartos + cuota_amortizacion + arriendo + edad_promedio + jefe_hogar_mujer + Nro_hijos + edu_promedio + horas_trabajadas_promedio + porcentaje_mujeres + porcentaje_trabajo_formal + porcentaje_subsidio_familiar + segundo_trabajo + otros_ingresos + otros_ingresos_instituciones + tasa_ocupacion + tasa_desempleo + tasa_participacion")

model2 <- as.formula("Pobre ~ tipo_vivienda + Dominio + Nro_personas_cuartos + cuota_amortizacion + arriendo + edad_promedio + edad_promedio:porcentaje_mujeres + jefe_hogar_mujer + Nro_hijos + Nro_hijos:porcentaje_mujeres + edu_promedio + edu_promedio:porcentaje_mujeres + edu_promedio:Dominio + edu_promedio:jefe_hogar_mujer + horas_trabajadas_promedio + horas_trabajadas_promedio:porcentaje_mujeres + horas_trabajadas_promedio:jefe_hogar_mujer + porcentaje_mujeres + porcentaje_trabajo_formal + porcentaje_subsidio_familiar + segundo_trabajo + otros_ingresos + otros_ingresos_instituciones + tasa_ocupacion + tasa_desempleo + tasa_participacion")

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
