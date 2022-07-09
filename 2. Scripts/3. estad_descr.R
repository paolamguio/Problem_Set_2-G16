
# Estadísticas descriptivas
# Problem_Set_2 
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_2/3. Stores")

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

library(datasets)
library(data.table)

## se importan bases de datos creada en 2.data_cleaning
df_hogares<- readRDS("df_hogares.rds")
dim(df_hogares)
colnames(df_hogares)

#estadísticas descriptivas
tbl_summary(df_hogares)

summary(df_hogares)