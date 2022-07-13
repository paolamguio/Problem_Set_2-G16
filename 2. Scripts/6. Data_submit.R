
# Data submit
# Problem_Set_2 
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_2-G16/3. Stores")
setwd("C:/Users/ocaco/OneDrive/15. Maestria Economia/9. Big Data/3. GitHub/Problem_Set_2/3. Stores")

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
  class,
  caret
)

### 1. llamado bases de datos ###

Pobre_classification <- import("clasification_model_submit.rds")
Pobre_inncome <- import("ingreso_model_submit.rds")

submit_pobre <- left_join(Pobre_classification, Pobre_inncome) 

write_csv(submit_pobre, "submit_pobreG16.csv")