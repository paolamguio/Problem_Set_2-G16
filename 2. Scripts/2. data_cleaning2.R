

# Data Cleaning 2
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
  gtsummary
)


### 1. llamado base de datos generada en 1.data_cleaning.R ###
df_hogares <- import("df_hogares.rds")
colnames(df_hogares)
summary(df_hogares)

### 2. limpieza base de datos ###
df_hogares <- df_hogares %>% mutate(Dominio = factor(Dominio))
df_hogares <- df_hogares %>% mutate(porcentaje_mujeres = Nro_mujeres/Nper,
                                    porcentaje_trabajo_formal = Nro_personas_trabajo_formal/Nro_personas_ocupadas,
                                    porcentaje_subsidio_familiar = Nro_personas_subsidio_familiar/Nper,
                                    segundo_trabajo = ifelse(Nro_personas_segundo_trabajo == 0, 0, 1),
                                    otros_ingresos = ifelse(Nro_personas_arriendos == 0 & Nro_personas_pensiones == 0 & Nro_personas_pension_alimenticia == 0 & Nro_personas_otros_ingresos == 0 & Nro_personas_otros_ingresos_pais == 0 & Nro_personas_otros_ingresos_otros_paises == 0 & Nro_personas_otras_ganancias == 0, 0, 1),
                                    otros_ingresos_instituciones = ifelse(Nro_personas_otros_ingresos_instituciones == 0, 0, 1),
                                    tasa_ocupacion = Nro_personas_ocupadas/Nro_personas_PET,
                                    tasa_desempleo = Nro_personas_desempleadas/(Nro_personas_PET - Nro_personas_inactivas),
                                    tasa_inactivas = Nro_personas_inactivas/Nro_personas_PET,
                                    tasa_participacion = (Nro_personas_PET - Nro_personas_inactivas)/Nro_personas_PET)

summary(df_hogares)

df_hogares <- df_hogares %>% replace_with_na(replace = list(Nro_cuartos = 98))
df_hogares <- df_hogares %>% mutate(cuota_amortizacion = ifelse(is.na(cuota_amortizacion) == T, 0, cuota_amortizacion),
                                    arriendo = ifelse(is.na(arriendo) == T, 0, arriendo),
                                    horas_trabajadas_promedio = ifelse(is.na(horas_trabajadas_promedio) == T, 0, horas_trabajadas_promedio),
                                    porcentaje_trabajo_formal = ifelse(is.na(porcentaje_trabajo_formal) == T, 0, porcentaje_trabajo_formal),
                                    tasa_desempleo = ifelse(is.na(tasa_desempleo) == T, 0, tasa_desempleo))

df_hogares <- df_hogares %>% subset(is.na(Nro_cuartos) == F)
df_hogares <- df_hogares %>% subset(is.na(edu_promedio) == F)

colnames(df_hogares)

# selección variables de interes 
df_hogares <- df_hogares %>% select(c("id", "Clase", "Dominio", "Nper", "Li", "Lp", "Pobre", "Npobres", "tipo_vivienda", "Nro_cuartos", "Nro_personas_cuartos", "cuota_amortizacion", "arriendo", "Nro_mujeres", "edad_promedio", "jefe_hogar_mujer", "Nro_hijos", "Nro_personas_trabajo_formal", "edu_promedio", "Nro_personas_subsidio_familiar", "horas_trabajadas_promedio", "Nro_personas_arriendos", "Nro_personas_pensiones", "Nro_personas_pension_alimenticia", "Nro_personas_otros_ingresos", "Nro_personas_otros_ingresos_pais", "Nro_personas_otros_ingresos_otros_paises", "Nro_personas_otros_ingresos_instituciones", "Nro_personas_otras_ganancias", "Nro_personas_PET", "Nro_personas_ocupadas", "Nro_personas_desempleadas", "Nro_personas_inactivas", "Ingtotob_hogar", "porcentaje_mujeres", "porcentaje_trabajo_formal", "porcentaje_subsidio_familiar", "segundo_trabajo", "otros_ingresos", "otros_ingresos_instituciones", "tasa_ocupacion", "tasa_desempleo", "tasa_inactivas", "tasa_participacion"))

summary(df_hogares)

### 3. base de datos final ###
saveRDS(df_hogares, file = "df_hogares.rds")