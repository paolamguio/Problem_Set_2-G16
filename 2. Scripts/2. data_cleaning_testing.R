
# Data Cleaning base testing
# Problem_Set_2 
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_2-G16/3. Stores")
setwd("C:/Users/ocaco/OneDrive/15. Maestria Economia/9. Big Data/3. GitHub/Problem_Set_2/3. Stores")

## llamado librerías de la sesión
install.packages("pacman")
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

### 1. llamado base de datos ###
df_test_hogares <- import("test_hogares.Rds")
df_test_personas <- import("test_personas.Rds")

colnames(df_test_hogares)
colnames(df_test_personas)

summary(df_test_hogares)
summary(df_test_personas)

### 2. Generación de variables que se van a utilizar de la base personas ###

df_test_personas <- df_test_personas %>% mutate(female = ifelse(P6020 == 2, 1, 0))
df_test_personas <- df_test_personas %>% mutate(jefe_hogar = ifelse(P6050 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(hijo = ifelse(P6050 == 3, 1, 0))
df_test_personas <- df_test_personas %>% mutate(jefe_hogar_mujer = jefe_hogar*female)
df_test_personas <- df_test_personas %>% mutate(P6210s1 = ifelse(P6210 == 4 & P6210s1 == 0, 5, P6210s1))
df_test_personas <- df_test_personas %>% mutate(edu = case_when(P6210 == 1 ~ 0,
                                                      P6210 == 2 ~ P6210s1,
                                                      P6210 == 3 ~ P6210s1 + 1,
                                                      P6210 == 4 ~ P6210s1 + 1,
                                                      P6210 == 5 ~ P6210s1 + 1,
                                                      P6210 == 6 ~ P6210s1 + 12))

summary(df_test_personas$edu)

df_test_personas <- df_test_personas %>% mutate(trabajo_formal = ifelse(P6920 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(segundo_trabajo = ifelse(P7040 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(arriendos = ifelse(P7495 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(pensiones = ifelse(P7500s2 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(pension_alimenticia = ifelse(P7500s3 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(otros_ingresos = ifelse(P7505 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(otros_ingresos_pais = ifelse(P7510s1 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(otros_ingresos_otros_paises = ifelse(P7510s2 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(otros_ingresos_instituciones = ifelse(P7510s3 == 1, 1, 0))
df_test_personas <- df_test_personas %>% mutate(otras_ganancias = ifelse(P7510s5 == 1, 1, 0))

## Se agrupa la información de la base personas por hogar para unirla con la base de hogar
df_test_hogares2<-df_test_personas %>% group_by(id) %>% summarize(Nro_mujeres=sum(female,na.rm = TRUE),
                                                        edad_promedio=mean(P6040,na.rm = TRUE),
                                                        jefe_hogar_mujer=sum(jefe_hogar_mujer,na.rm = TRUE),
                                                        Nro_hijos=sum(hijo,na.rm = TRUE),
                                                        Nro_personas_trabajo_formal=sum(P6090,na.rm = TRUE),
                                                        edu_promedio=mean(edu,na.rm = TRUE),
                                                        Nro_personas_subsidio_familiar=sum(P6585s3,na.rm = TRUE),
                                                        horas_trabajadas_promedio=mean(P6800,na.rm = TRUE),
                                                        Nro_personas_empleo_propio=sum(P6870,na.rm = TRUE),
                                                        Nro_personas_trabajo_formal=sum(trabajo_formal,na.rm = TRUE),
                                                        Nro_personas_segundo_trabajo=sum(segundo_trabajo,na.rm = TRUE),
                                                        Nro_personas_arriendos=sum(arriendos,na.rm = TRUE),
                                                        Nro_personas_pensiones=sum(pensiones,na.rm = TRUE),
                                                        Nro_personas_pension_alimenticia=sum(pension_alimenticia,na.rm = TRUE),
                                                        Nro_personas_otros_ingresos=sum(otros_ingresos,na.rm = TRUE),
                                                        Nro_personas_otros_ingresos_pais=sum(otros_ingresos_pais,na.rm = TRUE),
                                                        Nro_personas_otros_ingresos_otros_paises=sum(otros_ingresos_otros_paises,na.rm = TRUE),
                                                        Nro_personas_otros_ingresos_instituciones=sum(otros_ingresos_instituciones,na.rm = TRUE),
                                                        Nro_personas_otras_ganancias=sum(otras_ganancias,na.rm = TRUE),
                                                        Nro_personas_PET=sum(Pet,na.rm = TRUE),
                                                        Nro_personas_ocupadas=sum(Oc,na.rm = TRUE),
                                                        Nro_personas_desempleadas=sum(Des,na.rm = TRUE),
                                                        Nro_personas_inactivas=sum(Ina,na.rm = TRUE))

summary(df_test_hogares2)

df_test_hogares<-df_test_hogares %>% mutate(tipo_vivienda=factor(P5090,levels=c(1, 2, 3, 4, 5, 6),labels=c("Propia, totalmente pagada","Propia, la
están pagando", "En arriendo o
subarriendo", "En usufructo", "Posesión sin
titulo", "Otra")))

## se crean variables adicionales en la base de hogar 
df_test_hogares <- df_test_hogares %>% mutate(Nro_cuartos = P5000)
df_test_hogares <- df_test_hogares %>% mutate(Nro_personas_cuartos = Nper/P5010)
df_test_hogares <- df_test_hogares %>% mutate(cuota_amortizacion = P5100)
df_test_hogares <- df_test_hogares %>% mutate(arriendo = P5140)

summary(df_test_hogares)

df_test_hogares <- left_join(df_test_hogares, df_test_hogares2)

### 3. ajuste de variables a utilizar en el modelo final ###
df_test_hogares <- df_test_hogares %>% mutate(Dominio = factor(Dominio))
df_test_hogares <- df_test_hogares %>% mutate(porcentaje_mujeres = Nro_mujeres/Nper,
                                              porcentaje_trabajo_formal = Nro_personas_trabajo_formal/Nro_personas_ocupadas,
                                              porcentaje_subsidio_familiar = Nro_personas_subsidio_familiar/Nper,
                                              segundo_trabajo = ifelse(Nro_personas_segundo_trabajo == 0, 0, 1),
                                              otros_ingresos = ifelse(Nro_personas_arriendos == 0 & Nro_personas_pensiones == 0 & Nro_personas_pension_alimenticia == 0 & Nro_personas_otros_ingresos == 0 & Nro_personas_otros_ingresos_pais == 0 & Nro_personas_otros_ingresos_otros_paises == 0 & Nro_personas_otras_ganancias == 0, 0, 1),
                                              otros_ingresos_instituciones = ifelse(Nro_personas_otros_ingresos_instituciones == 0, 0, 1),
                                              tasa_ocupacion = Nro_personas_ocupadas/Nro_personas_PET,
                                              tasa_desempleo = Nro_personas_desempleadas/(Nro_personas_PET - Nro_personas_inactivas),
                                              tasa_inactivas = Nro_personas_inactivas/Nro_personas_PET,
                                              tasa_participacion = (Nro_personas_PET - Nro_personas_inactivas)/Nro_personas_PET)

summary(df_test_hogares)

### 4. ajuste de missing values ###
df_test_hogares <- df_test_hogares %>% replace_with_na(replace = list(Nro_cuartos = 98))
df_test_hogares <- df_test_hogares %>% mutate(cuota_amortizacion = ifelse(is.na(cuota_amortizacion) == T, 0, cuota_amortizacion),
                                              arriendo = ifelse(is.na(arriendo) == T, 0, arriendo),
                                              horas_trabajadas_promedio = ifelse(is.na(horas_trabajadas_promedio) == T, 0, horas_trabajadas_promedio),
                                              porcentaje_trabajo_formal = ifelse(is.na(porcentaje_trabajo_formal) == T, 0, porcentaje_trabajo_formal),
                                              tasa_desempleo = ifelse(is.na(tasa_desempleo) == T, 0, tasa_desempleo),
                                              tasa_ocupacion = ifelse(is.na(tasa_ocupacion) == T, 0, tasa_ocupacion),
                                              tasa_inactivas = ifelse(is.na(tasa_inactivas) == T, 0, tasa_inactivas),
                                              tasa_participacion = ifelse(is.na(tasa_participacion) == T, 0, tasa_participacion))

df_test_hogares <- df_test_hogares %>% 
  group_by(edad_promedio) %>% 
  mutate(mean_edu_promedio = mean(edu_promedio,na.rm=T))

df_test_hogares <- df_test_hogares %>% ungroup() %>%
  mutate(edu_promedio = ifelse(test = is.na(edu_promedio)==T,
                               yes = mean_edu_promedio,
                               no = edu_promedio))

# selección variables de interes 
df_test_hogares <- df_test_hogares %>% select(c("id", "Clase", "Dominio", "Nper", "Li", "Lp", "tipo_vivienda", "Nro_cuartos", "Nro_personas_cuartos", "cuota_amortizacion", "arriendo", "Nro_mujeres", "edad_promedio", "jefe_hogar_mujer", "Nro_hijos", "Nro_personas_trabajo_formal", "edu_promedio", "Nro_personas_subsidio_familiar", "horas_trabajadas_promedio", "Nro_personas_arriendos", "Nro_personas_pensiones", "Nro_personas_pension_alimenticia", "Nro_personas_otros_ingresos", "Nro_personas_otros_ingresos_pais", "Nro_personas_otros_ingresos_otros_paises", "Nro_personas_otros_ingresos_instituciones", "Nro_personas_otras_ganancias", "Nro_personas_PET", "Nro_personas_ocupadas", "Nro_personas_desempleadas", "Nro_personas_inactivas", "porcentaje_mujeres", "porcentaje_trabajo_formal", "porcentaje_subsidio_familiar", "segundo_trabajo", "otros_ingresos", "otros_ingresos_instituciones", "tasa_ocupacion", "tasa_desempleo", "tasa_inactivas", "tasa_participacion"))
df_test_hogares <- df_test_hogares %>% mutate(edad_promedio2 = edad_promedio^2)

summary(df_test_hogares)

colnames(df_test_hogares)

### 5. base de datos final ###
saveRDS(df_test_hogares, file = "df_test_hogares.rds")