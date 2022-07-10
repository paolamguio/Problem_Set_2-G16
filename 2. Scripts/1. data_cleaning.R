
# Data Cleaning
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
  gtsummary
)

### 1. llamado base de datos ###
df_hogares <- import("train_hogares.Rds")
df_personas <- import("train_personas.Rds")
df_test_hogares <- import("test_hogares.Rds")
df_test_personas <- import("test_personas.Rds")

colnames(df_hogares)
colnames(df_personas)
colnames(df_test_hogares)
colnames(df_test_personas)

summary(df_hogares)
summary(df_personas)
summary(df_test_hogares)
summary(df_test_personas)

### 2. limpieza base de datos ###

df_personas <- df_personas %>% select(colnames(df_test_personas), Ingtotob)
colnames(df_personas)

aggregate(df_personas$P6210s1, by = list(df_personas$P6210), mean, na.rm = TRUE)
aggregate(df_personas$P6210s1, by = list(df_personas$P6210), min, na.rm = TRUE)
aggregate(df_personas$P6210s1, by = list(df_personas$P6210), max, na.rm = TRUE)

df_personas %>% subset(P6210 == 2) %>% select(P6210s1) %>% table()
df_personas %>% subset(P6210 == 3) %>% select(P6210s1) %>% table()
df_personas %>% subset(P6210 == 4) %>% select(P6210s1) %>% table()
df_personas %>% subset(P6210 == 5) %>% select(P6210s1) %>% table()
df_personas %>% subset(P6210 == 6) %>% select(P6210s1) %>% table()

df_personas <- df_personas %>% mutate(female = ifelse(P6020 == 2, 1, 0))
df_personas <- df_personas %>% mutate(jefe_hogar = ifelse(P6050 == 1, 1, 0))
df_personas <- df_personas %>% mutate(hijo = ifelse(P6050 == 3, 1, 0))
df_personas <- df_personas %>% mutate(jefe_hogar_mujer = jefe_hogar*female)
df_personas <- df_personas %>% mutate(P6210s1 = ifelse(P6210 == 4 & P6210s1 == 0, 5, P6210s1))
df_personas <- df_personas %>% mutate(edu = case_when(P6210 == 1 ~ 0,
                                                      P6210 == 2 ~ P6210s1,
                                    P6210 == 3 ~ P6210s1 + 1,
                                    P6210 == 4 ~ P6210s1 + 1,
                                    P6210 == 5 ~ P6210s1 + 1,
                                    P6210 == 6 ~ P6210s1 + 12))

summary(df_personas$edu)

# creación de variables
df_personas <- df_personas %>% mutate(trabajo_formal = ifelse(P6920 == 1, 1, 0))
df_personas <- df_personas %>% mutate(segundo_trabajo = ifelse(P7040 == 1, 1, 0))
df_personas <- df_personas %>% mutate(arriendos = ifelse(P7495 == 1, 1, 0))
df_personas <- df_personas %>% mutate(pensiones = ifelse(P7500s2 == 1, 1, 0))
df_personas <- df_personas %>% mutate(pension_alimenticia = ifelse(P7500s3 == 1, 1, 0))
df_personas <- df_personas %>% mutate(otros_ingresos = ifelse(P7505 == 1, 1, 0))
df_personas <- df_personas %>% mutate(otros_ingresos_pais = ifelse(P7510s1 == 1, 1, 0))
df_personas <- df_personas %>% mutate(otros_ingresos_otros_paises = ifelse(P7510s2 == 1, 1, 0))
df_personas <- df_personas %>% mutate(otros_ingresos_instituciones = ifelse(P7510s3 == 1, 1, 0))
df_personas <- df_personas %>% mutate(otras_ganancias = ifelse(P7510s5 == 1, 1, 0))
df_hogares2<-df_personas %>% group_by(id) %>% summarize(Nro_mujeres=sum(female,na.rm = TRUE),
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
                                                        Nro_personas_inactivas=sum(Ina,na.rm = TRUE),
                                                        Ingtotob_hogar=sum(Ingtotob,na.rm = TRUE))

summary(df_hogares2)

df_hogares<-df_hogares %>% mutate(Pobre=factor(Pobre,levels=c(1,0),labels=c("Si","No")))
df_hogares<-df_hogares %>% mutate(Indigente=factor(Indigente,levels=c(1,0),labels=c("Si","No")))
df_hogares<-df_hogares %>% mutate(tipo_vivienda=factor(P5090,levels=c(1, 2, 3, 4, 5, 6),labels=c("Propia, totalmente pagada","Propia, la
están pagando", "En arriendo o
subarriendo", "En usufructo", "Posesión sin
titulo", "Otra")))
df_hogares <- df_hogares %>% mutate(Nro_cuartos = P5000)
df_hogares <- df_hogares %>% mutate(Nro_personas_cuartos = Nper/P5010)
df_hogares <- df_hogares %>% mutate(cuota_amortizacion = P5100)
df_hogares <- df_hogares %>% mutate(arriendo = P5140)

summary(df_hogares)

df_hogares <- left_join(df_hogares, df_hogares2)

### 3. base de datos final ###
saveRDS(df_hogares, file = "df_hogares.rds")
