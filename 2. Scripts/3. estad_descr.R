
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

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  pastecs,
  stargazer,
  PerformanceAnalytics,
  naniar
)

##### Estadísticas descriptivas base training #####

## se importan bases de datos creada en 1.data_cleaning_training
df_hogares<- readRDS("df_hogares.rds")
dim(df_hogares)
colnames(df_hogares)
prop.table(table(df_hogares$Pobre)) #20% pobres y 80% no pobre

## Se seleccionan variables de interes
df <- df_hogares %>% select(c("Pobre", "Nper", "Nro_personas_trabajo_formal", "tipo_vivienda", "Dominio", "Nro_personas_cuartos", "cuota_amortizacion", "arriendo", "edad_promedio", "jefe_hogar_mujer", "Nro_hijos", "edu_promedio", "horas_trabajadas_promedio", "porcentaje_mujeres", "porcentaje_trabajo_formal", "porcentaje_subsidio_familiar", "segundo_trabajo", "otros_ingresos", "otros_ingresos_instituciones", "tasa_ocupacion", "tasa_desempleo", "tasa_participacion", "Ingtotob_hogar"))
df2 <- df %>% mutate(loging = log(Ingtotob_hogar))

### 1. estadísticas descriptivas ###
summary(df)

# estadísticas descriptivas variables de interes
require(gtsummary) #llamado librería
# estadísiticas descriptivas generales datos
stat.desc(df)
descriptivas <- stat.desc(df) # se guardan las estadísticas descriptivas de todas las variables para luego exportarlas a un excel
descriptivas$Estadisticas <- row.names(descriptivas) # se crea columna dentro del dataframe con el nombre de las filas 
descriptivas <- descriptivas %>% select(Estadisticas, everything()) # se ubica la columna creada en la primera posición 
write_xlsx(descriptivas, "descriptivas.xlsx") # se exporta a excel tabla con las estadísticas descriptivas

# Tablas descriptivas
tbl_summary(df, statistic = list (all_continuous()~"{mean} ({sd})")) # generales
tbl_summary(df, by= Pobre, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

## Gráficos

#Gráfico dispersión por ingreso
ggplot(data = df2, mapping = aes(x = Pobre , y = log)) +
  labs(x = "Pobre", y = "Ingreso promedio hogar") +
    geom_point(col = "chocolate4" , size = 0.5) 

ggplot(data = df , mapping = aes(x = Pobre , y = Nro_personas_cuartos)) +
  geom_point(col = "red" , size = 0.5) 

ggplot(data = df_hogares , mapping = aes(x = Pobre , y = Nro_personas_subsidio_familiar )) +
  geom_point(col = "red" , size = 0.5)

ggplot(data = df , 
       mapping = aes(x = edad_promedio , y = Pobre , group=as.factor(edu_promedio) , color=as.factor(edu_promedio))) +
  geom_point() 

p <- ggplot(data=df_hogares) + 
  geom_histogram(mapping = aes(x=Nro_personas_subsidio_familiar, group=(Pobre) , fill=(Pobre)))
p + scale_fill_manual(values = c("Si"="green" , "No"="blue") , label = c("Si"="Pobre" , "No"="No Pobre") , name = "Clasificación") # histograma relación ingreso por género, distribución de los datos hacia la izquierda, es asimétrica, lo mejor que se puede hacer es transformar la seria a log y de esta forma normalizar los datos 

p <- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=Nro_hijos, group=(Pobre) , fill=(Pobre)))
p + scale_fill_manual(values = c("Si"="green" , "No"="blue") , label = c("Si"="Pobre" , "No"="No Pobre") , name = "Clasificación") # histograma relación ingreso por género, distribución de los datos hacia la izquierda, es asimétrica, lo mejor que se puede hacer es transformar la seria a log y de esta forma normalizar los datos 

p <- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=porcentaje_mujeres, group=(Pobre) , fill=(Pobre)))
p + scale_fill_manual(values = c("Si"="green" , "No"="blue") , label = c("Si"="Pobre" , "No"="No Pobre") , name = "Clasificación") # histograma relación ingreso por género, distribución de los datos hacia la izquierda, es asimétrica, lo mejor que se puede hacer es transformar la seria a log y de esta forma normalizar los datos 

p <- ggplot(data=df2) + 
  geom_histogram(mapping = aes(x=loging, group=(Pobre) , fill=(Pobre)))
p + scale_fill_manual(values = c("Si"="green" , "No"="blue") , label = c("Si"="Pobre" , "No"="No Pobre") , name = "Clasificación") # histograma relación ingreso por género, distribución de los datos hacia la izquierda, es asimétrica, lo mejor que se puede hacer es transformar la seria a log y de esta forma normalizar los datos 


# Relación entre años de educación e ingresos por hogar
box_plot <- ggplot(data=df , mapping = aes(as.factor(edu_promedio) , Ingtotob_hogar)) + 
  geom_boxplot()
box_plot <- box_plot +
  labs(x = "No. años educación promedio", y = "Ingreso promedio por hogar") +
  geom_point(aes(colour=as.factor(Pobre))) +
  scale_color_manual(values = c("Si"="red" , "No"="blue") , label = c("Si"="Pobre" , "No"="No Pobre") , name = "Clasificación")
box_plot

# Relación número de personas por hogar con ingreso total 
box_plot2 <- ggplot(data=df , mapping = aes(as.factor(Nper) , Ingtotob_hogar)) + 
  geom_boxplot()
box_plot2 <- box_plot2 +
  labs(x = "No. de personas por hogar con trabajo formal", y = "Ingreso promedio por hogar") +
  geom_point(aes(colour=as.factor(Pobre))) + 
  scale_color_manual(values = c("Si"="red" , "No"="blue") , label = c("Si"="Pobre" , "1"="No Pobre") , name = "Clasificación")
box_plot2  

# Relación número de personas con trabajo formal por hogar con ingreso total 
box_plot3 <- ggplot(data=df , mapping = aes(as.factor(Nro_personas_trabajo_formal) , horas_trabajadas_promedio)) + 
  geom_boxplot()
box_plot3 <- box_plot3 +
  labs(x = "No. de personas por hogar con trabajo formal", y = "Horas promedio de trabajo semanal") +
  geom_point(aes(colour=as.factor(Pobre))) + 
  scale_color_manual(values = c("Si"="red" , "No"="blue") , label = c("Si"="Pobre" , "1"="No Pobre") , name = "Clasificación")
box_plot3 

# Relación tipo de vivienda e ingresos
box_plot4 <- ggplot(data=df , mapping = aes(as.factor(tipo_vivienda) , Ingtotob_hogar)) + 
  geom_boxplot()
box_plot4 <- box_plot4 +
  labs(x = "Tipo de vivienda", y = "Ingreso promedio por hogar") +
  geom_point(aes(colour=as.factor(Pobre))) + 
  scale_color_manual(values = c("Si"="red" , "No"="blue") , label = c("Si"="Pobre" , "1"="No Pobre") , name = "Clasificación")
box_plot4 

# Relación número de hijos con ingreso
box_plot5 <- ggplot(data=df , mapping = aes(as.factor(Nro_hijos) , Ingtotob_hogar)) + 
  geom_boxplot()
box_plot5 <- box_plot5 +
  labs(x = "No. hijos promedio hogar", y = "Ingreso promedio por hogar") +
  geom_point(aes(colour=as.factor(Pobre))) + 
  scale_color_manual(values = c("Si"="red" , "No"="blue") , label = c("Si"="Pobre" , "1"="No Pobre") , name = "Clasificación")
box_plot5 

# tabla de correlación entre las variables 
df_hogares %>% select(c("Pobre", "jefe_hogar_mujer", "Nro_hijos", "edu_promedio", "horas_trabajadas_promedio", "porcentaje_subsidio_familiar", "tasa_desempleo", "Ingtotob_hogar")) %>% chart.Correlation()


##### Estadísticas descriptivas base testing #####

## se importan bases de datos creada en 2.data_cleaning_testing
df_test_hogares<- readRDS("df_test_hogares.rds")
dim(df_test_hogares)
colnames(df_test_hogares)

## Se seleccionan variables de interes
dftest <- df_test_hogares %>% select(c("Nper", "Nro_personas_trabajo_formal", "tipo_vivienda", "Dominio", "Nro_personas_cuartos", "cuota_amortizacion", "arriendo", "edad_promedio", "jefe_hogar_mujer", "Nro_hijos", "edu_promedio", "horas_trabajadas_promedio", "porcentaje_mujeres", "porcentaje_trabajo_formal", "porcentaje_subsidio_familiar", "segundo_trabajo", "otros_ingresos", "otros_ingresos_instituciones", "tasa_ocupacion", "tasa_desempleo", "tasa_participacion"))

### 1. estadísticas descriptivas ###
summary(dftest)

# estadísticas descriptivas variables de interes
require(gtsummary) #llamado librería
# estadísiticas descriptivas generales datos
stat.desc(dftest)
descriptivastest <- stat.desc(dftest) # se guardan las estadísticas descriptivas de todas las variables para luego exportarlas a un excel
descriptivastest$Estadisticas <- row.names(descriptivastest) # se crea columna dentro del dataframe con el nombre de las filas 
descriptivastest <- descriptivastest %>% select(Estadisticas, everything()) # se ubica la columna creada en la primera posición 
write_xlsx(descriptivastest, "descriptivastesting.xlsx") # se exporta a excel tabla con las estadísticas descriptivas

# Tablas descriptivas
tbl_summary(dftest, statistic = list (all_continuous()~"{mean} ({sd})")) # generales

