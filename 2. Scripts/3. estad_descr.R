
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

## se importan bases de datos creada en 2.data_cleaning
df_hogares<- readRDS("df_hogares.rds")
dim(df_hogares)
colnames(df_hogares)
prop.table(table(df_hogares$Pobre)) #20% pobres y 80% no pobre

## Se seleccionan variables de interes
df <- df_hogares %>% select(c("Pobre", "Nper", "Nro_personas_trabajo_formal", "tipo_vivienda", "Dominio", "Nro_personas_cuartos", "cuota_amortizacion", "arriendo", "edad_promedio", "jefe_hogar_mujer", "Nro_hijos", "edu_promedio", "horas_trabajadas_promedio", "porcentaje_mujeres", "porcentaje_trabajo_formal", "porcentaje_subsidio_familiar", "segundo_trabajo", "otros_ingresos", "otros_ingresos_instituciones", "tasa_ocupacion", "tasa_desempleo", "tasa_participacion", "Ingtotob_hogar"))

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
tbl_summary(df) # generales
tbl_summary(df, by= Pobre, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

# Gráficos

#no
ggplot(data = df, mapping = aes(x = Ingtotob_hogar , y = Pobre)) +
  geom_point(col = "red" , size = 0.5) # gráfico de dispersión entre la edad y el ingreso total

ggplot(data = df , mapping = aes(x = Pobre , y = edad_promedio)) +
  geom_point(col = "red" , size = 0.5) # gráfico de dispersión entre años de educación y e ingreso total

ggplot(data = df , 
       mapping = aes(x = edad_promedio , y = Pobre , group=as.factor(edu_promedio) , color=as.factor(edu_promedio))) +
  geom_point() # trabajo formal (1) e informal (0)


p <- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=Nro_hijos, group=(Pobre) , fill=(Pobre)))
p + scale_fill_manual(values = c("Si"="green" , "No"="blue") , label = c("Si"="Pobre" , "No"="No Pobre") , name = "Clasificación") # histograma relación ingreso por género, distribución de los datos hacia la izquierda, es asimétrica, lo mejor que se puede hacer es transformar la seria a log y de esta forma normalizar los datos 

#puede ser
p <- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=Ingtotob_hogar, group=(Pobre) , fill=(Pobre)))
p + scale_fill_manual(values = c("Si"="green" , "No"="blue") , label = c("Si"="Pobre" , "No"="No Pobre") , name = "Clasificación") # histograma relación ingreso por género, distribución de los datos hacia la izquierda, es asimétrica, lo mejor que se puede hacer es transformar la seria a log y de esta forma normalizar los datos 


p <- ggplot(data=df) + 
  geom_histogram(mapping = aes(x=Ingtotob_hogar, group=(Pobre) , fill=(Pobre)))
p + scale_fill_manual(values = c("Si"="green" , "No"="blue") , label = c("Si"="Pobre" , "No"="No Pobre") , name = "Clasificación") # histograma relación ingreso por género, distribución de los datos hacia la izquierda, es asimétrica, lo mejor que se puede hacer es transformar la seria a log y de esta forma normalizar los datos 


#se esta ejecutando #############3
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
  labs(x = "No. de personas por hogar", y = "Ingreso mensual promedio hogar") +
  geom_point(aes(colour=as.factor(Pobre))) + 
  scale_color_manual(values = c("Si"="red" , "No"="blue") , label = c("Si"="Pobre" , "1"="No Pobre") , name = "Clasificación")
box_plot2  

#correr
box_plot3 <- ggplot(data=df , mapping = aes(as.factor(Nro_personas_trabajo_formal) , horas_trabajadas_promedio)) + 
  geom_boxplot()
box_plot3 <- box_plot2 +
  labs(x = "No. de personas por hogar con trabajo formal", y = "Horas promedio de trabajo semanal") +
  geom_point(aes(colour=as.factor(Pobre))) + 
  scale_color_manual(values = c("Si"="red" , "No"="blue") , label = c("Si"="Pobre" , "1"="No Pobre") , name = "Clasificación")
box_plot3 



df %>% select(c("Pobre", "Nper", "tipo_vivienda", "jefe_hogar_mujer", "Nro_hijos", "edu_promedio", "horas_trabajadas_promedio", "porcentaje_subsidio_familiar", "tasa_desempleo", "Ingtotob_hogar")) %>% chart.Correlation()