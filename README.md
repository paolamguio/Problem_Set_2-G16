# Problem_Set_2-G16

Universidad de los Andes

Maestría en Economía Aplicada

Andres Felipe Martinez - 202121008

Angela Paola Morales Guio – 201015503

Oscar Cortes - 200222692

# Problem Set 2: Predicting Poverty

Este repositorio contiene las siguientes carpetas

- `document`: Contiene el documento final del problem set 
- `scripts`: Contiene los scripts
	- `1. data_cleaning_training`: Este script contiene el proceso para limpiar la base de datos training
	- `2. data_cleaning_testing`: Este script contiene el proceso para limpiar la base de datos testing
	- `3. estad_descr`: Este script contiene las estadísticas descriptivas de las bases de datos
	- `4. classification_models`: Este script contiene el proceso de estimación de los modelos de clasificación para predecir  pobreza
	- `5. prediction_models`: Este script contiene el proceso de de estimación de los modelos de ingreso para predecir pobreza
	- `6. Data_submit`: Este script contiene la generación de la base de datos submit final
- `stores`: Contiene las bases de datos
	- `clasification_model_submit`: Esta base de datos contiene la predicción de pobreza del modelo de clasificación seleccionado 
	- `df_hogares`: Esta base de datos contiene las variables obtenidas en el data cleaning necesarias para el treaning
	- `df_hogares_predict`: Esta base de datos incluye como variable ingreso “ingtot” la cual considera los ingresos no laborales y se eliminan los valores iguales a cero
	- `df_test_hogares`: Esta base de datos contiene las variables obtenidas en el data cleaning necesarias para el testing 
	- `ingreso_model_submit`: Esta base de datos contiene la predicción de pobreza del modelo de ingresos seleccionado 
	- `submit_pobreG16`: este archivo csv contiene las predicciones finales de los modelos 
	- `test_hogares`, `test_personas`, `train_hogares`, `train_personas`: Estas bases de datos contienen la información inicial por hogar y personas para training y testing
- `views`: Contiene las gráficas y tablas del problem set
- `info`: Contiene el enunciado del Problem Set y documentos de apoyo

Este Problem Set se trabajó en la versión de R 4.2.0 
