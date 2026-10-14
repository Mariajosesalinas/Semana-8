##T3 – Análisis de datos con CASEN y ENE

#Asignatura: Fundamentos de Programación para Análisis Económico
#Institución: Universidad de Concepción
#Autor/a: María José Salinas Correa
#Fecha: 13 Octubre de 2026

#1. Descripción del proyecto

#Este trabajo utiliza datos de encuestas nacionales para aplicar técnicas de 
#limpieza, análisis y modelación estadística mediante R. Se trabaja con las bases 
#CASEN y Encuesta Nacional de Empleo (ENE), organizando el código y guardando los 
#resultados en archivos CSV.

#2. Fuentes de datos

#CASEN: archivo data/raw/casen_reducido.csv.

#ENE: archivo data/raw/ene-2026-06-mjj.csv, correspondiente al trimestre móvil 
#mayo-junio-julio de 2026.

#Los archivos originales se encuentran en la carpeta data/raw/.

#3. Parte A: Análisis de CASEN

#Se estiman y comparan modelos de regresión lineal para estudiar la relación entre
#los ingresos y variables explicativas como la educación, la experiencia y el género.

#Se estiman tres modelos: uno simple, uno con variables de control y uno que 
#incorpora una interacción entre educación y género. También se examinan los 
#residuos y observaciones potencialmente influyentes para evaluar la 
#sensibilidad de los resultados.

#Resultado exportado: data/processed/t3_coeficientes_casen.csv.

#4. Parte B: Análisis de ENE

#Se limpian los datos de la ENE, seleccionando personas ocupadas con información 
#válida sobre las horas efectivamente trabajadas, la formalidad laboral y los 
#factores de expansión.

#La pregunta de investigación es:

#¿Existe una brecha en las horas semanales efectivamente trabajadas en la 
#actividad principal entre trabajadores formales e informales?

#Se estiman dos modelos de regresión lineal ponderados por el factor de expansión:
#uno que considera la formalidad laboral y otro que incorpora controles por edad 
#y sexo. Además, se revisan observaciones influyentes y se realiza un análisis 
#de sensibilidad.

#Resultados exportados:
#data/processed/t3_comparacion_modelos_ene.csv
#data/processed/t3_sensibilidad_ene.csv

#5. Parte C: Tabla de resultados

#Se utiliza la función modelsummary() para generar una tabla comparativa de 
#los dos modelos estimados con los datos de la ENE.
#Archivo generado: data/processed/t3_tabla_ene.csv.

#6. Organización de carpetas

#data/raw/: contiene los archivos originales.
#data/processed/: contiene los archivos CSV con los resultados.
#scripts/: contiene el código R utilizado para desarrollar la tarea.
#README.md: describe el proyecto, las fuentes de datos y los procedimientos realizados.

#7. Reproducibilidad

#El análisis se encuentra en el script scripts/tarea_t3.R. Para reproducirlo, 
#se debe abrir el proyecto desde su carpeta principal, verificar que los 
#archivos originales estén disponibles en data/raw/ y ejecutar el script en RStudio.