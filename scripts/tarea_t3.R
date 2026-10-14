# T3 — Regresión económica: CASEN y ENE
# Autora: María José Salinas Correa
# Fecha: 13-10-2026
# Descripción: Estimar, comparar e interpretar modelos de regresión
# utilizando las bases CASEN y ENE.

install.packages("modelsummary")

library(dplyr)
library(broom)
library(modelsummary)

# PARTE A: REGRESIONES CON CASEN

# A1. Crear la variable experiencia
# Descripción: calcula la experiencia potencial usando la edad
# y los años de educación, evitando valores negativos.

casen <- casen |>
  mutate(
    experiencia = pmax(edad - educ - 6, 0) )


# Cargar la base y crear nuevas variables
casen <- read.csv("~/Fund Programación/Semana 8/data/raw/casen_reducido.csv") |>
  mutate(
    superior = ifelse(educ >= 13, 1, 0),
    genero = as.factor(genero))

# Modelo 1: relacion entre ingreso y educacion
m1 <- lm(ingreso ~ educ, data = casen)
# Este modelo sirve como punto de partida y permite
# observar la asociacion sin incorporar otras variables.

# Modelo 2: agregar genero y educacion superior
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)
# Descripción: estima la asociación entre educación e ingreso,
# manteniendo constantes la experiencia y el género.

# Modelo 3: analizar si la relacion entre educacion e ingreso
# cambia segun el genero
m3 <- lm(ingreso ~ educ * genero + experiencia, data = casen)
# Verificar la cantidad de observaciones de cada modelo
# Descripción: comprueba cuántos casos utiliza cada regresión.

nobs(m1)
nobs(m2)
nobs(m3)
# Revisar cuantas observaciones utiliza cada modelo.
# Esto permite identificar si los modelos trabajan con
# distinta cantidad de casos por la presencia de datos NA.

# A2. Tabla de coeficientes del modelo 2
# Descripción: extrae los coeficientes, intervalos de confianza
# al 95%, valores p y significancia estadística.
# Excluye el intercepto y ordena por valor p.

tabla_coeficientes <- tidy(m2, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    p.value = round(p.value, 4),
    significativo = p.value < 0.05
  ) |>
  arrange(p.value)

# Mostrar la tabla
tabla_coeficientes

# A3. Comparación de los tres modelos
# Descripción: organiza los modelos en una lista y compara
# el número de observaciones, el R² y el R² ajustado,
# entre otros indicadores del ajuste.

modelos_casen <- list(
  simple = m1,
  controles = m2,
  mio = m3)

comparacion_casen <- lapply(modelos_casen, glance) |>
  bind_rows(.id = "modelo")

# Mostrar los indicadores de comparación
comparacion_casen |>
  select(modelo, nobs, r.squared, adj.r.squared, AIC, BIC)
# El modelo simple presenta un R² de 0.458 y un R² ajustado de 0.448.
# Al agregar experiencia y género, el R² aumenta a 0.475,
# pero el R² ajustado disminuye a 0.444.
# El modelo con interacción alcanza un R² de 0.480,
# aunque su R² ajustado baja a 0.438. Por ello, el aumento
# del R² no implica una mejora del modelo al considerar su complejidad.

# A4. Comparar el coeficiente de educación
# Descripción: extrae el coeficiente de educación de cada modelo
# para observar cómo cambia al incorporar controles e interacción.

coef_educacion <- bind_rows(
  tidy(m1) |> mutate(modelo = "simple"),
  tidy(m2) |> mutate(modelo = "controles"),
  tidy(m3) |> mutate(modelo = "mio")
) |>
  filter(term == "educ") |>
  select(modelo, term, estimate, std.error, p.value)

# Mostrar la comparación
coef_educacion
# El coeficiente de educación aumenta de $42.080 en el modelo simple
# a $42.246 en el modelo con controles. El cambio es pequeño,
# lo que indica que incorporar experiencia y género modifica poco
# la asociación estimada entre educación e ingreso en esta muestra.
# En el modelo con interacción, el coeficiente de educación es $46.646
# y corresponde al grupo de referencia de género. Los controles pueden
# modificar el coeficiente porque educación puede estar relacionada
# con otras variables incluidas en la regresión.

# A5.1. Identificar las tres observaciones peor predichas
# Descripción: calcula los valores predichos y los residuos
# del modelo 2, y ordena las observaciones por el tamaño
# absoluto del residuo.

diagnostico_m2 <- augment(m2) |>
  mutate(error_absoluto = abs(.resid)) |>
  arrange(desc(error_absoluto))

# Mostrar las tres observaciones con mayor error absoluto
diagnostico_m2 |>
  select(.rownames, .fitted, .resid, error_absoluto) |>
  slice_head(n = 3)

# A5.2. Diagnóstico de influencia y apalancamiento
# Descripción: calcula la máxima distancia de Cook y el máximo
# apalancamiento del modelo 2 para identificar observaciones
# que podrían influir en los resultados de la regresión.

resumen_diagnostico <- augment(m2) |>
  summarise(
    max_cook = max(.cooksd, na.rm = TRUE),
    max_apalancamiento = max(.hat, na.rm = TRUE)
  )

# Mostrar los valores máximos
resumen_diagnostico

# A5.3. Reestimar el modelo sin la observación más influyente
# Descripción: identifica la fila con mayor distancia de Cook,
# estima nuevamente el modelo sin esa observación y compara
# el coeficiente de educación antes y después de excluirla.

# Identificar la observación con mayor distancia de Cook
observacion_influyente <- augment(m2) |>
  arrange(desc(.cooksd)) |>
  slice_head(n = 1) |>
  pull(.rownames) |>
  as.integer()

# Mostrar el número de fila identificado
observacion_influyente

# Estimar el modelo sin esa observación
m2_sin_influyente <- casen |>
  filter(row_number() != observacion_influyente) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)

# Comparar el coeficiente de educación
# Descripción: muestra la estimación y el valor p con y sin
# la observación más influyente.

bind_rows(
  todos = tidy(m2),
  sin_influyente = tidy(m2_sin_influyente),
  .id = "muestra"
) |>
  filter(term == "educ") |>
  select(muestra, estimate, std.error, p.value)
#El coeficiente de educación cambia de $42.080 en el modelo simple
#a $42.246 al agregar experiencia y género. El cambio es pequeño,
#lo que indica que estos controles modifican poco la asociación
#estimada entre educación e ingreso en esta muestra.
#En el modelo con interacción, el coeficiente de educación es
#$46.646 y corresponde al género de referencia. Para el otro
#género, la asociación depende además del término de interacción.
#Estos resultados describen asociaciones, no relaciones causales.

# Revisar las características de las tres observaciones
# que presentan los mayores errores de predicción.

casen |>
  mutate(fila = row_number()) |>
  filter(fila %in% c(47, 51, 40)) |>
  select(fila, ingreso, edad, educ, experiencia, genero)
#Interpretación del diagnóstico:
#Las tres observaciones con mayor error absoluto corresponden
#a personas de género masculino, con edades entre 47 y 61 años
#y experiencia estimada entre 32 y 42 años.
#Sus ingresos mensuales varían entre $226.000 y $609.000.
#Esto muestra que, incluso entre personas con algunas características
#similares, existen diferencias importantes de ingresos que el
#modelo no logra capturar completamente. Podrían existir otras
#variables relevantes que no están incluidas en la regresión.

# A6. Exportar los coeficientes de CASEN
# Se crea la carpeta de resultados si todavía no existe.
dir.create("data/processed", showWarnings = FALSE)

# Se extraen los coeficientes del modelo con controles,
# incluyendo los intervalos de confianza al 95 %.
# El resultado se guarda como un archivo CSV.
tidy(m2, conf.int = TRUE) |>
  write.csv(
    "data/processed/t3_coeficientes_casen.csv",
    row.names = FALSE )
# Comprobar que el archivo CSV se guardó correctamente.
file.exists("data/processed/t3_coeficientes_casen.csv")

# B1. Cargar la base original de la ENE.
# Descripción: importa los datos del trimestre móvil
# mayo-junio-julio de 2026 para revisar las variables
# disponibles antes de preparar los modelos de regresión.

ene_original <- read.csv2("data/raw/ene-2026-06-mjj.csv")

# Revisar las dimensiones de la base original.
# Descripción: muestra la cantidad de filas y columnas.
dim(ene_original)

# Revisar los nombres de las variables.
# Descripción: permite identificar las variables de horas trabajadas,
# formalidad laboral y factor de expansión.
names(ene_original)

# Revisar la estructura de las variables.
# Descripción: muestra el tipo de dato de cada variable
# y algunos ejemplos de sus valores.
str(ene_original)


# B2. Revisar las variables necesarias para la regresión.
# Descripción: se examinan las horas efectivas, la formalidad,
# la situación de actividad y el factor de expansión para
# verificar sus códigos y detectar valores especiales.

# Revisar la distribución de las horas efectivamente trabajadas.
summary(ene_original$efectivas)
table(ene_original$efectivas, useNA = "ifany")

# Revisar los códigos de formalidad laboral.
table(ene_original$ocup_form, useNA = "ifany")

# Revisar los códigos de actividad.
table(ene_original$activ, useNA = "ifany")

# Revisar el factor de expansión.
summary(ene_original$fact_cal)

# Contar los valores especiales 888 y 999 en horas efectivas.
sum(ene_original$efectivas == 888, na.rm = TRUE)
sum(ene_original$efectivas == 999, na.rm = TRUE)

# B3. Revisar la relación entre actividad, formalidad y horas.
# Descripción: se cruzan las variables para identificar qué códigos
# aparecen entre las personas ocupadas y comprobar los valores
# disponibles antes de construir la base para la regresión.

# Cruzar actividad y formalidad.
table(
  ene_original$activ,
  ene_original$ocup_form,
  useNA = "ifany")

# Revisar las horas efectivas según el código de actividad.
aggregate(
  efectivas ~ activ,
  data = ene_original,
  FUN = function(x) sum(!is.na(x)))

# Revisar los códigos de formalidad entre las personas
# clasificadas con actividad igual a 1.
table(
  ene_original$ocup_form[ene_original$activ == 1],
  useNA = "ifany")

# B4. Limpiar los datos para preparar los modelos de regresión.
# Descripción: se seleccionan personas ocupadas con formalidad
# identificada, horas efectivas válidas y factor de expansión
# disponible. Los códigos 888 y 999 se excluyen porque representan
# valores especiales y no cantidades reales de horas.

ene_modelo <- ene_original |>
  filter(
    activ == 1,
    ocup_form %in% c(1, 2),
    !is.na(efectivas),
    !efectivas %in% c(888, 999),
    !is.na(fact_cal)
  ) |>
  mutate(
    formalidad = factor(
      ifelse(ocup_form == 1, "Formal", "Informal"),
      levels = c("Formal", "Informal")
    )
  )

# Revisar las dimensiones de la base limpia.
# Descripción: permite conocer cuántas observaciones y variables
# quedaron disponibles después de aplicar los filtros.
dim(ene_modelo)

# Contar las personas de cada grupo de formalidad.
# Descripción: comprueba cuántas observaciones formales e informales
# se utilizarán para comparar las horas efectivamente trabajadas.
table(ene_modelo$formalidad, useNA = "ifany")

# Revisar las horas efectivas después de la limpieza.
# Descripción: verifica que ya no existan los códigos especiales
# 888 y 999 en la variable dependiente.
summary(ene_modelo$efectivas)

# B5. Revisar las variables de control.
# Descripción: se examinan edad, sexo, educación y categoría
# ocupacional para elegir controles apropiados para los modelos.
# También se revisan sus valores faltantes y categorías.

# Revisar la distribución de edad.
summary(ene_modelo$edad)

# Revisar los códigos de sexo.
table(ene_modelo$sexo, useNA = "ifany")

# Revisar los códigos de educación.
table(ene_modelo$nivel, useNA = "ifany")

# Revisar las categorías de ocupación.
table(ene_modelo$categoria_ocupacion, useNA = "ifany")

# Revisar cuántos valores faltantes tiene cada variable candidata.
sapply(
  ene_modelo[c("edad", "sexo", "nivel", "categoria_ocupacion")],
  function(x) sum(is.na(x)))

# B6. Preparar las variables para la regresión.
# Descripción: se convierte sexo en una variable categórica.
# La formalidad ya está codificada como factor, con Formal
# como categoría de referencia e Informal como comparación.

ene_modelo <- ene_modelo |>
  mutate(
    sexo = factor(sexo))

# B6.1. Modelo 1: asociación entre formalidad y horas efectivas.
# Descripción: estima la diferencia promedio de horas efectivas
# entre trabajadores informales y formales, usando el factor
# de expansión de la encuesta.

m1_ene <- lm(
  efectivas ~ formalidad,
  data = ene_modelo,
  weights = fact_cal)

# B6.2. Modelo 2: incorporar variables de control.
# Descripción: estima la asociación entre formalidad y horas
# efectivas, manteniendo constantes la edad y el sexo.
# Esto permite comprobar si la diferencia estimada cambia
# al considerar estas características.

m2_ene <- lm(
  efectivas ~ formalidad + edad + sexo,
  data = ene_modelo,
  weights = fact_ca)

# B6.3. Revisar los resultados de ambos modelos.
# Descripción: muestra los coeficientes, errores estándar,
# valores p y medidas de ajuste para cada regresión.

summary(m1_ene)
summary(m2_ene)

# B6.2. Estimar el segundo modelo con variables de control.
# Descripción: analizar la relación entre formalidad y horas
# efectivamente trabajadas, controlando por edad y sexo.

m2_ene <- lm(
  efectivas ~ formalidad + edad + sexo,
  data = ene_modelo,
  weights = fact_cal)

# Mostrar los resultados del segundo modelo.
summary(m2_ene)

# B7. Comparar los modelos de regresión de la ENE.
# Descripción: reunir las principales medidas de ajuste
# para comparar el modelo simple con el modelo que incluye controles.

comparacion_ene <- bind_rows(
  # Extraer las medidas de ajuste del modelo 1.
  broom::glance(m1_ene) |>
    mutate(modelo = "Modelo 1: solo formalidad"),
  
  # Extraer las medidas de ajuste del modelo 2.
  broom::glance(m2_ene) |>
    mutate(modelo = "Modelo 2: formalidad, edad y sexo")) |>
  # Ordenar las columnas para facilitar la lectura.
  select(modelo, nobs, r.squared, adj.r.squared, AIC, BIC)

# Mostrar la comparación en la consola.
comparacion_ene

# Crear la carpeta de resultados si aún no existe.
dir.create("data/processed", showWarnings = FALSE)

# Guardar la comparación de modelos en un archivo CSV.
write.csv(
  comparacion_ene,
  "data/processed/t3_comparacion_modelos_ene.csv",
  row.names = FALSE)

# Verificar que el archivo se haya guardado correctamente.
file.exists("data/processed/t3_comparacion_modelos_ene.csv")

# B8. Análisis de sensibilidad del modelo ENE.
# Descripción: calcular medidas de influencia para detectar
# observaciones que podrían afectar los coeficientes estimados.

# Obtener residuos, valores ajustados y distancia de Cook.
diagnostico_ene <- broom::augment(m2_ene)

# Agregar el número de fila para identificar cada observación.
diagnostico_ene <- diagnostico_ene |>
  mutate(fila = seq_len(n()))

# Ordenar por distancia de Cook y mostrar las cinco primeras.
diagnostico_ene |>
  arrange(desc(.cooksd)) |>
  select(fila, .fitted, .resid, .cooksd) |>
  slice_head(n = 5)

# B9. Análisis de sensibilidad.
# Descripción: comparar el coeficiente de informalidad del modelo
# original con el resultado al excluir la observación más influyente.

# Identificar la observación con mayor distancia de Cook.
fila_influyente <- diagnostico_ene |>
  slice_max(.cooksd, n = 1, with_ties = FALSE) |>
  pull(fila)

# Crear una base temporal sin esa observación.
ene_sensibilidad <- ene_modelo |>
  mutate(fila = row_number()) |>
  filter(fila != fila_influyente) |>
  select(-fila)

# Estimar nuevamente el modelo con los mismos controles.
m2_ene_sensibilidad <- lm(
  efectivas ~ formalidad + edad + sexo,
  data = ene_sensibilidad,
  weights = fact_cal
)

# Mostrar el coeficiente de informalidad del modelo original.
coef_original <- coef(m2_ene)["formalidadInformal"]

# Mostrar el coeficiente de informalidad del modelo sin esa observación.
coef_sensibilidad <- coef(m2_ene_sensibilidad)["formalidadInformal"]

# Comparar los dos coeficientes.
data.frame(
  modelo = c("Original", "Sin observación influyente"),
  coeficiente = c(
    as.numeric(coef_original),
    as.numeric(coef_sensibilidad)
  )
)

# Calcular el cambio en la estimación.
as.numeric(coef_sensibilidad - coef_original)

# B10. Guardar los resultados del análisis de sensibilidad.
# Descripción: crear una tabla que compare el coeficiente
# de informalidad antes y después de excluir una observación.

tabla_sensibilidad <- data.frame(
  modelo = c(
    "Modelo original",
    "Modelo sin observación influyente"
  ),
  coeficiente_informalidad = c(
    as.numeric(coef_original),
    as.numeric(coef_sensibilidad)),
  cambio_respecto_original = c(
    0,
    as.numeric(coef_sensibilidad - coef_original)
  ))

# Mostrar la tabla para revisar los resultados.
tabla_sensibilidad

# Guardar la tabla en la carpeta de resultados.
write.csv(
  tabla_sensibilidad,
  "data/processed/t3_sensibilidad_ene.csv",
  row.names = FALSE)

# Comprobar que el archivo se guardó.
file.exists("data/processed/t3_sensibilidad_ene.csv")

# C1. Crear la tabla final de regresiones.
# Descripción: comparar los dos modelos ENE, incluyendo sus
# coeficientes, errores estándar, significancia y R².

modelsummary(
  list(
    "Modelo 1: solo formalidad" = m1_ene,
    "Modelo 2: con controles" = m2_ene ),
  stars = TRUE,
  statistic = "({std.error})",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "data/processed/t3_tabla_ene.csv")

# Comprobar que el archivo se haya creado.
file.exists("data/processed/t3_tabla_ene.csv")


## Declaración de autoría y uso de IA
#Herramienta utilizada: ChatGPT.

#Para qué la usé: Para resolver dudas sobre el código en R, comprender el 
#funcionamiento de algunas funciones, identificar errores y revisar la redacción
#de las explicaciones.

#Qué hice yo: Desarrollé la tarea paso a paso, trabajé con los datos, ejecuté y 
#revisé el código, participé en las decisiones del análisis y elaboré las 
#interpretaciones de los resultados.

#Verificación: Confirmo que entiendo y puedo explicar el código y los resultados
#que entrego.