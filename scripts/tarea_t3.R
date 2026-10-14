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