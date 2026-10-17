# TRABAJO PRACTICO: ARBOLES DE CLASIFICACION
# Dataset integrado: iris
# Dataset de Kaggle: Telco Customer Churn
# Fuente: https://www.kaggle.com/datasets/blastchar/telco-customer-churn


# ============================================================
# 0. PAQUETES
# ============================================================

# Instalar solamente los paquetes que falten
paquetes <- c("caTools", "rpart", "rpart.plot")
faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  install.packages(
    faltantes,
    repos = "https://cloud.r-project.org"
  )
}

library(caTools)
library(rpart)
library(rpart.plot)


# ============================================================
# EJERCICIO 1: DATASET INTEGRADO IRIS
# ============================================================

# Objetivo: predecir la especie a partir de las medidas de la flor.

head(iris)
dim(iris)
str(iris)
summary(iris)
sum(is.na(iris))
table(iris$Species)

# Diagnostico:
# 150 observaciones, 4 variables predictoras numericas y una
# variable objetivo categorica. No hay valores faltantes.
# Cada especie tiene 50 observaciones.
# La division mantiene la proporcion de especies; esto no
# garantiza representatividad respecto de cualquier poblacion.

# Separar aproximadamente 75% para entrenamiento y 25% para prueba
set.seed(42)

division <- sample.split(
  iris$Species,
  SplitRatio = 0.75
)

entrenamiento <- iris[division, ]
prueba <- iris[!division, ]

table(entrenamiento$Species)
table(prueba$Species)

# Entrenar el modelo
modelo_arbol <- rpart(
  Species ~ .,
  data = entrenamiento,
  method = "class"
)

print(modelo_arbol)
rpart.plot(modelo_arbol, main = "Iris: arbol original")

# Predecir y evaluar
prediccion <- predict(
  modelo_arbol,
  newdata = prueba,
  type = "class"
)

table(
  Prediccion = prediccion,
  Real = prueba$Species
)

exactitud <- mean(prediccion == prueba$Species)
exactitud * 100

# Resultado observado:
# 33 aciertos sobre 36 flores: 91,67%.
# Tres virginica fueron clasificadas como versicolor.


# ============================================================
# EJERCICIO 3, PARTE A: AJUSTE DE IRIS
# ============================================================

# Primera variante: modificar condiciones de crecimiento
set.seed(42)

modelo_ajustado <- rpart(
  Species ~ .,
  data = entrenamiento,
  method = "class",
  control = rpart.control(
    minsplit = 10,
    minbucket = 3,
    maxdepth = 3
  )
)

print(modelo_ajustado)

# Segunda variante: reducir cp y usar validacion cruzada
set.seed(42)

modelo_amplio <- rpart(
  Species ~ .,
  data = entrenamiento,
  method = "class",
  control = rpart.control(
    minsplit = 10,
    minbucket = 3,
    maxdepth = 3,
    cp = 0.001,
    xval = 10
  )
)

printcp(modelo_amplio)

# Elegir cp por el menor error de validacion dentro del entrenamiento
mejor_cp <- modelo_amplio$cptable[
  which.min(modelo_amplio$cptable[, "xerror"]),
  "CP"
]

modelo_final <- prune(
  modelo_amplio,
  cp = mejor_cp
)

prediccion_final <- predict(
  modelo_final,
  newdata = prueba,
  type = "class"
)

comparacion_iris <- data.frame(
  Modelo = c("Original", "Seleccionado por validacion"),
  Exactitud = c(
    mean(prediccion == prueba$Species),
    mean(prediccion_final == prueba$Species)
  ) * 100
)

print(comparacion_iris)
rpart.plot(modelo_final, main = "Iris: arbol seleccionado")

# Conclusion:
# Los ajustes probados mantuvieron las mismas reglas.
# La exactitud de prueba fue 91,67% en ambos modelos.


# ============================================================
# EJERCICIO 2: DATASET DE KAGGLE
# ============================================================

# Seleccionar WA_Fn-UseC_-Telco-Customer-Churn.csv
clientes <- read.csv(
  "WA_Fn-UseC_-Telco-Customer-Churn.csv",
  stringsAsFactors = FALSE
)

# Explorar los datos
dim(clientes)
names(clientes)
str(clientes)
table(clientes$Churn)
prop.table(table(clientes$Churn)) * 100
colSums(is.na(clientes))

# Objetivo: predecir Churn (Yes = baja, No = permanencia).
# Datos originales: 7043 filas y 21 columnas.
# TotalCharges tiene 11 valores faltantes.
# Las clases estan desbalanceadas: aproximadamente 73% No y 27% Yes.

# Preparacion desde R
clientes_limpios <- clientes

# Excluir el identificador de cliente
clientes_limpios$customerID <- NULL

# Asegurar que TotalCharges sea numerica
clientes_limpios$TotalCharges <- as.numeric(
  trimws(as.character(clientes_limpios$TotalCharges))
)

# Eliminar filas sin TotalCharges: 11 en el archivo utilizado
clientes_limpios <- clientes_limpios[
  !is.na(clientes_limpios$TotalCharges),
]

# Convertir texto en variables categoricas
columnas_texto <- sapply(clientes_limpios, is.character)

clientes_limpios[columnas_texto] <- lapply(
  clientes_limpios[columnas_texto],
  as.factor
)

clientes_limpios$SeniorCitizen <- factor(
  clientes_limpios$SeniorCitizen
)

# Comprobar la limpieza
dim(clientes_limpios)
sum(is.na(clientes_limpios))
table(clientes_limpios$Churn)

# Separar entrenamiento y prueba, conservando proporciones de Churn
set.seed(42)

division_clientes <- sample.split(
  clientes_limpios$Churn,
  SplitRatio = 0.75
)

entrenamiento_clientes <- clientes_limpios[division_clientes, ]
prueba_clientes <- clientes_limpios[!division_clientes, ]

prop.table(table(entrenamiento_clientes$Churn)) * 100
prop.table(table(prueba_clientes$Churn)) * 100

# Modelo original
set.seed(42)

arbol_clientes <- rpart(
  Churn ~ .,
  data = entrenamiento_clientes,
  method = "class"
)

print(arbol_clientes)
rpart.plot(arbol_clientes, main = "Clientes: arbol original")

pred_clientes <- predict(
  arbol_clientes,
  newdata = prueba_clientes,
  type = "class"
)

matriz_clientes <- table(
  Prediccion = pred_clientes,
  Real = prueba_clientes$Churn
)

print(matriz_clientes)

# Exactitud
mean(pred_clientes == prueba_clientes$Churn) * 100

# Sensibilidad: proporcion de bajas reales detectadas
matriz_clientes["Yes", "Yes"] /
  sum(matriz_clientes[, "Yes"]) * 100

# Referencia: predecir siempre la clase mayoritaria
mean(prueba_clientes$Churn == "No") * 100

# Resultados observados:
# Exactitud: 80,03%.
# Sensibilidad para bajas: 49,04%.
# Referencia de clase mayoritaria: 73,44%.


# ============================================================
# EJERCICIO 3, PARTE B: AJUSTE DEL MODELO DE CLIENTES
# ============================================================

set.seed(42)

arbol_clientes_amplio <- rpart(
  Churn ~ .,
  data = entrenamiento_clientes,
  method = "class",
  control = rpart.control(
    cp = 0.001,
    minsplit = 20,
    minbucket = 7,
    maxdepth = 10,
    xval = 10
  )
)

printcp(arbol_clientes_amplio)

# Seleccionar cp mediante validacion cruzada
mejor_cp_clientes <- arbol_clientes_amplio$cptable[
  which.min(arbol_clientes_amplio$cptable[, "xerror"]),
  "CP"
]

arbol_clientes_final <- prune(
  arbol_clientes_amplio,
  cp = mejor_cp_clientes
)

pred_clientes_final <- predict(
  arbol_clientes_final,
  newdata = prueba_clientes,
  type = "class"
)

matriz_final <- table(
  Prediccion = pred_clientes_final,
  Real = prueba_clientes$Churn
)

print(matriz_final)

comparacion_clientes <- data.frame(
  Modelo = c("Original", "Ajustado"),
  Exactitud = c(
    mean(pred_clientes == prueba_clientes$Churn),
    mean(pred_clientes_final == prueba_clientes$Churn)
  ) * 100,
  Sensibilidad_bajas = c(
    matriz_clientes["Yes", "Yes"] / sum(matriz_clientes[, "Yes"]),
    matriz_final["Yes", "Yes"] / sum(matriz_final[, "Yes"])
  ) * 100
)

print(comparacion_clientes)
rpart.plot(arbol_clientes_final, main = "Clientes: arbol ajustado")

# Conclusion:
# La exactitud paso de 80,03% a 80,77%.
# La sensibilidad para bajas paso de 49,04% a 58,89%.
# Las falsas alarmas aumentaron de 113 a 146.
# Los parametros se seleccionaron dentro del entrenamiento.
# La prueba se utilizo para comparar resultados, no para elegir cp.


# ============================================================
# EJERCICIO 4: ARBOLES CON GRUPOS DE VARIABLES
# ============================================================

# Dividir columnas, conservando Churn en cada grupo.
# Usar las mismas filas de entrenamiento y prueba en todos los modelos.

vars_perfil <- c(
  "gender", "SeniorCitizen", "Partner", "Dependents", "Churn"
)

vars_servicios <- c(
  "PhoneService", "MultipleLines", "InternetService",
  "OnlineSecurity", "OnlineBackup", "DeviceProtection",
  "TechSupport", "StreamingTV", "StreamingMovies", "Churn"
)

vars_cuenta <- c(
  "tenure", "Contract", "PaperlessBilling",
  "PaymentMethod", "MonthlyCharges", "TotalCharges", "Churn"
)

# Entrenar con parametros predeterminados en los tres grupos
set.seed(42)

arbol_perfil <- rpart(
  Churn ~ .,
  data = entrenamiento_clientes[, vars_perfil],
  method = "class"
)

arbol_servicios <- rpart(
  Churn ~ .,
  data = entrenamiento_clientes[, vars_servicios],
  method = "class"
)

arbol_cuenta <- rpart(
  Churn ~ .,
  data = entrenamiento_clientes[, vars_cuenta],
  method = "class"
)

print(arbol_perfil)
print(arbol_servicios)
print(arbol_cuenta)

rpart.plot(arbol_perfil, main = "Grupo: perfil")
rpart.plot(arbol_servicios, main = "Grupo: servicios")
rpart.plot(arbol_cuenta, main = "Grupo: cuenta")

# Predecir en prueba
pred_perfil <- predict(
  arbol_perfil,
  newdata = prueba_clientes,
  type = "class"
)

pred_servicios <- predict(
  arbol_servicios,
  newdata = prueba_clientes,
  type = "class"
)

pred_cuenta <- predict(
  arbol_cuenta,
  newdata = prueba_clientes,
  type = "class"
)

# Matrices de confusion
table(Prediccion = pred_perfil, Real = prueba_clientes$Churn)
table(Prediccion = pred_servicios, Real = prueba_clientes$Churn)
table(Prediccion = pred_cuenta, Real = prueba_clientes$Churn)

# Comparar indicadores
bajas_reales <- prueba_clientes$Churn == "Yes"

comparacion_grupos <- data.frame(
  Grupo = c("Perfil", "Servicios", "Cuenta"),
  Exactitud = c(
    mean(pred_perfil == prueba_clientes$Churn),
    mean(pred_servicios == prueba_clientes$Churn),
    mean(pred_cuenta == prueba_clientes$Churn)
  ) * 100,
  Sensibilidad_bajas = c(
    mean(pred_perfil[bajas_reales] == "Yes"),
    mean(pred_servicios[bajas_reales] == "Yes"),
    mean(pred_cuenta[bajas_reales] == "Yes")
  ) * 100
)

print(comparacion_grupos)

# Resultados observados:
# Perfil:    exactitud 73,44%; sensibilidad  0,00%.
# Servicios: exactitud 77,36%; sensibilidad 39,83%.
# Cuenta:    exactitud 79,35%; sensibilidad 51,18%.

# Conclusion:
# Cuenta obtuvo los mejores resultados entre los grupos probados.
# Perfil no genero divisiones y predijo No para todos los clientes.
# La exactitud debe analizarse junto con la sensibilidad debido
# al desbalance de clases.
# El modelo ajustado con todas las variables obtuvo los mejores
# valores observados de exactitud y sensibilidad.
# Esta comparacion corresponde a esta division de los datos
# y a los parametros utilizados; no demuestra causalidad.
