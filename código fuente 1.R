# Comentarios
# Ctrl + Shift + C

# Instalar el paquete dplyr si no está instalado
if (!require(dplyr)) install.packages("dplyr")

# Cargar el paquete dplyr
library(dplyr)

# Establecer el directorio de trabajo donde se encuentra el archivo
setwd("C:/Users/Usuario/Desktop/_TESIS DAVID/ANALISIS TESIS R/data")

# Leer el archivo CSV usando read.csv2 que automáticamente usa ';' como separador
datos <- read.csv2("dataset_tesis.csv", stringsAsFactors = FALSE)

# Verificar las primeras filas del dataset
cat("Primeras filas del dataset:\n")
head(datos)

# Opcional: Verificar la estructura del dataset
cat("\nEstructura del dataset:\n")
str(datos)

# Resumen estadístico básico del dataset
cat("\nResumen estadístico:\n")
summary(datos)

# Librería para los gráficos
library(ggplot2)

################################################################################
# Preparar los datos para análisis
################################################################################

# Asegúrate de que la columna Ganan_peso sea numérica y que las categorías 
# como Tratamiento estén definidas como factores:
  
# Convertir columnas necesarias a factores o numéricas
datos <- datos %>%
  mutate(
    Tratamiento = as.factor(Tratamiento),
    Ganan_peso = as.numeric(Ganan_peso)
  )

# Verificar estructura
str(datos)

################################################################################
# Análisis descriptivo
################################################################################

# Obten un resumen general de la ganancia de peso por tratamiento:

# Resumen general por tratamiento
datos %>%
  group_by(Tratamiento) %>%
  summarize(
    Promedio_Ganancia = mean(Ganan_peso, na.rm = TRUE),
    SD_Ganancia = sd(Ganan_peso, na.rm = TRUE),
    Min_Ganancia = min(Ganan_peso, na.rm = TRUE),
    Max_Ganancia = max(Ganan_peso, na.rm = TRUE)
  )

################################################################################
# Visualización
################################################################################

# Crear un boxplot para visualizar la distribución de la ganancia de peso por
# tratamiento:

# El boxplot ayuda a visualizar si hay diferencias claras entre tratamientos.

# Instalar el paquete dplyr si no está instalado
if (!require(plotly)) install.packages("plotly")

library(plotly)

# Crear un gráfico de caja interactivo con plotly
fig <- plot_ly(
  data = datos,
  x = ~Tratamiento,
  y = ~Ganan_peso,
  type = "box",
  color = ~Tratamiento
)

# Añadir título y etiquetas
fig <- fig %>%
  layout(
    title = "Ganancia de peso por tratamiento",
    xaxis = list(title = "Tratamiento"),
    yaxis = list(title = "Ganancia de peso (kg)")
  )

# Mostrar el gráfico
fig

################################################################################

# Gráfico de caja básico con base R
boxplot(
  Ganan_peso ~ Tratamiento,
  data = datos,
  col = c("forestgreen", "saddlebrown"),
  main = "Ganancia de peso por tratamiento",
  xlab = "Tratamiento",
  ylab = "Ganancia de peso (kg)"
)

################################################################################
# Prueba estadística
################################################################################

# Para comparar las ganancias de peso entre los dos tratamientos (Mazorca de
# cacao y Banano verde), se puede usar una prueba t-test si los datos cumplen
# con las suposiciones, o una prueba no paramétrica si no las cumplen.

# Las pruebas estadísticas (t-test o Wilcoxon) indican si las diferencias son
# significativas

# Prueba t para comparar tratamientos
t.test(Ganan_peso ~ Tratamiento, data = datos)

# Prueba no paramétrica para comparar tratamientos
# Si los datos no son normales o las varianzas no son iguales:
wilcox.test(Ganan_peso ~ Tratamiento, data = datos)

# Pruebas de Homogeneidad de Varianzas
# Confirma si las varianzas entre grupos son iguales para validar pruebas
# paramétricas como ANOVA.
bartlett.test(Ganan_peso ~ Tratamiento, data = datos)

# Puedes analizar si existe una diferencia significativa en la ganancia de peso
# considerando otros factores, como el sexo o el tiempo:
# ANOVA para varios factores (si aplican)
anova_model <- aov(Ganan_peso ~ Tratamiento + Sexo, data = datos)
summary(anova_model)

################################################################################
# Matrices de Correlación
################################################################################

# Instalar los paquetes necesarios si no están instalados
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(ggcorrplot)) install.packages("ggcorrplot")

# Cargar los paquetes necesarios
library(dplyr)
library(tidyr)
library(ggcorrplot)

# Establecer el directorio de trabajo donde se encuentra el archivo
setwd("C:/Users/Usuario/Desktop/_TESIS DAVID/ANALISIS TESIS R/data")

# Leer el archivo CSV usando read.csv2 que automáticamente usa ';' como separador
datos <- read.csv2("dataset_correlacion.csv", stringsAsFactors = FALSE)

# Convertir las columnas relevantes a tipo numérico
datos <- datos %>%
  mutate(
    Ganan_peso = as.numeric(Ganan_peso),
    Valor = as.numeric(Valor)
  )

# Dividir los datos en los 4 subconjuntos: cacao inicial, cacao final, banano inicial, banano final
cacao_inicial <- datos %>%
  filter(Tratamiento == "Mazorca de cacao") %>%
  select(contains("_i"), Ganan_peso)

cacao_final <- datos %>%
  filter(Tratamiento == "Mazorca de cacao") %>%
  select(contains("_f"), Ganan_peso)

banano_inicial <- datos %>%
  filter(Tratamiento == "Banano verde") %>%
  select(contains("_i"), Ganan_peso)

banano_final <- datos %>%
  filter(Tratamiento == "Banano verde") %>%
  select(contains("_f"), Ganan_peso)

# Convertir todas las columnas de cada subconjunto a numéricas
cacao_inicial <- cacao_inicial %>%
  mutate(across(everything(), as.numeric))

cacao_final <- cacao_final %>%
  mutate(across(everything(), as.numeric))

banano_inicial <- banano_inicial %>%
  mutate(across(everything(), as.numeric))

banano_final <- banano_final %>%
  mutate(across(everything(), as.numeric))

# Función para calcular y visualizar matrices de correlación
mostrar_matriz_correlacion <- function(data, titulo) {
  corr_matrix <- cor(data, use = "complete.obs")  # Calcular la matriz de correlación
  print(paste("Matriz de correlación:", titulo))
  print(corr_matrix)  # Mostrar la matriz
  ggcorrplot(corr_matrix, title = titulo, lab = TRUE, method = "square", hc.order = TRUE)  # Visualizar la matriz
}

# Calcular y mostrar las matrices de correlación
mostrar_matriz_correlacion(cacao_inicial, "Dieta Mazorca de cacao Inicial")
mostrar_matriz_correlacion(cacao_final, "Dieta Mazorca de cacao Final")
mostrar_matriz_correlacion(banano_inicial, "Dieta Banano verde Inicial")
mostrar_matriz_correlacion(banano_final, "Dieta Banano verde Final")


