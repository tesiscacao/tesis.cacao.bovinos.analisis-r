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
# ALB
################################################################################

# Filtrar datos para ALB y convertir a numérico si es necesario
alb_data <- datos %>%
  filter(Biomarker == "ALB") %>%
  mutate(Valor = as.numeric(Valor))

# Crear gráfico de cajas para ALB
ggplot(alb_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 25.0, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 36.0, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro ALB por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de ALB (g/L)",
       fill = "Tiempo",
       caption = "Línea azul discontinua: Límite inferior (25.0 g/L)\nLínea verde discontinua: Límite superior (36.0 g/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# TP
################################################################################

# Filtrar los datos para el parámetro TP y convertir "Valor" a numérico
tp_data <- datos %>%
  filter(Biomarker == "TP") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para TP
ggplot(tp_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 58.0, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 80.0, linetype = "dashed", color = "BLUE", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro TP por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de TP (g/L)",
       fill = "Tiempo",
       caption = "Línea azul discontinua: Límite inferior (58.0 g/L)\nLínea verde discontinua: Límite superior (80.0 g/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# GLOB
################################################################################

# Filtrar los datos para el parámetro GLOB y convertir "Valor" a numérico
glob_data <- datos %>%
  filter(Biomarker == "GLOB") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para GLOB
ggplot(glob_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 27.0, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 38.0, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro GLOB por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de GLOB (g/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (27.0 g/L)\nLínea azul discontinua: Límite superior (38.0 g/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# A/G
################################################################################

# Filtrar los datos para el parámetro GLOB y convertir "Valor" a numérico
ag_data <- datos %>%
  filter(Biomarker == "A/G") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para A/G con ajustes al eje y
ggplot(ag_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  scale_y_continuous(limits = c(0.5, 1)) +  # Ajustar los límites del eje y
  labs(title = "Gráfico de cajas del parámetro A/G por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Razón A/G",
       fill = "Tiempo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# TB
################################################################################

# Filtrar los datos para el parámetro TB y convertir "Valor" a numérico
tb_data <- datos %>%
  filter(Biomarker == "TB") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para TB
ggplot(tb_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 0.0, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 12.0, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro TB por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de TB (umol/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (0.0 umol/L)\nLínea azul discontinua: Límite superior (12.0 umol/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# AST
################################################################################

# Filtrar los datos para el parámetro AST y convertir "Valor" a numérico
ast_data <- datos %>%
  filter(Biomarker == "AST") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para AST
ggplot(ast_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 0.0, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 91.0, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro AST por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de AST (U/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (0.0 U/L)\nLínea azul discontinua: Límite superior (91.0 U/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# ALT
################################################################################

# Filtrar los datos para el parámetro ALT y convertir "Valor" a numérico
alt_data <- datos %>%
  filter(Biomarker == "ALT") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para ALT
ggplot(alt_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Gráfico de cajas del parámetro ALT por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de ALT (U/L)",
       fill = "Tiempo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# AMY
################################################################################

# Filtrar los datos para el parámetro AMY y convertir "Valor" a numérico
amy_data <- datos %>%
  filter(Biomarker == "AMY") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para AMY
ggplot(amy_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 0.0, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 28.0, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro AMY por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de AMY (U/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (0.0 U/L)\nLínea azul discontinua: Límite superior (28.0 U/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# CK
################################################################################

# Filtrar los datos para el parámetro CK y convertir "Valor" a numérico
ck_data <- datos %>%
  filter(Biomarker == "CK") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para CK
ggplot(ck_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 0.0, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 110.0, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro CK por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de CK (U/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (0.0 U/L)\nLínea azul discontinua: Límite superior (110.0 U/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# Crea
################################################################################

# Filtrar los datos para el parámetro Crea y convertir "Valor" a numérico
crea_data <- datos %>%
  filter(Biomarker == "Crea") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para Crea
ggplot(crea_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 44.0, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 159.0, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro Crea por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de Crea (µmol/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (44.0 µmol/L)\nLínea azul discontinua: Límite superior (159.0 µmol/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# BUN
################################################################################

# Filtrar los datos para el parámetro BUN y convertir "Valor" a numérico
bun_data <- datos %>%
  filter(Biomarker == "BUN") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para BUN
ggplot(bun_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 2.14, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 6.10, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro BUN por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de BUN (mmol/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (2.14 mmol/L)\nLínea azul discontinua: Límite superior (6.10 mmol/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# BUN/CREA
################################################################################

# Filtrar los datos para el parámetro BUN/CREA y convertir "Valor" a numérico
bun_crea_data <- datos %>%
  filter(Biomarker == "BUN/CREA") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para BUN/CREA
ggplot(bun_crea_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Gráfico de cajas del parámetro BUN/CREA por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Relación BUN/CREA",
       fill = "Tiempo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# GLU
################################################################################

# Filtrar los datos para el parámetro GLU y convertir "Valor" a numérico
glu_data <- datos %>%
  filter(Biomarker == "GLU") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para GLU
ggplot(glu_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 2.56, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 6.13, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro GLU por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de GLU (mmol/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (2.56 mmol/L)\nLínea azul discontinua: Límite superior (6.13 mmol/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# TG
################################################################################

# Filtrar los datos para el parámetro TG y convertir "Valor" a numérico
tg_data <- datos %>%
  filter(Biomarker == "TG") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para TG
ggplot(tg_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Gráfico de cajas del parámetro TG por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de TG (mmol/L)",
       fill = "Tiempo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# Ca
################################################################################

# Filtrar los datos para el parámetro Ca y convertir "Valor" a numérico
ca_data <- datos %>%
  filter(Biomarker == "Ca") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para Ca
ggplot(ca_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 1.95, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 3.10, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro Ca por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de Ca (mmol/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (1.95 mmol/L)\nLínea azul discontinua: Límite superior (3.10 mmol/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
# PHOS
################################################################################

# Filtrar los datos para el parámetro PHOS y convertir "Valor" a numérico
phos_data <- datos %>%
  filter(Biomarker == "PHOS") %>%
  mutate(Valor = as.numeric(Valor))

# Crear el gráfico de cajas para PHOS
ggplot(phos_data, aes(x = Tratamiento, y = Valor, fill = Tiempo)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_hline(yintercept = 1.38, linetype = "dashed", color = "red", size = 1) + # Línea para el límite inferior
  geom_hline(yintercept = 2.55, linetype = "dashed", color = "blue", size = 1) + # Línea para el límite superior
  labs(title = "Gráfico de cajas del parámetro PHOS por tratamiento y tiempo",
       x = "Tratamiento",
       y = "Valor de PHOS (mmol/L)",
       fill = "Tiempo",
       caption = "Línea roja discontinua: Límite inferior (1.38 mmol/L)\nLínea azul discontinua: Límite superior (2.55 mmol/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
# Composición Bromatológica de la Cáscara de Cacao
################################################################################

# Crear un dataframe con los datos bromatológicos
bromatologia_cacao <- data.frame(
  Parametro = c("Proteína", "Humedad", "Grasa", "Ceniza", "Carbohidratos Totales", "Fibra Bruta"),
  Porcentaje = c(0.66, 84.49, 0.04, 1.61, 13.20, 4.98)
)

# Crear el gráfico de barras horizontales con etiquetas en las barras
ggplot(bromatologia_cacao, aes(x = Porcentaje, y = reorder(Parametro, Porcentaje))) +
  geom_bar(stat = "identity", fill = "saddlebrown", width = 0.7) +
  geom_text(aes(label = Porcentaje), hjust = -0.1, size = 4) +  # Agregar etiquetas a cada barra
  labs(title = "Composición Bromatológica de la Cáscara de Cacao",
       x = NULL,
       y = "Parámetro") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Quitar números del eje X
        axis.ticks.x = element_blank(), # Quitar las marcas del eje X
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  coord_cartesian(clip = "off")  # Permite que las etiquetas sobresalgan del área de trazado

################################################################################
# Composición Bromatológica del Banano verde
################################################################################

# Crear un dataframe con los datos bromatológicos del banano
bromatologia_banano <- data.frame(
  Parametro = c("MS", "Agua", "Cenizas", "E.E", "P.C", "F.C", "E.L.N"),
  Porcentaje = c(18.23, 83.11, 4.93, 1.05, 4.87, 2.18, 86.97)
)

# Crear el gráfico de barras horizontales con etiquetas en las barras
ggplot(bromatologia_banano, aes(x = Porcentaje, y = reorder(Parametro, Porcentaje))) +
  geom_bar(stat = "identity", fill = "forestgreen", width = 0.7) +
  geom_text(aes(label = Porcentaje), hjust = -0.1, size = 4) +  # Agregar etiquetas a cada barra
  labs(title = "Composición Bromatológica del Banano verde",
       x = NULL,
       y = "Parámetro") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Quitar números del eje X
        axis.ticks.x = element_blank(), # Quitar las marcas del eje X
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  coord_cartesian(clip = "off")  # Permite que las etiquetas sobresalgan del área de trazado

################################################################################
# 
################################################################################