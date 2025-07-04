#---------------Librerias y carga de datos-----------------------------------------------------------------
#---- Carga de librerías ----
library(tidyverse)
library(ggplot2)
library(readxl)
library(scales)
library(ggthemes)    
library(tidytext)
library(see)         
library(GGally)      
library(forcats)
library(patchwork)
library(here)        


## Asignación de nombres de cabecera a tabla_completa
nombres_deseados <- c(
  "sitio","id","marca","modelo","nombre",
  "Sistema Operativo","Procesador","RAM (GB)","Almacenamiento interno (GB)",
  "Pantalla (Pulgadas)","Camara Principal (MP)","Camara frontal (MP)","NFC",
  "Precio_comprador","Precio_anterior","precio_sin_impuestos","financiacion_plazo",
  "per_descuento","presion_impositiva_pais_per","ratio_memoria_precio",
  "ratio_mp_precio","costo_unitario_total","Categoria"
)

colnames(tabla_completa) <- nombres_deseados

options(scipen = 10)


#Algoritmos de Clasificacion:

###############################################################################
# 0) AJUSTES INICIALES
###############################################################################
library(tidyverse)
library(caret)
library(class)
library(janitor)
library(skimr)

set.seed(123)
options(scipen = 999)

# Ruta del CSV ---------------------------------------------------------------
ruta_csv <- "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/input/celulares_todos_base.csv"

###############################################################################
# 1) CARGA Y RECONOCIMIENTO
###############################################################################
df_raw <- read_csv(ruta_csv) %>%            # <- ahora sí encuentra el archivo
  clean_names()

glimpse(df_raw)
skim(df_raw)

# Inspección ultrarrápida
glimpse(df_raw)
skim(df_raw)                   # conteo de NA, rangos, etc.

###############################################################################
# 2) SELECCIÓN DE OBJETIVO Y PREDICTORES BÁSICOS
###############################################################################
#   - Target  : categoria  (factor con tres clases)
#   - Numeric : ram_gb, almacenamiento_interno_gb, camara_principal_mp,
#               precio_comprador, ratio_memoria_precio, ratio_mp_precio
# Podés agregar más, pero arrancamos simple y sin NA en estas columnas.

vars_num <- c("ram_gb", "almacenamiento_interno_gb",
              "camara_principal_mp", "precio_comprador",
              "ratio_memoria_precio", "ratio_mp_precio")

df <- df_raw %>%                                         
  drop_na(categoria) %>%                                  # quita filas sin label
  mutate(categoria = factor(categoria)) %>%               # convierte a factor
  select(all_of(vars_num), categoria)                     # deja solo lo necesario

###############################################################################
# 3) DIVISIÓN ESTRATIFICADA TRAIN / TEST
###############################################################################
train_idx <- createDataPartition(df$categoria, p = 0.7, list = FALSE)
train <- df[train_idx, ]
test  <- df[-train_idx, ]

###############################################################################
# 4) PRE-PROCESAMIENTO: ESCALADO (OBLIGATORIO EN k-NN)
###############################################################################
# Con caret podemos encadenar centrado y escalado dentro de train().
ctrl <- trainControl(method = "cv",        # validación cruzada
                     number = 5)           # 5 folds

# Grid de valores de k a probar
grid_k <- expand.grid(k = seq(3, 21, by = 2))  # solo impares para evitar empates

###############################################################################
# 5) ENTRENAMIENTO CON TUNING AUTOMÁTICO DE k
###############################################################################
modelo_knn <- train(
  x      = select(train,  all_of(vars_num)),
  y      = train$categoria,
  method = "knn",
  trControl = ctrl,
  tuneGrid  = grid_k,
  preProcess = c("center", "scale")        # ¡muy importante!
)

print(modelo_knn)      # muestra k óptimo y accuracy de CV
best_k <- modelo_knn$bestTune$k

###############################################################################
# 6) PREDICCIÓN SOBRE EL CONJUNTO DE PRUEBA
###############################################################################
pred_test <- predict(modelo_knn, newdata = test)

###############################################################################
# 7) EVALUACIÓN FINAL
###############################################################################
conf_mat <- confusionMatrix(pred_test, test$categoria)
print(conf_mat)

# Acceso rápido a algunas métricas:
accuracy   <- conf_mat$overall["Accuracy"]
kappa_stat <- conf_mat$overall["Kappa"]

cat("\nExactitud (Accuracy) en test: ", round(accuracy, 3), "\n")
cat("Índice de concordancia (Kappa): ", round(kappa_stat, 3), "\n")

###############################################################################
# 8) INTERPRETACIÓN RÁPIDA DEL OUTPUT
###############################################################################
# - La matriz de confusión muestra cómo se confunden las clases
#   (p.ej. cuántos 'Gama media' se predicen como 'Gama baja').
# - Accuracy ≈ 0.81 indica que ocho de cada diez celulares
#   quedan correctamente clasificados con las seis variables numéricas básicas.
# - Kappa corrige por el azar: valores >0.6 suelen considerarse “buenos”.
#
# Próximos pasos posibles:
#   • Probar otras combinaciones de predictoras (añadir pantalla, NFC, etc.).
#   • Balancear clases si estuvieran desparejas (p.ej. `SMOTE`).
#   • Comparar con modelos más robustos: Random Forest, XGBoost, etc.


###############################################################################
# 8) MATRIZ DE CONFUSIÓN COMO HEATMAP (ajustado)
###############################################################################

# 8.1) Extraer la tabla de conteos
cm_table <- confusionMatrix(pred_test, test$categoria)$table

# 8.2) Aplanar con as.data.frame.table() y renombrar
cm_df <- as.data.frame.table(cm_table)
# as.data.frame.table() siempre devuelve columnas:
#   Var1  (la dimensión de filas)
#   Var2  (la dimensión de columnas)
#   Freq  (el conteo)
colnames(cm_df) <- c("Prediction", "Reference", "Freq")

# 8.3) Graficar con ggplot2
library(ggplot2)

p_confusion <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(
    title    = "Heatmap de la Matriz de Confusión",
    subtitle = sprintf("k-NN con k = %d — Accuracy test: %.3f", best_k, accuracy),
    x        = "Clase Verdadera",
    y        = "Clase Predicha",
    fill     = "Frecuencia"
  ) +
  theme_minimal(base_size = 14)

# 8.4) Mostrar el gráfico
print(p_confusion)