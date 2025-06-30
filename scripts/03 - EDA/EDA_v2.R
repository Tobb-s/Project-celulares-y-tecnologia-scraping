#---------------Librerias y carga de datos (setear directorio en carpeta imput)-----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(readxl)
library(scales)
library(ggthemes)  ##Paquete R que aporta paletas “económicas” (The Economist), “excel” y más
library(tidytext)
library(see)       ##Paletas inspiradas en Material Design de Google
library(GGally)    ## Para matriz de correlación

# --- NOTA: setwd() es específico para tu máquina. Otros usuarios necesitarán cambiar esta línea.
setwd("C:/Users/valentino.didomenica/Desktop/Project-celulares-y-tecnologia-scraping-main/input")

# --- Carga de datos (asumiendo que los archivos están en el directorio de trabajo)
# Descomentar las siguientes líneas si los archivos no se cargan desde otra parte del script
# tabla_claro<-read.csv('claro_celulares_base.csv')
# tabla_personal<-read.csv('personal_celulares_base.csv')
# tabla_centrogar<-read.csv('cetrogar_celulares_base.csv')
tabla_completa<-read.csv('celulares_todos_base.csv')

## Asignación de nombres de cabecera a tabla_completa
nombres_deseados <- c(
  "sitio","id","marca","modelo","nombre",
  "Sistema Operativo","Procesador","RAM (GB)","Almacenamiento interno (GB)",
  "Pantalla (Pulgadas)","Camara Principal (MP)","Camara frontal (MP)","NFC",
  "Precio_comprador","Precio_anterior","precio_sin_impuestos","financiacion_plazo",
  "per_descuento","presion_impositiva_pais_per","ratio_memoria_precio",
  "ratio_mp_precio","Categoria"
)

colnames(tabla_completa) <- nombres_deseados

options(scipen = 10)
#-------------------------------------------------------------Paletas de Colores--------------------------------------------
#Otras paletas de ggthemes
#scale_fill_tableau(palette="Classic 10")
#scale_fill_solarized()
#scale_fill_economist()
#scale_fill_colorblind()

##De Google
#scale_fill_material_d() / scale_color_material_d() para datos discretos
#scale_fill_material_c() / scale_color_material_c() para datos continuos

#================================================================EDA UNIVARIABLE===========================================================

## Medidas de Tendencia central::-----------------------------------------------------------------------------------------------------

# Columnas numéricas a analizar
numeric_cols <- c(
  "Precio_comprador",
  "per_descuento",
  "RAM (GB)",
  "Almacenamiento interno (GB)",
  "Camara Principal (MP)",
  "Camara frontal (MP)"
)

# 2. Limpiamos tabla_completa: convertimos a numérico y filtramos filas con NA en estas columnas
# Nota: La advertencia sobre `across()` en `filter()` es de dplyr y puede ignorarse o actualizarse a if_all().
tabla_limpia <- tabla_completa %>%
  mutate(across(all_of(numeric_cols), as.numeric)) %>%
  filter(if_all(all_of(numeric_cols), ~ !is.na(.)))

# 1. Función para Moda
Mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 2. Cálculo de estadísticas univariantes
eda <- tabla_limpia %>%
  summarise(
    # Precio_comprador
    media_Precio_comprador    = mean(`Precio_comprador`, na.rm = TRUE),
    mediana_Precio_comprador  = median(`Precio_comprador`, na.rm = TRUE),
    moda_Precio_comprador     = Mode(`Precio_comprador`),
    min_Precio_comprador      = min(`Precio_comprador`, na.rm = TRUE),
    max_Precio_comprador      = max(`Precio_comprador`, na.rm = TRUE),
    midrange_Precio_comprador = (min_Precio_comprador + max_Precio_comprador) / 2,
    
    # RAM (GB)
    media_RAM                 = mean(`RAM (GB)`, na.rm = TRUE),
    mediana_RAM               = median(`RAM (GB)`, na.rm = TRUE),
    moda_RAM                  = Mode(`RAM (GB)`),
    min_RAM                   = min(`RAM (GB)`, na.rm = TRUE),
    max_RAM                   = max(`RAM (GB)`, na.rm = TRUE),
    midrange_RAM              = (min_RAM + max_RAM) / 2,
    
    # Almacenamiento interno (GB)
    media_Almacenamiento      = mean(`Almacenamiento interno (GB)`, na.rm = TRUE),
    mediana_Almacenamiento    = median(`Almacenamiento interno (GB)`, na.rm = TRUE),
    moda_Almacenamiento       = Mode(`Almacenamiento interno (GB)`),
    min_Almacenamiento        = min(`Almacenamiento interno (GB)`, na.rm = TRUE),
    max_Almacenamiento        = max(`Almacenamiento interno (GB)`, na.rm = TRUE),
    midrange_Almacenamiento   = (min_Almacenamiento + max_Almacenamiento) / 2,
    
    # Cámara Principal (MP)
    media_Camara_Principal    = mean(`Camara Principal (MP)`, na.rm = TRUE),
    mediana_Camara_Principal  = median(`Camara Principal (MP)`, na.rm = TRUE),
    moda_Camara_Principal     = Mode(`Camara Principal (MP)`),
    min_Camara_Principal      = min(`Camara Principal (MP)`, na.rm = TRUE),
    max_Camara_Principal      = max(`Camara Principal (MP)`, na.rm = TRUE),
    midrange_Camara_Principal = (min_Camara_Principal + max_Camara_Principal) / 2,
    
    # Cámara frontal (MP)
    media_Camara_frontal      = mean(`Camara frontal (MP)`, na.rm = TRUE),
    mediana_Camara_frontal    = median(`Camara frontal (MP)`, na.rm = TRUE),
    moda_Camara_frontal       = Mode(`Camara frontal (MP)`),
    min_Camara_frontal        = min(`Camara frontal (MP)`, na.rm = TRUE),
    max_Camara_frontal        = max(`Camara frontal (MP)`, na.rm = TRUE),
    midrange_Camara_frontal   = (min_Camara_frontal + max_Camara_frontal) / 2,
    
    # per_descuento
    media_per_descuento       = mean(`per_descuento`, na.rm = TRUE),
    mediana_per_descuento     = median(`per_descuento`, na.rm = TRUE),
    moda_per_descuento        = Mode(`per_descuento`),
    min_per_descuento         = min(`per_descuento`, na.rm = TRUE),
    max_per_descuento         = max(`per_descuento`, na.rm = TRUE),
    midrange_per_descuento    = (min_per_descuento + max_per_descuento) / 2
  )

# 3. Pivotar para tabla tidy y redondear
eda_tabla <- eda %>%
  pivot_longer(
    everything(),
    names_to      = c("estadística", "variable"),
    names_pattern = "^(media|mediana|moda|min|max|midrange)_(.*)$"
  ) %>%
  pivot_wider(
    names_from  = estadística,
    values_from = value
  ) %>%
  select(variable, media, mediana, moda, min, max, midrange) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

View(eda_tabla)


## Medidas de Dispersion::-----------------------------------------------------------------------------------------------------

dispersion_longa <- tabla_limpia %>%
  summarise(
    across(
      all_of(numeric_cols),
      list(
        rango = ~ diff(range(.x, na.rm = TRUE)),
        sd    = ~ sd(.x,    na.rm = TRUE),
        var   = ~ var(.x,   na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = c("variable", "estadistica"),
    names_sep     = "__",
    values_to     = "valor"
  ) %>%
  pivot_wider(
    names_from  = estadistica,
    values_from = valor
  ) %>%
  mutate(variable = recode(variable,
                           "Precio_comprador"            = "Precio",
                           "per_descuento"               = "% Descuento",
                           "RAM (GB)"                    = "RAM (GB)",
                           "Almacenamiento interno (GB)" = "Alm. (GB)",
                           "Camara Principal (MP)"       = "Cámara Ppal (MP)",
                           "Camara frontal (MP)"         = "Cámara Frontal (MP)"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
  select(variable, rango, sd, var)

View(dispersion_longa)


## Medidas Basadas en Cuantiles (medidas mas robustas)-------------------------------------------------
robust_longa <- tabla_limpia %>%
  summarise(
    across(
      all_of(numeric_cols),
      list(
        IQR     = ~ IQR(.x, na.rm = TRUE),
        SemiIQR = ~ IQR(.x, na.rm = TRUE) / 2,
        P90P10  = ~ diff(quantile(.x, c(0.1, 0.9), na.rm = TRUE))
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(
    cols         = everything(),
    names_to     = c("variable", "medida"),
    names_sep    = "__",
    values_to    = "valor"
  ) %>%
  pivot_wider(
    names_from  = medida,
    values_from = valor
  ) %>%
  mutate(variable = recode(variable,
                           "Precio_comprador"            = "Precio",
                           "per_descuento"               = "% Desc.",
                           "RAM (GB)"                    = "RAM (GB)",
                           "Almacenamiento interno (GB)" = "Alm. (GB)",
                           "Camara Principal (MP)"       = "Cámara Ppal (MP)",
                           "Camara frontal (MP)"         = "Cámara Frontal (MP)"
  )) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
  select(variable, IQR, Semi_IQR = SemiIQR, P90_10 = P90P10)


View(robust_longa)


#====================================================================== GRAFICOS =================================================

#------------------ Ranking de marcas por RAM promedio en cada gama de celulares --------------------------------------------------

# Primero voy a limpiar la tabla completa de todos aquellos datos que esten en blanco y ensucien las categorias
Ranking_de_marcas_por_RAM <- tabla_completa %>%
  filter(
    !is.na(Procesador)        & Procesador        != "",
    !is.na(Categoria)                               # evita faceta NA
  )



# Vector editable con el orden deseado de facetas
orden_gamas <- c("Gama alta", "Gama media", "Gama baja")   # <-- cámbialo aquí

# ---- 2. Cálculo del Top-3 por RAM - MÉTODO ALTERNATIVO PARA ORDENAR ----
top3 <- Ranking_de_marcas_por_RAM %>%
  filter(!is.na(Categoria), !is.na(`RAM (GB)`)) %>%
  group_by(Categoria, marca) %>%
  summarise(ram_prom = mean(`RAM (GB)`, na.rm = TRUE), .groups = "drop") %>%
  group_by(Categoria) %>%
  slice_max(order_by = ram_prom, n = 3) %>%
  ungroup() %>%
  mutate(
    Categoria = factor(Categoria, levels = orden_gamas),
    # CORRECCIÓN: Para ordenar de mayor a menor, reordenamos por el valor negativo de ram_prom
    marca_ord = reorder_within(marca, -ram_prom, Categoria)
  )

# Gráfico
barplot_top3_ram <- ggplot(top3,                                   # datos
                           aes(x = marca_ord,                      # barras reordenadas
                               y = ram_prom,
                               fill = marca)) +                    # color por marca
  geom_col(show.legend = FALSE) +                                  # barras
  geom_text(aes(label = round(ram_prom, 0)),                       # etiquetas sin decimales
            vjust = -0.5, size = 4) +
  facet_wrap(~ Categoria, scales = "free_x", nrow = 1) +           # facetas en una fila
  scale_x_reordered() +                                            # limpia sufijo de reorder_within
  scale_fill_material_d() +                                              # paleta de colores de Google
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +   # pequeño margen superior
  labs(
    title    = "Top 3 · RAM promedio",
    subtitle = "Marcas líderes por gama",
    x        = NULL,
    y        = "RAM Promedio (GB)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x   = element_text(angle = 45, hjust = 1)
  )

# Mostrar el gráfico
print(barplot_top3_ram)

#------------------------------------Barras Horizontales - Separado por Tiendas --------------------------------------------------

#voy a usar una tabla con menos filtros ya que mi columna con cantidades por modelo esta perfecto, solo le saco los espacios extras que puedan molestar
Tabla_separado_x_tiendas <- tabla_completa %>%mutate(sitio = str_trim(sitio))

sitios_obj <- c("Personal", "Claro", "Cetrogar")

Tabla_separado_x_tiendas <- Tabla_separado_x_tiendas %>%
  filter(sitio %in% sitios_obj)

# armo una cuarta categoria con la suma de las otras3
tabla_counts <- Tabla_separado_x_tiendas %>%
  count(marca, sitio, name = "cantidad") %>%
  ungroup()

# Crear el sitio “Total” con la suma de los tres
tabla_total <- tabla_counts %>%
  group_by(marca) %>%
  summarise(cantidad = sum(cantidad), .groups = "drop") %>%
  mutate(sitio = "Total")

# tabla con los 3 sitios mas el cuarto llamado " total" para consolidar los otros 3
tabla_counts <- bind_rows(tabla_counts, tabla_total)

#Definir orden global de marcas
orden_global <- tabla_counts %>%
  group_by(marca) %>%
  summarise(total = sum(cantidad), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(marca)

#Definir orden de facetas (incluye Total al final)
niveles_sitio <- c(sitios_obj, "Total")

tabla_counts <- tabla_counts %>%
  mutate(
    marca = factor(marca, levels = orden_global),
    sitio = factor(sitio, levels = niveles_sitio)
  )

# Graficar
marcasXmodelo <- ggplot(tabla_counts, aes(x = cantidad, y = marca, fill = sitio)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = cantidad), hjust = -0.1, size = 3) +
  facet_wrap(~ sitio, scales = "free_x", nrow = 1) +
  scale_fill_material_d() +       # paleta Material Design discreta
  labs(
    title    = "Marcas × Modelos",
    subtitle = NULL,
    x        = "Modelos(n)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines"),
    strip.text    = element_text(face = "bold")
  ) +
  expand_limits(x = max(tabla_counts$cantidad) * 1.1)


print(marcasXmodelo)

#----------Distrbucion Gauss de Precios "Precio_comprador" y " Precio_anterior" --------------------------------------------------


datos_norm <- tabla_completa %>%
  mutate(
    Precio_comprador = Precio_comprador / 1000,
    Precio_anterior  = Precio_anterior  / 1000
  )

# 1. Calcular medias y desviaciones
mu1    <- mean(datos_norm$Precio_comprador, na.rm = TRUE)
sd1    <- sd(datos_norm$Precio_comprador, na.rm = TRUE)
mu2    <- mean(datos_norm$Precio_anterior,  na.rm = TRUE)
sd2    <- sd(datos_norm$Precio_anterior,  na.rm = TRUE)

# 2. Crear un data.frame de valores x para graficar
x_min  <- min(c(datos_norm$Precio_comprador, datos_norm$Precio_anterior), na.rm = TRUE)
x_max  <- max(c(datos_norm$Precio_comprador, datos_norm$Precio_anterior), na.rm = TRUE)
x_vals <- seq(x_min, x_max, length.out = 500)

gauss_df <- tibble(
  x = rep(x_vals, 2),
  y = c(dnorm(x_vals, mean = mu1, sd = sd1),
        dnorm(x_vals, mean = mu2, sd = sd2)),
  tipo = rep(c("Precio_comprador", "Precio_anterior"), each = length(x_vals))
)

# 3. Graficar
# CORRECCIÓN: Se cambia 'size = 1' por 'linewidth = 1' para alinearse con las nuevas versiones de ggplot2.
distribucion_precios_gauss <- ggplot(gauss_df, aes(x = x, y = y, color = tipo)) +
  geom_line(linewidth = 1) +
  scale_color_material_d() +
  scale_x_continuous(
    labels = scales::dollar_format(
      prefix = "$",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 0.01
    )
  ) +
  scale_y_continuous(
    labels = function(z) {
      # multiplicar por 1000 y luego formatear con 4 decimales fijos
      fmt <- scales::number_format(accuracy = 0.0001, decimal.mark = ",", big.mark = ".")
      fmt(z * 1000)
    }
  ) +
  labs(
    title = "Distribución de Precios",
    x     = "Precio (/1000)",
    y     = "Densidad",   # aclarar en la etiqueta que está escalado
    color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(hjust = 0.5),
    legend.position = "top"
  )

print(distribucion_precios_gauss)

#--------------------------------- Box-Plot - Distribucion de precios por Tienda --------------------------------------------------


price_scale <- scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = "."))   # formato moneda con miles

common_labs <- labs(
  title    = "Distribución de Precios por Vendedor",
  subtitle = NULL,
  x        = "Vendedor",
  y        = "Precio"
)

common_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(hjust = 0.5, size = 14),   ## centramos título
    plot.subtitle   = element_text(hjust = 0.5, size = 12),
    axis.text.x     = element_text(angle = 45, hjust = 1),    ## rotamos etiquetas X
    legend.position = "none"
  )

# 2. Boxplot + jitter con paleta Material
box_plot_x_sitio <- ggplot(tabla_completa, aes(sitio, Precio_comprador, fill = sitio)) +
  geom_boxplot() +   # cajas para ver cuartiles
  geom_jitter(width = 0.2, alpha = 0.6) +  # puntos para ver dispersión
  scale_fill_material_d() +   # paleta
  price_scale +           # aplicamos formato peso
  common_labs +           # ponemos títulos
  common_theme

print(box_plot_x_sitio)


#####Segundos graficos####

#Analisis ratio almacenamiento-precio
tabla_promedio_por_marca <- tabla_completa %>%
  group_by(marca) %>%
  summarise(
    ratio_promedio = mean(ratio_memoria_precio, na.rm = TRUE)
  ) %>%
  ungroup()

barplot_valor_promedio_marca <- ggplot(tabla_promedio_por_marca, aes(x = ratio_promedio,
                                                                     y = fct_reorder(marca, ratio_promedio),
                                                                     fill = marca)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = number(ratio_promedio, accuracy = 0.01)),
            hjust = -0.2,
            size = 3.5,
            color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(
    title = "Ranking de Marcas por Almacenamiento-Precio Promedio",
    subtitle = "Comparación del ratio promedio de memoria vs. precio para cada marca",
    x = "Ratio Promedio",
    y = "Marca"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

print(barplot_valor_promedio_marca)

#analisis GB de RAM-Gama del celular

# CORRECCIÓN: Se cambió 'RAM..GB.' por '`RAM (GB)`' para que coincida con el nombre real de la columna.
tabla_top3_ram_por_categoria <- tabla_completa %>%
  group_by(Categoria, marca) %>%
  summarise(
    ram_promedio = mean(`RAM (GB)` , na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Categoria) %>%
  slice_max(order_by = ram_promedio, n = 4) %>%
  ungroup()

barplot_top3_ram_por_categoria <- ggplot(tabla_top3_ram_por_categoria, aes(x = reorder(marca, ram_promedio), y = ram_promedio, fill = marca))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = number(ram_promedio, accuracy = 1)),
            vjust = -0.5,
            size = 4,
            color = "black") +
  facet_wrap(~ Categoria, scales = "free_x") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(
    title = "Top 4 Marcas con Más RAM Promedio por Categoría",
    subtitle = "Los líderes en memoria para cada gama de celulares",
    x = "Marca",
    y = "Memoria RAM Promedio (en GB)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

print(barplot_top3_ram_por_categoria)

#==================== ANÁLISIS DE DISTRIBUCIÓN DE CARACTERÍSTICAS POR GAMA ====================

# --- Gráfico de Densidad de Características por Gama de Precios ---

# 1. Preparar los datos: Seleccionar y pivotar para formato largo.
datos_densidad_facetas <- tabla_limpia %>%
  select(
    `RAM (GB)`,
    `Almacenamiento (GB)` = `Almacenamiento interno (GB)`,
    `Cámara (MP)` = `Camara Principal (MP)`,
    `Gama` = Categoria
  ) %>%
  pivot_longer(
    cols = -Gama, # Pivotar todas las columnas excepto 'Gama'
    names_to = "Caracteristica",
    values_to = "Valor"
  )

# 2. Calcular las coordenadas de los picos para las etiquetas
peak_labels <- datos_densidad_facetas %>%
  group_by(Caracteristica, Gama) %>%
  summarise(
    dens = list(density(Valor, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  mutate(
    x_peak = map_dbl(dens, ~ .x$x[which.max(.x$y)]),
    y_peak = map_dbl(dens, ~ max(.x$y))
  ) %>%
  select(-dens) %>%
  # Redondear el valor del pico a un número entero
  mutate(x_rounded = round(x_peak)) %>%
  # Agrupar por característica y ordenar para encontrar duplicados
  group_by(Caracteristica) %>%
  arrange(x_rounded) %>%
  # Si un valor es igual o muy cercano al anterior, se considera un duplicado y se oculta la etiqueta
  mutate(
    label = if_else(
      abs(x_rounded - lag(x_rounded, default = -999)) <= 1,
      "", # Etiqueta vacía si es duplicado
      as.character(x_rounded) # Mostrar el número si no es duplicado
    )
  ) %>%
  ungroup()

# 3. Crear el gráfico de densidad facetado con etiquetas en los picos
distribucion_features_por_gama <- ggplot(datos_densidad_facetas, aes(x = Valor, fill = Gama)) +
  geom_density(alpha = 0.7, color="white", linewidth=0.2) +
  # Añadir las etiquetas de texto usando los datos procesados
  geom_text(
    data = peak_labels,
    aes(x = x_peak, y = y_peak, label = label), # Usar la nueva columna de etiquetas
    vjust = -0.7, # Ajuste vertical para que la etiqueta quede sobre el pico
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  facet_wrap(~ Caracteristica, scales = "free") + # Facetas para cada característica, con escalas libres
  scale_fill_material_d(name = "Gama de Precio") + # Usar paleta Material Design y nombrar la leyenda
  labs(
    title = "Distribución de Características por Gama de Precio",
    subtitle = "Cómo se distribuyen RAM, Almacenamiento y Megapíxeles en cada segmento",
    x = "Valor de la Característica",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold") # Estilo para los títulos de las facetas
  )

# 4. Imprimir el gráfico
print(distribucion_features_por_gama)

#==================== DISTRIBUCIÓN DE PRECIOS POR SITIO ====================

# --- Gráfico de Líneas de Densidad de Precios Finales por Sitio ---

# 1. Crear el gráfico de líneas de densidad usando directamente la columna de precio final
distribucion_por_sitio <- ggplot(tabla_limpia, aes(x = Precio_comprador, color = sitio)) +
  geom_density(linewidth = 1.1) +
  scale_x_continuous(
    labels = dollar_format(prefix = "$", big.mark = ".", decimal.mark = ",")
  ) +
  scale_color_material_d() + # Paleta de colores para los sitios
  labs(
    title = "Distribución de Precios Finales por Sitio",
    subtitle = "Comparación de las estrategias de precios de cada tienda",
    x = "Precio Final",
    y = "Densidad",
    color = "Sitio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

# 2. Imprimir el gráfico
print(distribucion_por_sitio)
