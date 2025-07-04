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



# --- Eliminamos setwd() ---
# La línea setwd() fue eliminada. Ya no es necesaria y es una mala práctica para proyectos compartidos.
# setwd("C:/Users/valentino.didomenica/Desktop/Project-celulares-y-tecnologia-scraping-main/input") # <-- ELIMINADO

# --- Carga de datos usando here() ---
# here() encuentra la raíz del proyecto (donde está tu archivo .Rproj) y construye la ruta desde allí.
# Esto funciona en cualquier computadora.
tabla_completa <- read.csv(here("input", "celulares_todos_base.csv"))

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

# --- Directorios de salida usando here() ---
# Definimos las carpetas de salida relativas a la raíz del proyecto.
output_dir <- here("output")
csv_dir <- file.path(output_dir, "CSVs")
graficos_dir <- file.path(output_dir, "Graficos")

# Creamos las carpetas si no existen. Este código no necesita cambios.
if (!dir.exists(csv_dir)) {
  dir.create(csv_dir, recursive = TRUE)
}
if (!dir.exists(graficos_dir)) {
  dir.create(graficos_dir, recursive = TRUE)
}


#-------------------------------------------------------------Paletas de Colores--------------------------------------------
#Otras paletas de ggthemes
#scale_fill_tableau(palette="Classic 10")
#scale_fill_solarized()
#scale_fill_economist()
#scale_fill_colorblind()

##De Google
#scale_fill_material_d() / scale_color_material_d() para datos discretos
#scale_fill_material_c() / scale_color_material_c() para datos continuos

#====================================================== EDA UNIVARIABLE GENERALES================================================

#1. ----- UNIVARIADOS  SOBRE TODO EL CONJUNTO ------
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


# 1.1  ----- Estadísticas sobre todo el conjunto :: ( Media, mediana, moda, min, max, midrange de cada variable numérica)  ----- 
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


#Ver tabla
#View(eda_tabla)



# 1.2  ----- Medidas de dispersión global:: ( Rango, sd, var, IQR, Semi-IQR, P90–P10)  ----- 

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

#Ver tabla

#View(dispersion_longa)




        ##---- Medidas Basadas en Cuantiles (medidas mas robustas)--------------------
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
        
        #Ver tabla
        
        #View(robust_longa)



#================================================= ANALISIS POR GRANDES GRUPOS ===========================================

#2. ----- COMPARACION ENTRE TIENDAS ------

# 2.1  ----- Distribucion de precios por Tienda  ----- 

price_scale <- scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = "."))    # formato moneda con miles

common_labs <- labs(
  title    = "Distribución de Precios por Vendedor",
  subtitle = NULL,
  x        = "Vendedor",
  y        = "Precio"
)

common_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title     = element_text(hjust = 0.5, size = 14),    ## centramos título
    plot.subtitle  = element_text(hjust = 0.5, size = 12),
    axis.text.x    = element_text(angle = 45, hjust = 1),     ## rotamos etiquetas X
    legend.position = "none"
  )


box_plot_x_sitio <- ggplot(tabla_completa, aes(sitio, Precio_comprador, fill = sitio)) +
  geom_boxplot() +  # cajas para ver cuartiles
  geom_jitter(width = 0.2, alpha = 0.6) + # puntos para ver dispersión
  scale_fill_material_d() +  # paleta
  price_scale +            # aplicamos formato peso
  common_labs +            # ponemos títulos
  common_theme

print(box_plot_x_sitio)


#3. ----- COMPARACION POR GAMA DE PRECIO ------           

# 3.1  -----  Caracteristicas por Gama  ----- 

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
    title = "Características por Gama",
    subtitle = "RAM, Almacenamiento y Megapíxeles por segmento",
    x = "Valor",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold") # Estilo para los títulos de las facetas
  )

# Grafico
print(distribucion_features_por_gama)

# 3.2  -----  Outliers de % Descuento por Gama  ----- 


datos_out <- tabla_completa %>%
  mutate(per_descuento = as.numeric(per_descuento)/ 100) %>% 
  filter(!is.na(per_descuento), !is.na(Categoria)) %>% 
  filter(per_descuento < 0.50) %>% 
  group_by(Categoria) %>% 
  mutate(
    Q1      = quantile(per_descuento, .25),
    Q3      = quantile(per_descuento, .75),
    IQR     = Q3 - Q1,
    lim_inf = Q1 - 1.5*IQR,
    lim_sup = Q3 + 1.5*IQR,
    outlier = per_descuento < lim_inf | per_descuento > lim_sup
  ) %>% 
  ungroup()

outliers_summary <- datos_out %>% 
  group_by(Categoria) %>% 
  summarise(
    n_outliers = sum(outlier),
    total      = n(),
    pct_out    = round(100*n_outliers/total,1),
    .groups="drop"
  )
#print(outliers_summary)

g_outliers <- ggplot(datos_out, aes(x = Categoria, y = per_descuento, fill = Categoria)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    data    = subset(datos_out, outlier == FALSE),
    color   = "black",
    width   = .18,
    size    = 2,
    alpha   = .7
  ) +
  scale_fill_material_d() +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "steelblue"), guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Outliers % descuento x Gama",
    subtitle = "Tukey (Q1 ± 1.5×IQR)",
    x = NULL, y = "% de descuento"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = .5, face = "bold"),
    plot.subtitle = element_text(hjust = .5),
    legend.position = "none"   # oculta leyenda de fill si no la necesitas
  )

print(g_outliers)



#================================================  ANALISIS POR SUBGRUPOS INTERMEDIOS ====================================         

#4. ----- RANKING DE MARCAS ------  

# 4.1 -----  Top 3. RAM promedio (Por Gama) ----------------------------

# 1) Filtrar y resumir
Ranking_de_marcas_por_RAM <- tabla_completa %>%
  filter(
    !is.na(Procesador) & Procesador != "",
    !is.na(Categoria),
    !is.na(`RAM (GB)`)
  )

orden_gamas <- c("Gama alta", "Gama media", "Gama baja")

# 2) Calcular Top 3 y asignar Nivel por posición
top3 <- Ranking_de_marcas_por_RAM %>%
  group_by(Categoria, marca) %>%
  summarise(ram_prom = mean(`RAM (GB)`, na.rm = TRUE), .groups="drop") %>%
  group_by(Categoria) %>%
  slice_max(order_by = ram_prom, n = 3, with_ties = FALSE) %>%
  arrange(Categoria, desc(ram_prom)) %>%
  mutate(
    Nivel = factor(
      case_when(
        row_number() == 1 ~ "Máximos",    ## se crearon 3 variables para poder asignar a cada una un colo particular y se tuvo que dar numero a la posicion de cada una de las barras
        row_number() == 2 ~ "Intermedio",
        row_number() == 3 ~ "Mínimos"
      ),
      levels = c("Máximos", "Intermedio", "Mínimos")
    ),
    Categoria = factor(Categoria, levels = orden_gamas),
    marca_ord = reorder_within(marca, -ram_prom, Categoria)
  ) %>%
  ungroup()

# 3) Gráfico
barplot_top3_ram <- ggplot(top3,
                           aes(x = marca_ord, y = ram_prom, fill = Nivel)) +
  geom_col(show.legend = FALSE) +              ##oculto la leyenda
  geom_text(aes(label = round(ram_prom, 0)),
            vjust = -0.5, size = 4) +
  facet_wrap(~ Categoria, scales = "free_x", nrow = 1) +
  scale_x_reordered() +
  scale_fill_material_d(name = "Nivel") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
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

print(barplot_top3_ram)

# 4.2 -----   Barras Horizontales - Top 3. Marcas x Modelo ---------------


tabla_sep <- tabla_completa %>%
  mutate(sitio = str_trim(sitio)) %>%
  filter(sitio %in% c("Personal", "Claro", "Cetrogar"))

total_por_marca <- tabla_sep %>% count(marca, name = "total_modelos") 

top3_marcas <- total_por_marca %>% slice_max(order_by = total_modelos, n = 3) %>% pull(marca)

tabla_top3 <- tabla_sep %>% filter(marca %in% top3_marcas)

tabla_counts <- tabla_top3 %>% count(marca, sitio, name = "cantidad") %>% ungroup()


tabla_total <- tabla_counts %>%
  group_by(marca) %>%
  summarise(cantidad = sum(cantidad), .groups = "drop") %>%
  mutate(sitio = "Total")

tabla_counts <- bind_rows(tabla_counts, tabla_total)

orden_global <- top3_marcas
niveles_sitio <- c("Personal", "Claro", "Cetrogar", "Total")

tabla_counts <- tabla_counts %>%
  mutate(
    marca = factor(marca, levels = orden_global),
    sitio = factor(sitio, levels = niveles_sitio)
  )

# 8. Graficamos
marcasXmodelo <- ggplot(tabla_counts, aes(x = cantidad, y = marca, fill = sitio)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = cantidad), hjust = -0.1, size = 3) +
  facet_wrap(~ sitio, scales = "free_x", nrow = 1) +
  scale_fill_material_d() +
  labs(
    title = "Top 3 Marcas con Mas Modelos Publicados",
    x     = "Cantidad de Modelos",
    y     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(face = "bold")
  ) +
  expand_limits(x = max(tabla_counts$cantidad) * 1.1)

print(marcasXmodelo)        



#================================================  DESAGREGACION FINA Y CASOS ESPECIFICOS ===============================

#6. ----- DISTRIBUCIONES DETALLADAS ------

#6.1 -----  Distrbucion Gauss de Precios "Precio_comprador" y " Precio_anterior" ------


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
  scale_fill_material_d() +    ## Paleta de colores elegida
  scale_x_continuous(
    labels = scales::dollar_format(
      prefix       = "$",
      big.mark     = ".",
      decimal.mark= ",",
      accuracy     = 0.01
    )
  ) +
  # Eje Y: densidad reescalada (multiplicada por 1000 antes de formatear)
  scale_y_continuous(
    labels = function(z) {
      # z * 1000 porque antes dividiste precios / 1000
      scales::number_format(
        accuracy     = 0.0001,
        big.mark     = ".",
        decimal.mark= ","
      )(z * 1000)
    }
  )+
  labs(
    title = "Distribución de Precios",
    x     = "Precio (/1000)",
    y     = "Densidad",    # aclarar en la etiqueta que está escalado
    color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(hjust = 0.5),
    legend.position = "top"
  )

print(distribucion_precios_gauss)




#6.2 -----  Cálculo del ratio promedio por marca ------

tabla_promedio_por_marca <- tabla_completa %>%
  group_by(marca) %>%
  summarise(
    ratio_promedio = mean(costo_unitario_total, na.rm = TRUE)
  ) %>%
  ungroup()

# Gráfico con paleta Material Design discreta
barplot_valor_promedio_marca <- ggplot(
  tabla_promedio_por_marca,
  aes(
    x = ratio_promedio,
    y = fct_reorder(marca, ratio_promedio),
    fill = marca
  )
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = number(ratio_promedio, accuracy = 0.01)),
    hjust = -0.2,
    size = 3.5,
    color = "black"
  ) +
  scale_fill_material_d() +  # paleta Material Design discreta
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(
    title    = "Precio Medio por GB y MP Combinados",
    subtitle = NULL,
    x        = "$",
    y        = "Marca"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

print(barplot_valor_promedio_marca)



#================================================= QUE (Celular), DONDE ( sitio) y PORQUE ( caracteristicas/precio) ==========

#Seleccion de mejores celulares
mejores_celulares<-tabla_completa%>%
  group_by(Categoria)%>%
  slice_min(order_by = costo_unitario_total, n = 1) %>%
  select(Categoria, marca, sitio, id, costo_unitario_total, `RAM (GB)`, `Almacenamiento interno (GB)`, `Camara Principal (MP)`) %>%
  distinct(marca, .keep_all = TRUE) %>%
  arrange(costo_unitario_total) %>%
  ungroup()

#Ver Cuadro

#view(mejores_celulares)

#Crear el gráfico con el puntaje como etiqueta
mejor_celular_por_gama <- ggplot(mejores_celulares , aes(x = fct_reorder(Categoria, costo_unitario_total), y = costo_unitario_total, fill = id)) +
  geom_col() +
  geom_text(
    aes(label = round(costo_unitario_total)),
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Top Celulares por Puntaje",
    subtitle = "Costo por GB + MP de cámara",
    x = "Gama",
    y = "Puntaje (↓ mejor)",
    fill = "Marca"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x    = element_text(angle = 30, hjust = 1),
    plot.title     = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle  = element_text(hjust = 0.5, size = 12),
    legend.title   = element_text(size = 12, face = "bold")
  )

#print(mejor_celular_por_gama)

#2. ---- Comparativa de Lideres por Especificacion ----

            # Tabla filtro con Comparacion de caracteristicas entre mejores celulares
            
            comparacion_mejores_celulares <- mejores_celulares %>% 
              select(  id,
                       `RAM (GB)`,
                       `Almacenamiento interno (GB)`,
                       `Camara Principal (MP)`
              ) %>%
              pivot_longer(
                cols = -id, # Pivotar todas las columnas excepto 'Gama'
                names_to = "Caracteristica",
                values_to = "Valor"
              )

              comparacion_mejores_celulares <- mejores_celulares %>%
                mutate(
                  id_sitio = paste0(id, " (", sitio, ")")
                ) %>%
                select(
                  id_sitio,
                  `RAM (GB)`,
                  `Almacenamiento interno (GB)`,
                  `Camara Principal (MP)`
                ) %>%
                pivot_longer(
                  cols        = -id_sitio,
                  names_to    = "Caracteristica",
                  values_to   = "Valor"
                )
              
              # Grafico:
        
              comparacion_tecnica_mejores_celulares <- ggplot(
                comparacion_mejores_celulares,
                aes(x = id_sitio, y = Valor, fill = id_sitio)
              ) +
                geom_col(show.legend = FALSE) +
                geom_text(
                  aes(label = Valor),
                  vjust = -0.5,
                  fontface = "bold"
                ) +
                facet_wrap(~ Caracteristica, scales = "free") +
                labs(
                  title    = "Comparativa de Líderes por Especificación",
                  subtitle = "RAM • Almacenamiento • Cámara",
                  x        = "Celular (sitio)",
                  y        = "Valor"
                ) +
                theme_minimal(base_size = 12) +
                theme(
                  axis.text.x    = element_text(angle = 30, hjust = 1),
                  plot.title     = element_text(hjust = 0.5, size = 16, face = "bold"),
                  plot.subtitle  = element_text(hjust = 0.5, size = 12),
                  strip.text     = element_text(size = 11, face = "bold")
                )
              
              #print(comparacion_tecnica_mejores_celulares)


      comparacion_todo_junto <- mejor_celular_por_gama + comparacion_tecnica_mejores_celulares + scale_y_continuous(position = "right")+ theme(
        strip.text = element_text(size = 9.5, face = "bold") 
      )


print(comparacion_todo_junto)


#================================================= GUARDADO DE ARCHIVOS ===========================================
# En esta sección se guardan todas las tablas y gráficos generados en el script.
# Como los directorios 'csv_dir' y 'graficos_dir' ya usan here(), este código funcionará sin cambios.

# --- Tablas (CSV) ---
write.csv(eda_tabla, file.path(csv_dir, "eda_tabla.csv"), row.names = FALSE)
write.csv(dispersion_longa, file.path(csv_dir, "dispersion_longa.csv"), row.names = FALSE)
write.csv(robust_longa, file.path(csv_dir, "robust_longa.csv"), row.names = FALSE)
write.csv(outliers_summary, file.path(csv_dir, "outliers_summary.csv"), row.names = FALSE)
write.csv(top3, file.path(csv_dir, "top3_ram_marcas.csv"), row.names = FALSE)
write.csv(tabla_counts, file.path(csv_dir, "tabla_counts_modelos_por_marca.csv"), row.names = FALSE)
write.csv(tabla_promedio_por_marca, file.path(csv_dir, "tabla_promedio_por_marca.csv"), row.names = FALSE)
write.csv(mejores_celulares, file.path(csv_dir, "mejores_celulares.csv"), row.names = FALSE)
write.csv(comparacion_mejores_celulares, file.path(csv_dir, "comparacion_mejores_celulares.csv"), row.names = FALSE)

# --- Gráficos (PNG) ---
ggsave(file.path(graficos_dir, "box_plot_x_sitio.jpeg"), plot = box_plot_x_sitio, width = 8, height = 6)
ggsave(file.path(graficos_dir, "distribucion_features_por_gama.jpeg"), plot = distribucion_features_por_gama, width = 10, height = 7)
ggsave(file.path(graficos_dir, "g_outliers.jpeg"), plot = g_outliers, width = 8, height = 6)
ggsave(file.path(graficos_dir, "barplot_top3_ram.jpeg"), plot = barplot_top3_ram, width = 10, height = 6)
ggsave(file.path(graficos_dir, "marcasXmodelo.jpeg"), plot = marcasXmodelo, width = 12, height = 6)
ggsave(file.path(graficos_dir, "distribucion_precios_gauss.jpeg"), plot = distribucion_precios_gauss, width = 8, height = 6)
ggsave(file.path(graficos_dir, "barplot_valor_promedio_marca.jpeg"), plot = barplot_valor_promedio_marca, width = 10, height = 8)
ggsave(file.path(graficos_dir, "mejor_celular_por_gama.jpeg"), plot = mejor_celular_por_gama, width = 10, height = 7)
ggsave(file.path(graficos_dir, "comparacion_tecnica_mejores_celulares.jpeg"), plot = comparacion_tecnica_mejores_celulares, width = 12, height = 7)
ggsave(file.path(graficos_dir, "comparacion_todo_junto.jpeg"), plot = comparacion_todo_junto, width = 14, height = 8)
