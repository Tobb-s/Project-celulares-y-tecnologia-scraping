
#---------------Librerias y carga de datos (setear directorio en carpeta imput)-----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(readxl)
library(scales)
library(ggthemes)  ##Paquete R que aporta paletas “económicas” (The Economist), “excel” y más
library(tidytext)
library(see)  ##Paletas inspiradas en Material Design de Google

#install.packages("tidytext")
#install.packages("see")
#install.packages("ggthemes")

setwd("C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/input")

tabla_claro<-read.csv('claro_celulares_base.csv')
tabla_personal<-read.csv('personal_celulares_base.csv')
tabla_centrogar<-read.csv('cetrogar_celulares_base.csv')
tabla_completa<-read.csv('celulares_todos_base.csv')

## les asigno bien los nombres de cabecera a tabla_completa 

nombres_deseados <- c(
  "sitio","id","marca","modelo","nombre",
  "Sistema Operativo","Procesador","RAM (GB)","Almacenamiento interno (GB)",
  "Pantalla (Pulgadas)","Camara Principal (MP)","Camara frontal (MP)","NFC",
  "Precio_comprador","Precio_anterior","precio_sin_impuestos","financiacion_plazo",
  "per_descuento","presion_impositiva_pais_per","ratio_memoria_precio",
  "ratio_mp_precio","Categoria"
)

# Se lo asignas directamente:
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

                              # pedimos que las columnas sean numeros y que se limpies los NA y demas datos que ensucien
                              numeric_cols <- c(
                                "Precio_comprador",
                                "per_descuento",
                                "RAM (GB)",
                                "Almacenamiento interno (GB)",
                                "Camara Principal (MP)",
                                "Camara frontal (MP)"
                              )
                              
                              # 2. Limpiamos tabla_completa
                              tabla_limpia <- tabla_completa %>%mutate(across(all_of(numeric_cols), ~ as.numeric(.))) %>%filter(across(all_of(numeric_cols), ~ !is.na(.))) 



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
                          media_Camara_principal    = mean(`Camara Principal (MP)`, na.rm = TRUE),
                          mediana_Camara_principal  = median(`Camara Principal (MP)`, na.rm = TRUE),
                          moda_Camara_principal     = Mode(`Camara Principal (MP)`),
                          min_Camara_principal      = min(`Camara Principal (MP)`, na.rm = TRUE),
                          max_Camara_principal      = max(`Camara Principal (MP)`, na.rm = TRUE),
                          midrange_Camara_principal = (min_Camara_principal + max_Camara_principal) / 2,
                          
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
                          names_to    = c("estadística", "variable"),
                          names_pattern = "^(media|mediana|moda|min|max|midrange)_(.+)$"
                        ) %>%
                        pivot_wider(
                          names_from  = estadística,
                          values_from = value
                        ) %>%
                        mutate(variable = recode(variable,
                                                 Precio_comprador            = "Precio_comprador",
                                                 `RAM (GB)`                  = "RAM (GB)",
                                                 `Almacenamiento interno (GB)` = "Almacenamiento interno (GB)",
                                                 Camara_Principal            = "Camara Principal (MP)",
                                                 `Camara frontal (MP)`       = "Camara frontal (MP)",
                                                 per_descuento               = "per_descuento"
                        )) %>%
                        select(variable, media, mediana, moda, min, max, midrange) %>%mutate(across(where(is.numeric), ~ round(.x, 0)))
                      
            
          View(eda_tabla)
          
          
        ## Medidas de Dispercion::-----------------------------------------------------------------------------------------------------

          
          dispersion_longa <- tabla_limpia %>%
            summarise(
              across(
                all_of(numeric_cols),
                list(
                  rango = ~ diff(range(.x, na.rm = TRUE)),
                  sd    = ~ sd(.x,    na.rm = TRUE),
                  var   = ~ var(.x,   na.rm = TRUE)
                ),
                .names = "{.col}_{.fn}"
              )
            ) %>%
            pivot_longer(
              cols      = everything(),
           
              names_to  = c("variable", "estadistica"),
              names_pattern = "(.*)_(rango|sd|var)$",
              values_to = "valor"
            ) %>%
            pivot_wider(
              names_from  = estadistica,
              values_from = valor
            ) %>%
        
            mutate(variable = recode(variable,
                                     "Precio_comprador"             = "Precio",
                                     "per_descuento"                = "% Descuento",
                                     "RAM (GB)"                     = "RAM (GB)",
                                     "Almacenamiento interno (GB)"  = "Alm. (GB)",
                                     "Camara Principal (MP)"        = "Cámara Ppal (MP)",
                                     "Camara frontal (MP)"          = "Cámara Frontal (MP)"
            )) %>%
     
            mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
            select(variable, rango, sd, var)
          
    
      View(dispersion_longa)
      
      
        ## Medidas Basadas en Cuantiles ( medidas mas robustas)-------------------------------------------------
      
      
      robust_longa <- tabla_limpia %>%
        summarise(
          across(
            all_of(numeric_cols),
            list(
              IQR      = ~ IQR(.x,   na.rm = TRUE),                          # Q3 – Q1
              Semi_IQR = ~ IQR(.x,   na.rm = TRUE) / 2,                      # (Q3–Q1)/2
              P90_10   = ~ diff(quantile(.x, c(0.1,0.9), na.rm = TRUE))      # P90 – P10
            ),
            .names = "{.col}_{.fn}"
          )
        ) %>%
        pivot_longer(
          cols       = everything(),
          names_to   = c("variable", "medida"),
          names_sep  = "_(?=[^_]+$)",   # separa sólo en el ÚLTIMO guión bajo
          values_to  = "valor"
        ) %>%
        pivot_wider(
          names_from  = medida,
          values_from = valor
        ) %>%
        # 2. Recodificar nombres de variable para que queden más cortos
        mutate(variable = recode(variable,
                                 "Precio_comprador"            = "Precio",
                                 "per_descuento"               = "% Desc.",
                                 "RAM (GB)"                    = "RAM (GB)",
                                 "Almacenamiento interno (GB)" = "Alm. (GB)",
                                 "Camara Principal (MP)"       = "Cámara Ppal (MP)",
                                 "Camara frontal (MP)"         = "Cámara Frontal (MP)"
        )) %>%
        # 3. Redondear sin decimales
        mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
        # 4. Seleccionar columnas en el orden deseado
        select(variable, IQR, Semi_IQR, P90_10)
      
      View(robust_longa)
                    
#====================================================================== GRAFICOS =================================================

#------------------ Ranking de marcas por RAM promedio en cada gama de celulares --------------------------------------------------

    # Primero voy a limpiar la tabla completa de todos aquellos datos que esten en blanco y ensucien las categorias

          Ranking_de_marcas_por_RAM <- tabla_completa %>%
                                                        filter(
                                                          !is.na(Procesador)          & Procesador          != "",
                                                          !is.na(Categoria)                               # evita faceta NA
                                                        )



    # Vector editable con el orden deseado de facetas 
    orden_gamas <- c("Gama alta", "Gama media", "Gama baja")   # <-- cámbialo aquí

    # ---- 2. Cálculo del Top-3 por RAM 
    top3 <- Ranking_de_marcas_por_RAM %>%                                   # parte de tu objeto
                                        filter(!is.na(Categoria), !is.na(`RAM (GB)`)) %>%          # descarta NA importantes
                                          group_by(Categoria, marca) %>% 
                                            summarise(ram_prom = mean(`RAM (GB)`, na.rm = TRUE), .groups = "drop_last") %>% 
                                              arrange(desc(ram_prom), .by_group = TRUE) %>%              # ordenar de mayor a menor
                                                slice_head(n = 3) %>%                                      #exactamente 3 por gama ( ni dos, ni cuatro)
                                                  ungroup() %>% 
                                                    mutate(
                                                      Categoria = factor(Categoria, levels = orden_gamas),     # fija el orden de facetas ( Gama ALta, media y baja)
                                                      marca_ord = reorder_within(
                                                        x      = marca,
                                                        by     = ram_prom,
                                                        within = Categoria,
                                                        .desc  = FALSE   
                                                      )
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
  scale_fill_material_d() +                                         # paleta de colores de Google
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

    #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#==#=#=#=#=#=#=#=#=#=
    #  Gama alta (≥ 8 GB): OPPO y Xiaomi lideran con 12 GB de RAM promedio, por encima de Samsung (10 GB), lo que demuestra su foco en usuarios premium 
    #                      que demandan alto rendimiento.
    #  Gama media (4–7 GB): Infinix encabeza con 8 GB, seguido de OPPO (6 GB) y TCL (5 GB), indicando que Infinix busca diferenciarse ofreciendo 
    #                       características avanzadas en el segmento intermedio.
    #  Gama baja (≤ 3 GB): Xiaomi alcanza 3 GB frente a 2 GB de Motorola y ZTE, lo que le otorga una ventaja competitiva al ofrecer mayor fluidez incluso 
    #                      en los modelos más económicos.
    #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#==#=#=#=#=#=#=#=#=#=
    
    
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
    p <- ggplot(tabla_counts, aes(x = cantidad, y = marca, fill = sitio)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      geom_text(aes(label = cantidad), hjust = -0.1, size = 3) +
      facet_wrap(~ sitio, scales = "free_x", nrow = 1) +
      scale_fill_material_d() +            # paleta Material Design discreta
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
    

  print(p)
  
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#==#=#=#=#=#=#=#=#=#=
  #  Top-3 dominante: Motorola, Samsung y Xiaomi concentran la mayoría de los modelos (59, 36 y 20 respectivamente), 
  #  lo que revela su liderazgo absoluto en el mercado.
  #
  #  Canales con enfoque distinto:
  #       > Personal:: apuesta por gamas medias/bajas, con Motorola (13) y Samsung (6) al frente y escasa presencia de Apple (3).
  #       > Claro:: refuerza aún más a Motorola (21) y Samsung (20), mostrando una cobertura casi total de estos fabricantes en el operador principal.
  #       > Cetrogar::  como retailer independiente, ofrece un surtido más amplio y equilibrado: Motorola (25), Xiaomi (14), Samsung (10) y Tecno (6), 
  #          buscando captar distintos segmentos de clientes
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#==#=#=#=#=#=#=#=#=#=
  
  
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
  distribucion_precios_gauss <- ggplot(gauss_df, aes(x = x, y = y, color = tipo)) +
    geom_line(size = 1) +
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
      plot.title      = element_text(hjust = 0.5),
      legend.position = "top"
    )
  
  print(distribucion_precios_gauss)
  
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
  #   Las promociones desplazan la distribución del precio real hacia valores menores que el precio de lista, 
  #   de modo que cuanto mayor sea la separación entre los picos rojo (promocional) y azul (lista), mayor será el descuento 
  #   promedio. Además, la curva roja suele ser más estrecha que la azul, indicando que los precios con oferta se concentran 
  #   en un rango reducido, mientras que los precios de lista abarcan una mayor variabilidad de modelos. 
  #   Por último, el solapamiento de las dos curvas señala los rangos de precios en los que ocasionalmente no hay descuentos, 
  #   y las porciones de la curva azul que quedan fuera de la roja representan niveles de precio de lista que casi nunca se pagan.
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
  
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
    price_scale +         # aplicamos formato peso
    common_labs +          # ponemos títulos
    common_theme
  
  print(box_plot_x_sitio)
  
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
  #  El boxplot muestra que Claro ofrece el rango de precios más amplio y elevado (mediana ≈ $600 000, IQR ≈ $300 000–$1 400 000) 
  #  con numerosos modelos premium fuera del rango intercuartílico. Cetrogar se sitúa en un nivel intermedio (mediana ≈ $300 000, IQR moderado) 
  #  y Personal exhibe la distribución más ajustada y económica (mediana ≈ $250 000, IQR estrecho). 
  #  Los outliers de Claro indican varios lanzamientos de alta gama, mientras que Cetrogar y Personal mantienen menos extremos, concentrándose 
  #  en gamas bajas-medias.
  # 
  # Conclusión::
  #  Claro lidera en diversidad y precios altos, pero debe vigilar el sobrestock de modelos caros; Cetrogar puede aprovechar su segmento medio 
  #  para pulir promociones, y Personal tiene espacio para incorporar más gamas medias-altas y diversificar su oferta.
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
  
  
  
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
  
  tabla_top3_ram_por_categoria <- tabla_completa %>%
    group_by(Categoria, marca) %>%
    summarise(
      ram_promedio = mean(RAM..GB. , na.rm = TRUE),
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
  