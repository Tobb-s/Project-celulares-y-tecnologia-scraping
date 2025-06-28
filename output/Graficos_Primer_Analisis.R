#####Librerias y carga de datos (setear directorio en carpeta imput)#####
library(tidyverse)
library(ggplot2)
library(readxl)
library(scales)
setwd("C:/Users/valen/OneDrive - Económicas - UBA/=ANALISIS DE DATO PARA LA ECONOMIA Y NEGOCIOS=/Project-celulares-y-tecnologia-scraping/input")

tabla_claro<-read.csv('claro_celulares_base.csv')
tabla_personal<-read.csv('personal_celulares_base.csv')
tabla_centrogar<-read.csv('cetrogar_celulares_base.csv')
tabla_completa<-read.csv('celulares_todos_base.csv')

#####Primeros graficos####
#Grafico de barras para entender las cantidades por marca
barplot_cantidades<-ggplot(tabla_completa, aes(y = fct_rev(fct_infreq(marca)))) +
  geom_bar(fill = "skyblue") +
  labs(
    title = "Presencia de las Marcas a Traves de las Paginas",
    subtitle = "Cantidad Total de Modelos Disponibles",
    y = "Marca",
    x = "Cantidad de Modelos"
  ) +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), hjust = -0.2)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(barplot_cantidades)

#Separado por tiendas

barplot_separado_tiendas<-ggplot(tabla_completa, aes(y = fct_rev(fct_infreq(marca)))) +
  geom_bar(fill = "skyblue") +
  facet_wrap(~ sitio, scales = "free_x")+
  labs(
    title = "Presencia de las Marcas a Traves de las Paginas",
    subtitle = "Cantidad Total de Modelos Disponibles",
    y = "Marca",
    x = "Cantidad de Modelos"
  ) +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), hjust = -0.2)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(barplot_separado_tiendas)

#Boxplot comparando precios

boxplot_precios<-ggplot(tabla_completa, aes(x = "Oferta Combinada", y = Precio_comprador)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) + 
  geom_jitter(width = 0.25, alpha = 0.4, color = "black") + 
  scale_y_continuous(labels = number_format(prefix = "$", big.mark = ".")) +
  labs(
    title = "Distribución General de los Precios",
    subtitle = "Visión combinada de la oferta de Cetrogar, Claro, y Personal",
    x = "",
    y = "Precio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

print(boxplot_precios)

#Boxplot separado por sitio

boxplot_precios_separado<-ggplot(tabla_completa, aes(x = sitio, y = Precio_comprador, fill = sitio)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.6, show.legend = FALSE) +
  scale_y_continuous(labels = number_format(prefix = "$", big.mark = ".")) +
  labs(
    title = "Distribucion de los Precios por Vendedor",
    subtitle = "Comparación de Precios por Vendedor",
    x = "Vendedor",
    y = "Precio (En Pesos)"
  ) +
  theme_minimal(base_size = 12)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(boxplot_precios_separado)

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
