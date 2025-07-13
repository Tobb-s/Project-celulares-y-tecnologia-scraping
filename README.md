Project-celulares-y-tecnologia-scraping
Este repositorio contiene todo el flujo de trabajo para recolectar, limpiar y analizar información de celulares disponibles en tiendas online de Argentina.
El proyecto se desarrolló como parte de un trabajo práctico universitario.
Todos los scripts están escritos en R y se organizan por etapas de extracción, limpieza, análisis exploratorio y presentación de resultados.

Contenido del repositorio
HTMLS/                       # Fragmentos HTML de las tiendas
input/                       # CSVs listos para análisis
output/
├── CSVs/                    # Salidas tabulares del EDA
├── Graficos/                # Imágenes generadas (ggplot2)
├── RData.gz                 # Workspace comprimido
raw/                         # Datos crudos obtenidos por scraping
scripts/
├── 01 - Extraccion/         # R scripts para cada tienda
├── 02 - Limpieza/           # Limpieza y consolidación de datos
├── 03 - EDA/                # Análisis exploratorio y modelos
└── 04_Presentacion/         # Presentación final (PDF)
TP Final UBA Opción 2 – Scraping y Análisis.pdf  # Informe entregable
Project-celulares-y-tecnologia-scraping-main.Rproj
README.md
Scripts de extracción (scripts/01 - Extraccion)
Tienda CENTROHOGAR / CLARO / PERSONAL
Cada carpeta incluye un script Scraping ... COMPLETO*.R y un diagrama de flujo.
Los scripts utilizan rvest y RSelenium para recorrer los sitios, obtener todas las URLs de productos y extraer precios, cuotas y especificaciones.

Limpieza y consolidación (scripts/02 - Limpieza)
Limpieza_v2.R lee los CSVs crudos de /raw, corrige variables, normaliza formatos y genera los archivos base en /input.
También unifica las tres fuentes en celulares_todos_base.csv.

Análisis exploratorio (scripts/03 - EDA)
EDA_v2 - copia.R realiza:

Descripción estadística (tablas y gráficos).

Detección de outliers en descuentos.

Visualizaciones comparando precios y prestaciones.

Un modelo k-nearest neighbors para clasificar la “Categoría” (gama) de cada equipo.

Las salidas se guardan en /output/CSVs y /output/Graficos.

Presentación (scripts/04_Presentacion)
Contiene el archivo Análisis Exploratorio de Precios y Especificaciones de Celulares en Plataformas de Venta con R.pdf, síntesis del proyecto con gráficos y conclusiones.

Requisitos
R (probado con la versión usada en el .Rproj).

Paquetes: tidyverse, rvest, RSelenium, xml2, stringr, readr, dplyr,
ggplot2, ggthemes, patchwork, caret, class, skimr, forcats, here, entre otros.

Cómo reproducir el pipeline
Clonar este repositorio y abrir Project-celulares-y-tecnologia-scraping-main.Rproj.

Instalar los paquetes necesarios (se indican al inicio de cada script).

Ejecutar, en orden:

Extracción: los scripts dentro de scripts/01 - Extraccion para cada tienda.
Los CSVs se guardarán en la carpeta raw/.

Limpieza: scripts/02 - Limpieza/Limpieza_v2.R para generar los archivos en input/.

EDA: scripts/03 - EDA/EDA_v2 - copia.R para crear gráficos y tablas dentro de output/.

Revisar la presentación en scripts/04_Presentacion.

Notas adicionales
Los fragmentos de HTML en HTMLS/ sirven como referencia de estructura de cada sitio.

Los archivos dentro de raw/ e input/ son ejemplos reducidos para facilitar la prueba del flujo.

Actualmente no se incluye una licencia explícita
