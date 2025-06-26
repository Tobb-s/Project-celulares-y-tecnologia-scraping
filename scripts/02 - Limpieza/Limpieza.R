



# 1. Cargar librerías ----------------------------------------------------------
library(readr)      # read_csv()
library(dplyr)      # si luego quieres manipular datos (opcional)
library(stringr)
library(purrr)


#==================================================================================================================================
#                                                         PERSONAL
#=================================================================================================================================


# 2. Definir carpeta y archivo 
dir_path  <- "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/raw"
file_name <- "personal_celulares_multipagina.csv"   

# 3. Construir la ruta completa 
csv_path <- file.path(dir_path, file_name)

# 4. Importar el CSV 
personal_celulares <- read_csv(csv_path)


#-----------------------------------------------------------------LIMPIEZA-----------------------------------------------------------------------#

# Limpieza de datos vacios o en blanco

personal_celulares <- personal_celulares %>% mutate( across(c(`Sistema Operativo`, `PRECIO DE LISTA`), ~ na_if(.x, ""))) %>%
  
                                              filter(
                                                  !is.na(`Sistema Operativo`),                                                      
                                                  !is.na(`PRECIO DE LISTA`) & `PRECIO DE LISTA`               != 0,
                                                  !is.na(`PRECIO DE LISTA SIN IMPUESTOS`) & `PRECIO DE LISTA SIN IMPUESTOS`               != 0,
                                                  !is.na(`PRECIO DE PROMOCIÓN`) & `PRECIO DE PROMOCIÓN`               != 0,
                                                  !is.na(`RAM (GB)`) & `RAM (GB)`               != 0,                 
                                                  !is.na(`Almacenamiento interno (GB)`) & `Almacenamiento interno (GB)` != 0,
                                                  !is.na(`Pantalla (Pulgadas)`) & `Pantalla (Pulgadas)`    != 0
                                                )

## Convierto en "numero" a las columnas de precios

personal_celulares <- personal_celulares %>% mutate(across(c(`PRECIO DE LISTA`,
                                                              `PRECIO DE LISTA SIN IMPUESTOS`,
                                                               `PRECIO DE PROMOCIÓN`),
                                                              ~ parse_number(.x,
                                                                             locale = locale(decimal_mark = ",", grouping_mark = "."))
                                                            ))

##las columnas de precio de "promocion sin impuestos", "precio de conexion total", " precio conexion total sin impuestos", " producto_url " se van a eliminar

personal_celulares <- personal_celulares %>% select( -"PRECIO CONEXIÓN TOTAL SIN IMPUESTOS", -"PRECIO CONEXIÓN TOTAL",-"PRECIO DE PROMOCIÓN SIN IMPUESTOS", -"producto_url")

## armo una columna para aislar la marca individualmente, tomando el dato desde el columna de "nombre"

personal_celulares <- personal_celulares %>% mutate(marca = word(nombre, 1))

personal_celulares <- personal_celulares %>% mutate(marca = recode(marca,"iPhone" = "Apple",
                                                                          .default = marca)
                                                   )

##-----Armo otra columna para aislar el modelo

##La segunda palabra la traemos si o si, y la tercera si es una palabra ( alafanumerico), si es solo numero no se agrega la tarcer

personal_celulares <- personal_celulares %>% 
  mutate(
    segunda = word(nombre, 2),
    tercera = word(nombre, 3),
    modelo = if_else(
      # sólo agrego la tercera si contiene al menos una letra
      str_detect(tercera, "[A-Za-z]"),
      str_c(segunda, tercera, sep = " "),
      segunda
    )
  )%>% 
  select(-segunda, -tercera)

#-----------------------------------------------------------------Apartado de Financiacion-------------------------------------------------------------#


## primero voy a tomar el numero mas grande, nos vamos a quedar con el plazo de financiacion mas grande nada mas. 
##La logica seria, quedarme con los numeros primero y luego filtrar por el que sea mas grande financacion_plazo


personal_celulares <- personal_celulares %>% mutate(financiacion_plazo = map_int( str_extract_all(Financiación, "\\d+"),       # extraigo los numeros
                                                                                         ~ max(as.integer(.x), na.rm = TRUE)         # toma el mayor
                                                                                            )
                                                                                          )
  

personal_celulares <-personal_celulares%>% select( -"Financiación")

#-----------------------------------------------------------------Reordeno las columnas-------------------------------------------------------------#
personal_celulares <- personal_celulares %>%select(
                                                        "sitio",
                                                        "marca",
                                                        "modelo",
                                                        "nombre",
                                                        "Sistema Operativo",
                                                        "Procesador",
                                                        "RAM (GB)",
                                                        "Almacenamiento interno (GB)",
                                                        "Pantalla (Pulgadas)",
                                                        "Cámara Principal (MP)",
                                                        "Cámara frontal (MP)",
                                                        "NFC",
                                                        "PRECIO DE LISTA",
                                                        "PRECIO DE LISTA SIN IMPUESTOS",
                                                        "PRECIO DE PROMOCIÓN",
                                                        "financiacion_plazo"
                                                      )
  


View(personal_celulares)
