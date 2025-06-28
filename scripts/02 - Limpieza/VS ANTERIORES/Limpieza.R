
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


#-----------------------------------------------------------------Limpieza y Estandarizacion-----------------------------------------

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
      !is.na(tercera) & tercera != "",
      str_c(segunda, tercera, sep = " "),
      segunda
    )
  ) %>%
  select(-segunda, -tercera)

##Todos los celulares de Apple tienen NFC asi que...

personal_celulares <- personal_celulares %>%
  mutate(
    NFC = if_else(
      marca == "Apple" & is.na(NFC),
      "Si",
      NFC
    )
  )

##Normalizacion de NFC

personal_celulares <- personal_celulares %>%
  mutate(
    # primero convierto a minúscula y quito espacios externos
    NFC_clean = str_to_lower(str_trim(NFC)),
    # luego normalizo
    NFC = case_when(
      NFC_clean %in% c("s", "si")  ~ "Si",
      NFC_clean %in% c("n", "no")  ~ "No",
      TRUE                          ~ NFC    # deja tal cual (incluye NA)
    )
  ) %>%
  select(-NFC_clean)

#-----------------------------------------------------------------Apartado de Financiacion-------------------------------------------------------------#


## primero voy a tomar el numero mas grande, nos vamos a quedar con el plazo de financiacion mas grande nada mas. 
##La logica seria, quedarme con los numeros primero y luego filtrar por el que sea mas grande financacion_plazo


personal_celulares <- personal_celulares %>% mutate(financiacion_plazo = map_int( str_extract_all(Financiación, "\\d+"),       # extraigo los numeros
                                                                                         ~ max(as.integer(.x), na.rm = TRUE)         # toma el mayor
                                                                                            )
                                                                                          )
  

personal_celulares <-personal_celulares%>% select( -"Financiación")

##Renombro algunas columnas 

personal_celulares <- personal_celulares %>%
  rename(
    Precio_comprador     = `PRECIO DE PROMOCIÓN`,
    Precio_anterior      = `PRECIO DE LISTA`,
    precio_sin_impuestos = `PRECIO DE LISTA SIN IMPUESTOS`
  ) %>%
  mutate(
    across(
      c(Precio_comprador, Precio_anterior, precio_sin_impuestos),
      ~ parse_number(
        as.character(.x),
        locale = locale(decimal_mark = ".", grouping_mark = ",")
      )
    )
  )

#----------------------------------------------------------------- Calculos y Estadisticas Iniciales------------------------------------------------

##Promociones:: Siendo Precio Comprador el precio mas bajo de los existentes, si no existe promocion, el Precio_comprador = Precio_anterior
personal_celulares <- personal_celulares %>%
  mutate(
    per_descuento = round((1 - Precio_comprador / Precio_anterior) * 100, 0))

##Presion Impositva a nivel pais

personal_celulares <- personal_celulares %>%
  mutate(
    presion_impositiva_pais_per = paste0(
      round((1 - precio_sin_impuestos / Precio_anterior) * 100, 0)
    )
  )

##Precio relativo GB / Precio_Comprador :: (RAM + almacenamiento interno) / Precio_Comprador


personal_celulares <- personal_celulares %>%
  mutate(
    GB_relativo_price = `RAM (GB)` + `Almacenamiento interno (GB)`,
    ratio_memoria_precio = round(
      GB_relativo_price / Precio_comprador,
      1
    )
  )

##Precio relativo MPX / Precio_Comprador :: (Camara Principal + Camara Frontal) / Precio_Comprador


personal_celulares <- personal_celulares %>%
  mutate(
    # Suma de MP de ambas cámaras
    mp_total = `Cámara Principal (MP)` + `Cámara frontal (MP)`,
    # Ratio: megapíxeles totales por unidad de precio comprador
    ratio_mp_precio = round(
      ((mp_total / Precio_comprador)*10000),
      1
    )
  )
  
#-----------------------------------------------------------------Reordeno las columnas-------------------------------------------------------------
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
                                                        "Precio_comprador",
                                                        "Precio_anterior",
                                                        "precio_sin_impuestos",
                                                        "financiacion_plazo",
                                                        "per_descuento",
                                                        "presion_impositiva_pais_per",
                                                        "GB_relativo_price",
                                                        "ratio_mp_precio"
                                                      )
  


#----------------------------------------------GUARDADO Y VISUALIZACION PERSONAL-------------------------------------------------------------
# Ruta base a la carpeta “input”
input_dir_PERSONAL <- "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/input"
write_csv(personal_celulares,file.path(input_dir_PERSONAL, "personal_celulares_base.csv"))

View(personal_celulares)

sapply(personal_celulares[, c("Precio_comprador", "Precio_anterior", "precio_sin_impuestos")],
       class)

#==================================================================================================================================
#                                                         CLARO
#=================================================================================================================================

# 2. Definir carpeta y archivo 
dir_path  <- "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/raw"
file_name <- "claro_celulares_multipagina.csv"   

# 3. Construir la ruta completa 
csv_path <- file.path(dir_path, file_name)

# 4. Importar el CSV 
claro_celulares <- read_csv(csv_path)

#-----------------------------------------------------Limpieza y Estandarizacion--------------------------------------------------


## Elimina las columnas que no voy a usar

claro_celulares <- claro_celulares %>% select( -"producto_url",
                                               -"Modelo",
                                               -"Tipo de SIM (no incluida)",
                                               -"Red",
                                               -"Frecuencia 2G",
                                               -"Frecuencia 3G",
                                               -"Frecuencia 4G",
                                               -"Frecuencia 5G",
                                               -"Batería",
                                               -"Peso",
                                               -"Dimensión del equipo",
                                               -"Llamadas por WiFi",
                                               -"Sensores",
                                               -"Celular Liberado",
                                               -"Memoria Externa",
                                               -"Cargador",
                                               -"Características",
                                               -"Auriculares",
                                               -"Cable USB C",
                                               -"Cover",
                                               -"Otros")

## Limpio los espacios en blanco o NA

claro_celulares <- claro_celulares %>%mutate(across(c(`Sistema Operativo`, NFC, Procesador),~ na_if(.x, ""))) %>%
                                      filter(
                                        !is.na(Precio_comprador)     & Precio_comprador     != 0,
                                        !is.na(`Cámara frontal`)     & `Cámara frontal`     != 0,
                                        !is.na(`Memoria RAM`)        & `Memoria RAM`        != 0,
                                        !is.na(`Memoria Interna`)    & `Memoria Interna`    != 0,
                                        !is.na(`Cámara trasera`)     & `Cámara trasera`     != 0,
                                        !is.na(`Sistema Operativo`)  & `Sistema Operativo`  != "",
                                        !is.na(NFC)                  & NFC                  != "",
                                        !is.na(Procesador)           & Procesador           != ""
                                      )



##Le agrego al nombre la marca " Motorola" en caso de que arranquen solo con "Moto "

claro_celulares <- claro_celulares %>% mutate(
    nombre = if_else(
      str_starts(nombre, "Moto "),
      str_c("Motorola ", nombre),
      nombre
    )
  )

##creo la columna de "marca"

claro_celulares <- claro_celulares %>%mutate( marca = word(nombre, 1))

##Me quedo solo con el primero numero del campo

claro_celulares <- claro_celulares %>%
  mutate(
    across(
      c(`Cámara trasera`, `Memoria RAM`),
      ~ parse_number(.x, locale = locale(decimal_mark = ",", grouping_mark = "."))
    ),
    
    `Memoria Interna` = {
      tmp <- parse_number(`Memoria Interna`,
                          locale = locale(decimal_mark = ",", grouping_mark = "."))
      if_else(tmp == 1, 1000, tmp)  ##si 1 ( por ser 1TB) entonces lo reemplazo por 1000 para que todo quede en GB
    }
  )

claro_celulares <- claro_celulares %>%
  mutate(
    Display = as.numeric(str_extract(Display, "^[0-9]+\\.?[0-9]*"))
  )

## creo la columna de modelo

claro_celulares <- claro_celulares %>%
  mutate(
    segunda = word(nombre, 2),
    tercera = word(nombre, 3),
    modelo = if_else(
      !is.na(tercera) & tercera != "",
      str_c(segunda, tercera, sep = " "),
      segunda
    )
  ) %>%
  select(-segunda, -tercera)

##simplifico la infor de los procesadores 


claro_celulares <- claro_celulares %>%
  mutate(
    Procesador = str_extract(Procesador, ".*?GHz")
  )

##Limpieza de columna de "financiacion_plazo" - Hay que quedarse con los numeros y luego, con el mas alto

claro_celulares <- claro_celulares %>%
  mutate(
    nums_list = str_extract_all(financiacion_plazo, "\\d+"),
    financiacion_plazo = map_int(
      nums_list,
      ~ if (length(.x) > 0) max(as.integer(.x)) else NA_integer_
    )
  ) %>%
  select(-nums_list)


##Renombramos algunas de las columnas
claro_celulares <- claro_celulares %>%
  rename(
    `RAM (GB)`                    = `Memoria RAM`,
    `Almacenamiento interno (GB)` = `Memoria Interna`,
    `Pantalla (Pulgadas)`         = Display,
    `Cámara frontal (MP)`         = `Cámara frontal`,
    `Cámara Principal (MP)`       = `Cámara trasera`
  )

##Relleno los valores NA en la columna " Precio_anterior" con el "Precio_comprador"
claro_celulares <- claro_celulares %>%
  mutate(
    Precio_anterior = if_else(
      is.na(Precio_anterior),
      Precio_comprador,
      Precio_anterior
    )
  )

claro_celulares <- claro_celulares %>%
  # — tras tu rename() y el mutate() que rellena NA en Precio_anterior —
  mutate(
    across(
      c(Precio_comprador, Precio_anterior, precio_sin_impuestos),
      ~ parse_number(
        .x,
        locale = locale(decimal_mark = ",", grouping_mark = ".")
      )
    )
  )


##Todos los celulares de Apple tienen NFC asi que...

claro_celulares <- claro_celulares %>%
  mutate(
    NFC = if_else(
      marca == "Apple" & is.na(NFC),
      "Si",
      NFC
    )
  )


##Normalizacion de NFC

personal_celulares <- personal_celulares %>%
  mutate(
    # primero convierto a minúscula y quito espacios externos
    NFC_clean = str_to_lower(str_trim(NFC)),
    # luego normalizo
    NFC = case_when(
      NFC_clean %in% c("s", "si", "SI", "sI")  ~ "Si",
      NFC_clean %in% c("n", "no", "NO","nO")  ~ "No",
      TRUE                          ~ NFC    # deja tal cual (incluye NA)
    )
  ) %>%
  select(-NFC_clean)

#----------------------------------------------------------------- Calculos y Estadisticas Iniciales------------------------------------------------

##Promociones:: Siendo Precio Comprador el precio mas bajo de los existentes, si no existe promocion, el Precio_comprador = Precio_anterior

claro_celulares <- claro_celulares %>%
  mutate(
    per_descuento = round((1 - Precio_comprador / Precio_anterior) * 100, 0)
  )


##Presion Impositva a nivel pais
    
claro_celulares <- claro_celulares %>%
  mutate(
    presion_impositiva_pais_per = paste0(
      round((1 - precio_sin_impuestos / Precio_anterior) * 100, 0)
    )
  )


##Precio relativo GB / Precio_Comprador :: (RAM + almacenamiento interno) / Precio_Comprador


claro_celulares <- claro_celulares %>%
  mutate(
    GB_relativo_price = `RAM (GB)` + `Almacenamiento interno (GB)`,
    ratio_memoria_precio = round(
      GB_relativo_price / Precio_comprador,
      1
    )
  )


##Precio relativo MPX / Precio_Comprador :: (Camara Principal + Camara Frontal) / Precio_Comprador

claro_celulares <- claro_celulares %>%
  mutate(
    # Suma de MP de ambas cámaras
    mp_total = `Cámara Principal (MP)` + `Cámara frontal (MP)`,
    # Ratio: megapíxeles totales por unidad de precio comprador
    ratio_mp_precio = round(
      ((mp_total / Precio_comprador)*10000),
      1
    )
  )

#-----------------------------------------------------------------Reordeno las columnas-------------------------------------------------------------
claro_celulares <- claro_celulares %>%select(
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
                                              "Precio_comprador",
                                              "Precio_anterior",
                                              "precio_sin_impuestos",
                                              "financiacion_plazo",
                                              "per_descuento",
                                              "presion_impositiva_pais_per",
                                              "GB_relativo_price",
                                              "ratio_mp_precio"
                                            )


#----------------------------------------------GUARDADO Y VISUALIZACION CLARO-------------------------------------------------------------
# Ruta base a la carpeta “input”
input_dir_CLARO <- "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/input"
write_csv(claro_celulares,file.path(input_dir_CLARO, "claro_celulares_base.csv"))



View(claro_celulares)



#===========================================================================================================================================
#                                                              REPORTE UNIFICADO
#===========================================================================================================================================

# 1) Une personal arriba y claro abajo
combined_celulares <- bind_rows(
  personal_celulares,
  claro_celulares
)

#-----------------------------------------------Indice de Categorizacion------------------------------------------------------------------


library(dplyr)
library(forcats)
library(stringr)

combined_celulares <- combined_celulares %>%
  # 1. Calcular las gamas individuales
  mutate(
    Gama_SO = case_when(
      str_detect(`Sistema Operativo`, regex("^iOS", ignore_case = TRUE)) ~ "Gama alta",
      `Sistema Operativo` == "Android 15"                                ~ "Gama alta",
      `Sistema Operativo` %in% c("Android 14", "Android 13")             ~ "Gama media",
      `Sistema Operativo` %in% c("Android 14 Go", "Android 13 Go")       ~ "Gama baja",
      TRUE                                                               ~ NA_character_
    ),
    Gama_NFC = case_when(
      NFC == "Si" ~ "Gama alta",
      NFC == "No" ~ "Gama baja",
      TRUE        ~ NA_character_
    ),
    Gama_RAM = case_when(
      `RAM (GB)` <= 3                    ~ "Gama baja",
      `RAM (GB)` %in% c(4, 6)            ~ "Gama media",
      `RAM (GB)` %in% c(8, 12, 16)       ~ "Gama alta",
      TRUE                               ~ NA_character_
    ),
    Gama_Almacenamiento = case_when(
      `Almacenamiento interno (GB)` <= 64            ~ "Gama baja",
      `Almacenamiento interno (GB)` %in% c(128, 256) ~ "Gama media",
      `Almacenamiento interno (GB)` %in% c(512, 1000)~ "Gama alta",
      TRUE                                           ~ NA_character_
    ),
    Gama_Camara = case_when(
      `Cámara Principal (MP)` < 10                                ~ "Gama baja",
      `Cámara Principal (MP)` <= 32                               ~ "Gama media",
      `Cámara Principal (MP)` > 32                                ~ "Gama alta",
      TRUE                                                       ~ NA_character_
    ),
    # opcional: ordenar los niveles para consistencia
    across(starts_with("Gama_"), ~ fct_relevel(.x, "Gama baja", "Gama media", "Gama alta"))
  ) %>%
  # 2. Calcular categoría global según mayoría de gamas
  mutate(
    Categoria = case_when(
      rowSums(across(starts_with("Gama_"), ~ .x == "Gama alta"),  na.rm = TRUE) >= 3 ~ "Gama alta",
      rowSums(across(starts_with("Gama_"), ~ .x == "Gama media"), na.rm = TRUE) >= 3 ~ "Gama media",
      rowSums(across(starts_with("Gama_"), ~ .x == "Gama baja"),  na.rm = TRUE) >= 3 ~ "Gama baja",
      TRUE                                                                            ~ "Gama media"
    )
  ) %>%
  # 3. Eliminar columnas auxiliares dejando sólo la clasificación
  select(-"Gama_SO",-"Gama_NFC",-"Gama_RAM",-"Gama_Almacenamiento",-"Gama_Camara")




#----------------------------------------------GUARDADO Y VISUALIZACION CLARO-------------------------------------------------------------

#Opcional: inspecciona
View(combined_celulares)

# Guarda en CSV
write_csv(
  combined_celulares,
  file.path(input_dir_PERSONAL, "celulares_todos_base.csv")
)

