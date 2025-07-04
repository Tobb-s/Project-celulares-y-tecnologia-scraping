# ==============================================================================
# SCRIPT DE LIMPIEZA, TRANSFORMACIÓN Y CONSOLIDACIÓN (Versión PORTÁTIL)
# ------------------------------------------------------------------------------
# • Lee los archivos CSV generados por los scripts de scraping desde la carpeta /raw.
# • Limpia, estandariza y enriquece los datos de cada fuente (Personal, Claro, Cetrogar).
# • Guarda los archivos base procesados en la carpeta /input.
# • Consolida las tres fuentes en un único archivo final.
# • Utiliza el paquete 'here' para que todas las rutas sean portátiles.
# ==============================================================================


# 1. Cargar librerías ----------------------------------------------------------
# install.packages(c("readr", "dplyr", "stringr", "purrr", "forcats", "here"))
library(readr)       # read_csv()
library(dplyr)       # si luego quieres manipular datos (opcional)
library(stringr)
library(purrr)
library(forcats)
library(here)        # Paquete para manejar rutas de archivo de forma portátil

# 2. Definir rutas portátiles --------------------------------------------------
# here() localiza la raíz del proyecto (donde está el archivo .Rproj)
raw_dir <- here("raw")
input_dir <- here("input")

# Crear la carpeta 'input' si no existe, para evitar errores al guardar.
if (!dir.exists(input_dir)) {
  dir.create(input_dir)
  message(paste("📁 Carpeta 'input' creada en:", input_dir))
}


#==================================================================================================================================
#> > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > >PERSONAL
#=================================================================================================================================

# 3. Importar el CSV de Personal de forma portátil
personal_csv_path <- file.path(raw_dir, "personal_celulares_multipagina.csv")
personal_celulares <- read_csv(personal_csv_path)


#-----------------------------------------------------Limpieza y Estandarizacion-----------------------------------------

# Limpieza de datos vacios o en blanco

personal_celulares <- personal_celulares %>% mutate( across(c(`Sistema Operativo`, `PRECIO DE LISTA`), ~ na_if(.x, ""))) %>%
  filter(
    !is.na(`Sistema Operativo`),                                                  
    !is.na(`PRECIO DE LISTA`) & `PRECIO DE LISTA`                       != 0,
    !is.na(`PRECIO DE LISTA SIN IMPUESTOS`) & `PRECIO DE LISTA SIN IMPUESTOS`       != 0,
    !is.na(`PRECIO DE PROMOCIÓN`) & `PRECIO DE PROMOCIÓN`               != 0,
    !is.na(`RAM (GB)`) & `RAM (GB)`                       != 0,               
    !is.na(`Almacenamiento interno (GB)`) & `Almacenamiento interno (GB)` != 0,
    !is.na(`Pantalla (Pulgadas)`) & `Pantalla (Pulgadas)`     != 0
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

#-------------------------------------------------------Apartado de Financiacion-------------------------------------------------------------


## primero voy a tomar el numero mas grande, nos vamos a quedar con el plazo de financiacion mas grande nada mas.  
##La logica seria, quedarme con los numeros primero y luego filtrar por el que sea mas grande financacion_plazo


personal_celulares <- personal_celulares %>% mutate(financiacion_plazo = map_int( str_extract_all(Financiación, "\\d+"),       # extraigo los numeros
                                                                                  ~ max(as.integer(.x), na.rm = TRUE)        # toma el mayor
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

##Columna "id" sintetica

personal_celulares <- personal_celulares %>%
  mutate(
    id = str_c(
      marca,
      modelo,
      sep = " - "
    )
  )


#----------------------------------------------Calculos y Estadisticas Iniciales------------------------------------------------

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
      ( Precio_comprador / GB_relativo_price ),
      1
    )
  )

##Precio relativo MPX / Precio_Comprador :: (Camara Principal + Camara Frontal) / Precio_Comprador


personal_celulares <- personal_celulares %>%
  mutate(
    # Suma de MP de ambas cámaras
    mp_total = `Camara Principal (MP)` + `Camara frontal (MP)`,
    # Ratio: megapíxeles totales por unidad de precio comprador
    ratio_mp_precio = round(
      (Precio_comprador / mp_total),
      1
    )
  )


cols_filtrar <- c(
  "per_descuento",
  "presion_impositiva_pais_per",
  "ratio_memoria_precio",
  "ratio_mp_precio"
)

personal_celulares <- personal_celulares %>% 
  filter(
    if_all(all_of(cols_filtrar), ~ .x > 0)
  )

#----------------------------------------------------------Reordeno las columnas-------------------------------------------------------------
personal_celulares <- personal_celulares %>%select(
  "sitio",
  "id",
  "marca",
  "modelo",
  "nombre",
  "Sistema Operativo",
  "Procesador",
  "RAM (GB)",
  "Almacenamiento interno (GB)",
  "Pantalla (Pulgadas)",
  "Camara Principal (MP)",
  "Camara frontal (MP)",
  "NFC",
  "Precio_comprador",
  "Precio_anterior",
  "precio_sin_impuestos",
  "financiacion_plazo",
  "per_descuento",
  "presion_impositiva_pais_per",
  "ratio_memoria_precio",
  "ratio_mp_precio"
)



#----------------------------------------------GUARDADO Y VISUALIZACION PERSONAL-------------------------------------------------------------
# Guardar en la carpeta 'input' del proyecto
write_csv(personal_celulares, file.path(input_dir, "personal_celulares_base.csv"))
View(personal_celulares)



#==================================================================================================================================
#> > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > CLARO
#=================================================================================================================================

# Importar el CSV de Claro de forma portátil
claro_csv_path <- file.path(raw_dir, "claro_celulares_multipagina.csv")
claro_celulares <- read_csv(claro_csv_path)



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


##Renombramos algunas de las columnas
claro_celulares <- claro_celulares %>%
  rename(
    `RAM (GB)`                  = `Memoria RAM`,
    `Almacenamiento interno (GB)` = `Memoria Interna`,
    `Pantalla (Pulgadas)`       = Display,
    `Camara frontal (MP)`       = `Cámara frontal`,
    `Camara Principal (MP)`     = `Cámara trasera`
  )

## Limpio los espacios en blanco o NA

claro_celulares <- claro_celulares %>%mutate(across(c(`Sistema Operativo`, NFC, Procesador),~ na_if(.x, ""))) %>%
  filter(
    !is.na(Precio_comprador)  & Precio_comprador    != 0,
    !is.na(`Camara frontal (MP)`)   & `Camara frontal (MP)`    != 0,
    !is.na(`RAM (GB)`)        & `RAM (GB)`         != 0,
    !is.na(`Almacenamiento interno (GB)`)   & `Almacenamiento interno (GB)`    != 0,
    !is.na(`Camara Principal (MP)`)   & `Camara Principal (MP)`    != 0,
    !is.na(`Sistema Operativo`) & `Sistema Operativo`  != "",
    !is.na(NFC)                 & NFC                  != "",
    !is.na(Procesador)          & Procesador           != ""
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
      c(`Camara Principal (MP)`, `RAM (GB)`),
      ~ parse_number(.x, locale = locale(decimal_mark = ",", grouping_mark = "."))
    ),
    
    `Almacenamiento interno (GB)` = {
      tmp <- parse_number(`Almacenamiento interno (GB)`,
                          locale = locale(decimal_mark = ",", grouping_mark = "."))
      if_else(tmp == 1, 1000, tmp)  ##si 1 ( por ser 1TB) entonces lo reemplazo por 1000 para que todo quede en GB
    }
  )

claro_celulares <- claro_celulares %>%
  mutate(
    `Pantalla (Pulgadas)` = as.numeric(
      str_extract(
        `Pantalla (Pulgadas)`,
        "^[0-9]+\\.?[0-9]*"
      )
    )
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

claro_celulares <- claro_celulares %>%
  mutate(
    # primero convierto a minúscula y quito espacios externos
    NFC_clean = str_to_lower(str_trim(NFC)),
    # luego normalizo
    NFC = case_when(
      NFC_clean %in% c("s", "si", "SI", "sI")  ~ "Si",
      NFC_clean %in% c("n", "no", "NO","nO")  ~ "No",
      TRUE                                      ~ NFC    # deja tal cual (incluye NA)
    )
  ) %>%
  select(-NFC_clean)


##Columna "id" sintetica

claro_celulares <- claro_celulares %>%
  mutate(
    id = str_c(
      marca,
      modelo,
      sep = " - "
    )
  )

#--------------------------------------------- Calculos y Estadisticas Iniciales------------------------------------------------

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
      ( Precio_comprador / GB_relativo_price ),
      1
    )
  )


##Precio relativo MPX / Precio_Comprador :: (Camara Principal + Camara Frontal) / Precio_Comprador

claro_celulares <- claro_celulares %>%
  mutate(
    # Suma de MP de ambas cámaras
    mp_total = `Camara Principal (MP)` + `Camara frontal (MP)`,
    # Ratio: megapíxeles totales por unidad de precio comprador
    ratio_mp_precio = round(
      (Precio_comprador/mp_total),
      1
    )
  )

#----------------------------------------------------------Reordeno las columnas-------------------------------------------------------------
claro_celulares <- claro_celulares %>%select(
  "sitio",
  "id",
  "marca",
  "modelo",
  "nombre",
  "Sistema Operativo",
  "Procesador",
  "RAM (GB)",
  "Almacenamiento interno (GB)",
  "Pantalla (Pulgadas)",
  "Camara Principal (MP)",
  "Camara frontal (MP)",
  "NFC",
  "Precio_comprador",
  "Precio_anterior",
  "precio_sin_impuestos",
  "financiacion_plazo",
  "per_descuento",
  "presion_impositiva_pais_per",
  "ratio_memoria_precio",
  "ratio_mp_precio"
)


#-------------------------------------------------GUARDADO Y VISUALIZACION CLARO-------------------------------------------------------------
# Guardar en la carpeta 'input' del proyecto
write_csv(claro_celulares, file.path(input_dir, "claro_celulares_base.csv"))
View(claro_celulares)



#======================================================================================================================================
#                                                     CENTROGAR
#======================================================================================================================================
# Importar el CSV de Cetrogar de forma portátil
cetrogar_csv_path  <- file.path(raw_dir, "centrohogar_celulares_multipagina.csv")

# Importar y renombrar columnas base
cetrogar_celulares <- read_csv(cetrogar_csv_path, show_col_types = FALSE) %>%
  # 2.1 Eliminar columnas que no usaré (solo aquellas que existan)
  select(-any_of(c(
    "producto_url", "fecha_scrapeo", "Tipo de pantalla", "Resistente al agua",
    "Velocidad de la CPU", "Batería", "Dual SIM", "Memoria externa incluida",
    "Tecnología AI", "Carga rápida", "Lector de huella", "Conexión Bluetooth",
    "GPS integrado", "Información adicional"
  ))) %>%
  # 2.2 Renombrar todas las columnas clave de una sola vez
  rename(
    Precio_anterior         = precio_lista_habitual,
    Precio_comprador        = precio_oferta_final,
    precio_sin_impuestos    = precio_sin_impuestos,
    `RAM (GB)`              = memoria_ram,
    `Camara frontal (MP)`   = camara_frontal,
    `Camara Principal (MP)` = `Cámara trasera`,
    `Pantalla (Pulgadas)`   = `Tamaño de la pantalla`,
    Procesador              = `Tipo de procesador`
  ) %>%
  
  # 3. Formatear precios y filtrar precios anteriores > 100000
  mutate(
    across(
      c(Precio_comprador, Precio_anterior, precio_sin_impuestos),
      ~ parse_number(.x, locale = locale(decimal_mark = ",", grouping_mark = "."))
    )
  ) %>%
  filter(Precio_anterior > 100000) %>%
  
  # 4. Extraer y convertir a numeric RAM, cámaras y pantalla
  mutate(
    `RAM (GB)`              = str_extract(`RAM (GB)`,              "\\d+")        %>% as.numeric(),
    `Camara frontal (MP)`   = str_extract(`Camara frontal (MP)`,  "\\d+")        %>% as.numeric(),
    `Camara Principal (MP)` = str_extract(`Camara Principal (MP)`, "\\d+")        %>% as.numeric(),
    `Pantalla (Pulgadas)`   = str_extract(`Pantalla (Pulgadas)`,   "\\d+\\.?\\d*") %>% as.numeric()
  ) %>%
  
  # 5. Normalizar NFC
  mutate(
    NFC_clean = str_to_lower(str_trim(NFC)),
    NFC = case_when(
      NFC_clean %in% c("s", "si", "sí") ~ "Si",
      NFC_clean %in% c("n", "no")       ~ "No",
      TRUE                             ~ NFC
    )
  ) %>%
  select(-NFC_clean) %>%
  
  # 6. Agregar sitio fijo
  mutate(sitio = "Cetrogar") %>%
  
  # 7. Limpiar y enriquecer nombre, marca y modelo
  mutate(
    # Quitar prefijo "Celular "
    nombre = str_remove(nombre, "^Celular\\s+"),
    # Prefijos según marca en el nombre
    nombre = case_when(
      str_detect(nombre, "^Neo")   ~ str_c("Nubia ", nombre),
      str_detect(nombre, "^Spark") ~ str_c("Tecno Mobile ", nombre),
      TRUE                        ~ nombre
    ),
    # Marca = primera palabra
    marca  = word(nombre, 1),
    # Modelo = segunda + tercera (si existe y no contiene comillas)
    segunda = word(nombre, 2),
    tercera = word(nombre, 3),
    modelo  = if_else(
      !is.na(tercera) & tercera != "" & !str_detect(tercera, '"'),
      str_c(segunda, tercera, sep = " "),
      segunda
    )
  ) %>%
  select(-segunda, -tercera) %>%
  
  # 8. Extraer Almacenamiento interno (GB) desde el nombre
  mutate(
    almacenamiento_raw = str_extract_all(nombre, "\\d+GB") %>% map_chr(~ .x[2] %||% NA_character_),
    `Almacenamiento interno (GB)` = almacenamiento_raw %>% str_remove("GB") %>% as.numeric()
  ) %>%
  select(-almacenamiento_raw)

# 9. Columnas vacías para homogeneizar con otros orígenes
cetrogar_celulares <- cetrogar_celulares %>%
  mutate(
    `Sistema Operativo` = NA_character_,
    financiacion_plazo  = NA_character_
  ) %>%
  
  # 10. Cálculos y ratios finales (con nueva fórmula de memoria)
  mutate(
    per_descuento               = round((1 - Precio_comprador / Precio_anterior) * 100, 0),
    presion_impositiva_pais_per  = round((1 - precio_sin_impuestos / Precio_anterior) * 100, 0),
    GB_relativo_price_count      = `RAM (GB)` + `Almacenamiento interno (GB)`,
    ratio_memoria_precio         = round((Precio_comprador /GB_relativo_price_count), 1),
    mp_total                     = `Camara Principal (MP)` + `Camara frontal (MP)`,
    ratio_mp_precio              = round(Precio_comprador/mp_total, 1)
  ) %>%
  
  # 11. Crear columna "id" y reordenar columnas
  mutate(
    id = str_c(
      marca,
      modelo,
      sep = " - "
    )
  ) %>%
  select(
    sitio, id, marca, modelo, nombre,
    `Sistema Operativo`, Procesador,
    `RAM (GB)`, `Almacenamiento interno (GB)`, `Pantalla (Pulgadas)`,
    `Camara Principal (MP)`, `Camara frontal (MP)`, NFC,
    Precio_comprador, Precio_anterior, precio_sin_impuestos,
    financiacion_plazo, per_descuento, presion_impositiva_pais_per, ratio_memoria_precio, ratio_mp_precio
  ) %>%
  
  # 12. Filtrar valores no positivos
  filter(
    ratio_memoria_precio > 0,
    ratio_mp_precio      > 0,
    per_descuento        > 0
  )

## rearmo la columna de "Procesador" en Cetrogar para que tenga un dato consistente

# a). Tabla de referencia con los procesadores "buenos" -------------
proc_lookup <- bind_rows(
  personal_celulares,
  claro_celulares
) %>% 
  filter(!is.na(Procesador) & Procesador != "") %>%    # sólo los que tengan dato
  distinct(id, .keep_all = TRUE) %>%              # un Procesador por id
  select(id, Procesador)                          # (id, Procesador) limpio

# b). Combinar con Cetrogar y reemplazar -----------------------------
cetrogar_celulares <- cetrogar_celulares %>% 
  left_join(proc_lookup, by = "id", suffix = c("", "_nuevo")) %>% 
  mutate(
    # si viene Procesador_nuevo úsalo; si no, deja el original
    Procesador = coalesce(Procesador_nuevo, Procesador)
  ) %>% 
  select(-Procesador_nuevo)  # descarto la columna auxiliar

## rearmo la columna de "Sistema Operativo" en Cetrogar para que tenga un dato consistente


proc_so_lookup <- bind_rows(
  personal_celulares,
  claro_celulares
) %>%
  filter(
    (!is.na(Procesador)        & Procesador        != "") |
      (!is.na(`Sistema Operativo`) & `Sistema Operativo` != "")
  ) %>%                       # al menos uno de los dos campos informado
  distinct(id, .keep_all = TRUE) %>%    # un único registro por id
  select(id, Procesador, `Sistema Operativo`)   # columnas que necesitamos

# 2. Unir con Cetrogar y completar faltantes
cetrogar_celulares <- cetrogar_celulares %>%
  left_join(proc_so_lookup, by = "id",
            suffix = c("", "_nuevo")) %>%      # añade *_nuevo para lookup
  mutate(
    Procesador          = coalesce(Procesador_nuevo, Procesador),
    `Sistema Operativo` = coalesce(`Sistema Operativo_nuevo`, `Sistema Operativo`)
  ) %>%
  select(-Procesador_nuevo, -`Sistema Operativo_nuevo`)



# 13. Guardar resultado final
write_csv(
  cetrogar_celulares,
  file.path(input_dir, "cetrogar_celulares_base.csv"),
  na = ""
)

View(cetrogar_celulares)
#==============================================================================================================================================
#                                                         CONSOLIDADO DE ARCHIVOS
#==============================================================================================================================================

# ── 1. Preprocesamiento individual ───────────────────────────────────────
personal_celulares <- personal_celulares %>% 
  mutate(
    financiacion_plazo          = as.integer(financiacion_plazo),
    presion_impositiva_pais_per = as.numeric(presion_impositiva_pais_per)
  )

claro_celulares <- claro_celulares %>% 
  mutate(
    financiacion_plazo          = as.integer(financiacion_plazo),
    presion_impositiva_pais_per = as.numeric(presion_impositiva_pais_per)
  )

cetrogar_celulares <- cetrogar_celulares %>% 
  mutate(
    financiacion_plazo          = as.integer(financiacion_plazo),
    presion_impositiva_pais_per = as.numeric(presion_impositiva_pais_per)
  )

# ── 2. Unir fuentes y calcular costo unitario total ──────────────────────
combined_celulares <- bind_rows(
  personal_celulares,
  claro_celulares,
  cetrogar_celulares
) %>%
  # Columna final: suma de ratio_memoria_precio + ratio_mp_precio
  mutate(
    costo_unitario_total = ratio_memoria_precio + ratio_mp_precio
  )

# ── 3. Índice de categorización por mayoría de gamas ─────────────────────
combined_celulares <- combined_celulares %>%
  # 3.1 Gamas parciales
  mutate(
    Gama_SO = case_when(
      str_detect(`Sistema Operativo`, regex("^iOS", ignore_case = TRUE)) ~ "Gama alta",
      `Sistema Operativo` == "Android 15"                             ~ "Gama alta",
      `Sistema Operativo` %in% c("Android 14", "Android 13")           ~ "Gama media",
      `Sistema Operativo` %in% c("Android 14 Go", "Android 13 Go")     ~ "Gama baja",
      TRUE                                                            ~ NA_character_
    ),
    Gama_NFC = case_when(
      NFC == "Si" ~ "Gama alta",
      NFC == "No" ~ "Gama baja",
      TRUE        ~ NA_character_
    ),
    Gama_RAM = case_when(
      `RAM (GB)` <= 3              ~ "Gama baja",
      `RAM (GB)` %in% c(4, 6)      ~ "Gama media",
      `RAM (GB)` %in% c(8, 12, 16) ~ "Gama alta",
      TRUE                         ~ NA_character_
    ),
    Gama_Almacenamiento = case_when(
      `Almacenamiento interno (GB)` <= 64         ~ "Gama baja",
      `Almacenamiento interno (GB)` %in% c(128, 256) ~ "Gama media",
      `Almacenamiento interno (GB)` %in% c(512, 1000)~ "Gama alta",
      TRUE                                          ~ NA_character_
    ),
    Gama_Camara = case_when(
      `Camara Principal (MP)` < 10   ~ "Gama baja",
      `Camara Principal (MP)` <= 32  ~ "Gama media",
      `Camara Principal (MP)` > 32   ~ "Gama alta",
      TRUE                           ~ NA_character_
    ),
    across(starts_with("Gama_"), ~ fct_relevel(.x, "Gama baja", "Gama media", "Gama alta"))
  ) %>%
  # 3.2 Categorización final por mayoría
  mutate(
    Categoria = case_when(
      rowSums(across(starts_with("Gama_"), ~ .x == "Gama alta"),  na.rm = TRUE) >= 3 ~ "Gama alta",
      rowSums(across(starts_with("Gama_"), ~ .x == "Gama media"), na.rm = TRUE) >= 3 ~ "Gama media",
      rowSums(across(starts_with("Gama_"), ~ .x == "Gama baja"),  na.rm = TRUE) >= 3 ~ "Gama baja",
      TRUE                                                                          ~ "Gama media"
    )
  ) %>%
  # 3.3 Limpiar columnas auxiliares
  select(
    -starts_with("Gama_")
  )

# ── 4. Guardado y visualización ──────────────────────────────────────────
View(combined_celulares)

# Guardar el archivo consolidado final en la carpeta 'input'
write_csv(
  combined_celulares,
  file.path(input_dir, "celulares_todos_base.csv")
)
