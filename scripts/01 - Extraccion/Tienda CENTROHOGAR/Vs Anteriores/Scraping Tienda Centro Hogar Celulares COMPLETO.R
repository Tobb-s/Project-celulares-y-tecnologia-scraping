# ==============================================================================
# SCRAPING "Cetrogar - Celulares" (Versión 2.8 - FINAL)
# ------------------------------------------------------------------------------
# • Lógica de scraping optimizada para MÁXIMA VELOCIDAD.
# • FASE 1: Recolecta todas las URLs de productos primero.
# • FASE 2: Itera sobre las URLs recolectadas para extraer los datos.
# • Función de extracción de precios mejorada para capturar todos los valores.
# ==============================================================================

# 1. LIBRERÍAS
# install.packages(c("tidyverse", "rvest", "RSelenium", "xml2", "stringr"))
library(tidyverse)
library(rvest)
library(RSelenium)
library(xml2)
library(stringr)

# 2. CONFIGURACIÓN
CFG <- list(
  home_url = "https://www.cetrogar.com.ar/",
  cel_fallback_url = "https://www.cetrogar.com.ar/tecnologia/celulares-accesorios/smartphones.html",
  out_csv = "C:/Users/valen/OneDrive - Económicas - UBA/=ANALISIS DE DATO PARA LA ECONOMIA Y NEGOCIOS=/Project-celulares-y-tecnologia-scraping/raw/centrohogar_celulares_multipagina.csv"
)

# 3. FUNCIÓN AUXILIAR
#' Extrae la ficha técnica completa de la página de un producto.
extraer_datos_producto <- function(remote_driver) {
  html_content <- read_html(remote_driver$getPageSource()[[1]])
  
  # Extraer datos básicos
  nombre_prod <- html_content %>% html_element("h1.page-title span.base") %>% html_text2()
  
  # --- LÓGICA DE PRECIOS MEJORADA ---
  # Precio final/oferta (el que paga el cliente)
  precio_oferta_final <- html_content %>%
    html_element("span[id^='price-including-tax-product-price-'] > .price") %>%
    html_text2()
  
  # Precio de lista/habitual (el tachado, si existe)
  precio_lista_habitual <- html_content %>%
    html_element("span[id^='price-including-tax-old-price-'] > .price") %>%
    html_text2()
  
  # Precio sin impuestos
  precio_sin_impuestos <- html_content %>%
    html_element("span[id^='price-excluding-tax-product-price-'] > .price") %>%
    html_text2()
  
  # Extraer "Memoria RAM" y "Cámara Frontal" de las características destacadas
  destacadas_items <- html_content %>% html_elements("div.destacada-item")
  
  memoria_ram <- NA_character_
  camara_frontal <- NA_character_
  
  if (length(destacadas_items) > 0) {
    for (item in destacadas_items) {
      label <- item %>% html_element(".label") %>% html_text2()
      value <- item %>% html_element(".value") %>% html_text2()
      
      if (!is.na(label) && label == "Memoria RAM") {
        memoria_ram <- value
      }
      if (!is.na(label) && label == "Cámara Frontal") {
        camara_frontal <- value
      }
    }
  }
  
  # Extraer especificaciones de las tablas "Detalles" y "Tecnología"
  specs_list <- list()
  secciones <- html_content %>%
    html_elements("div.especificadiones_generales .general-content")
  
  if (length(secciones) > 0) {
    specs_list <- map(secciones, function(seccion) {
      titulo_seccion <- seccion %>% html_element("h3.especificadiones-title") %>% html_text2()
      if (!is.na(titulo_seccion) && titulo_seccion %in% c("Detalles", "Tecnología")) {
        table <- seccion %>% html_element("table.additional-attributes")
        if (!is.na(table)) {
          rows <- table %>% html_elements("tr")
          map_df(rows, function(row) {
            label <- row %>% html_element("th.label") %>% html_text2()
            value <- row %>% html_element("td.data") %>% html_text2()
            tibble(label = str_trim(label), value = str_trim(value))
          })
        }
      }
    })
  }
  
  # Consolidar datos de las tablas
  all_specs <- tibble()
  if (length(specs_list) > 0) {
    all_specs <- bind_rows(specs_list) %>%
      filter(!is.na(label) & label != "" & !is.na(value)) %>%
      distinct(label, .keep_all = TRUE) %>%
      pivot_wider(names_from = label, values_from = value)
  }
  
  # Unir toda la información
  base_info <- tibble(
    producto_url = remote_driver$getCurrentUrl()[[1]],
    nombre = nombre_prod,
    precio_lista_habitual = precio_lista_habitual,
    precio_oferta_final = precio_oferta_final,
    precio_sin_impuestos = precio_sin_impuestos,
    memoria_ram = memoria_ram,
    camara_frontal = camara_frontal,
    fecha_scrapeo = Sys.Date()
  )
  
  if (nrow(all_specs) > 0) {
    return(bind_cols(base_info, all_specs))
  } else {
    return(base_info)
  }
}

# 4.---------------------------------------------------------INICIALIZAR SELENIUM-------------------------------------------------------
driver <- rsDriver(browser = "firefox", port = 4445L, chromever = NULL, check = FALSE)
remote_driver <- driver$client
remote_driver$setTimeout("page load", 20000)

# 5. NAVEGACIÓN DESDE LA HOME
message("➡️ Navegando a la página principal de Cetrogar...")
remote_driver$navigate(CFG$home_url)
Sys.sleep(3)

tryCatch({
  cookie_button <- remote_driver$findElement("css selector", ".agree-button")
  if (!is.null(cookie_button)) {
    cookie_button$clickElement()
    message("✅ Cookies aceptadas.")
    Sys.sleep(1)
  }
}, error = function(e) {
  message("ℹ️ No se encontró el banner de cookies.")
})

tryCatch({
  message("➡️ Buscando y haciendo clic en la sección 'Celulares'...")
  xpath_selector <- "//div[@class='link-container']/a[@title='Celulares']"
  celulares_link <- remote_driver$findElement("xpath", xpath_selector)
  remote_driver$executeScript("arguments[0].scrollIntoView(true);", list(celulares_link))
  Sys.sleep(0.5)
  celulares_link$clickElement()
  Sys.sleep(3)
  message("✅ Clic en 'Celulares' exitoso.")
}, error = function(e) {
  warning("⚠️ No se pudo hacer clic en 'Celulares'. Usando URL de fallback.")
  message("Error: ", e$message)
  remote_driver$navigate(CFG$cel_fallback_url)
})

# 6. BUCLE PRINCIPAL DE SCRAPING (LÓGICA RÁPIDA DE DOS FASES)

# --- FASE 1: Recolectar todas las URLs de los productos ---
message("➡️ FASE 1: Recolectando todas las URLs de productos...")
all_product_urls <- c()
pagina_actual <- 1
continuar_paginando <- TRUE # Bandera para controlar el bucle
repeat {
  if (!continuar_paginando) {
    break # Rompe el bucle si la bandera es FALSE
  }
  
  message(paste0("📄 Recolectando URLs de la página de listado N°: ", pagina_actual))
  
  # Obtener los enlaces de producto en la página actual
  product_links <- remote_driver$findElements("css selector", "a.product-item-info")
  
  if (length(product_links) == 0) {
    message("⚠️ No se encontraron más productos en la página.")
    break
  }
  
  urls_pagina <- map_chr(product_links, ~ .x$getElementAttribute("href")[[1]])
  all_product_urls <- c(all_product_urls, urls_pagina)
  message(paste("🔎", length(urls_pagina), "URLs recolectadas en esta página. Total acumulado:", length(all_product_urls)))
  
  # Buscar el botón "Siguiente" y hacer clic
  tryCatch({
    next_button <- remote_driver$findElement("css selector", "li.pages-item-next > a.next")
    message("➡️ Pasando a la siguiente página...")
    remote_driver$executeScript("arguments[0].click();", list(next_button))
    Sys.sleep(3)
    pagina_actual <- pagina_actual + 1
  }, error = function(e) {
    message("✅ Fin de la paginación. No se encontró el botón 'Siguiente'.")
    continuar_paginando <<- FALSE # Cambia la bandera para detener el bucle
  })
}

# --- FASE 2: Visitar cada URL y extraer los datos ---
product_urls_unicas <- unique(all_product_urls)
message(paste("\n➡️ FASE 2: Se iniciará la extracción de datos de", length(product_urls_unicas), "productos únicos."))
scraped_data <- list()

for (i in seq_along(product_urls_unicas)) {
  current_url <- product_urls_unicas[i]
  message(paste("✨ Procesando producto", i, "de", length(product_urls_unicas), ":", basename(current_url)))
  
  tryCatch({
    # Navegar directamente a la URL del producto
    remote_driver$navigate(current_url)
    Sys.sleep(2)
    
    # Extraer los datos
    fila <- extraer_datos_producto(remote_driver)
    scraped_data <- append(scraped_data, list(fila))
    
  }, error = function(e){
    warning(paste("❗️ Error procesando la URL", current_url, "-", e$message))
    # Agregar una fila de error para no perder la URL pero continuar con las demás
    scraped_data <- append(scraped_data, list(tibble(producto_url = current_url, nombre = "ERROR EN EXTRACCIÓN")))
  })
}

# 7. CONSOLIDAR Y GUARDAR
if (length(scraped_data) > 0) {
  # Se eliminan productos duplicados por si se scrapeó alguno más de una vez
  data_final <- bind_rows(scraped_data) %>% distinct(producto_url, .keep_all = TRUE)
  
  message(paste("\n➡️ Guardando", nrow(data_final), "productos únicos en el archivo CSV..."))
  tryCatch({
    write_csv(data_final, CFG$out_csv, na = "")
    message(paste("✅ Archivo CSV guardado en:", CFG$out_csv))
  }, error = function(e) {
    message("\n❗️❗️❗️ ERROR AL GUARDAR EL ARCHIVO CSV ❗️❗️❗️")
    message("Causa probable: El archivo está abierto o no tienes permisos.")
    message("Error original de R: ", e$message)
  })
  
} else {
  message("⚠️ No se extrajo ninguna fila. No se guardó ningún archivo CSV.")
}

# 8. CERRAR SELENIUM
remote_driver$close()
driver$server$stop()
message("✅ Sesión de Selenium cerrada.")


