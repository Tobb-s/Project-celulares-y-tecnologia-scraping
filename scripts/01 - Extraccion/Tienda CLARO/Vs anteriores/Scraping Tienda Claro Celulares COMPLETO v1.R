# ==============================================================================
# SCRAPING “Tienda Claro – Celulares” (Versión 4.5 - Final)
# ------------------------------------------------------------------------------
# • Lógica estricta basada en las clases HTML indicadas para máxima precisión.
# • Recopila dinámicamente TODAS las especificaciones disponibles para cada producto.
# • Simula el scroll y clic en la pestaña "Especificaciones".
# • Permite configurar el número máximo de productos a scrapear.
# ==============================================================================

# 1. LIBRERÍAS -----------------------------------------------------------------
# Asegúrate de tener estas librerías instaladas:
# install.packages(c("tidyverse", "rvest", "RSelenium", "xml2", "stringr"))
library(tidyverse)
library(rvest)
library(RSelenium)
library(xml2)
library(stringr)

# 2. CONFIGURACIÓN -------------------------------------------------------------
CFG <- list(
  home_url     = "https://tienda.claro.com.ar/",
  cel_txt      = "Celulares",
  cel_fallback = "https://tienda.claro.com.ar/plp/equipos",
  max_products = 48L, # ¡Puedes cambiar este número para limitar el scraping!
  out_csv      = "C:/Users/valen/OneDrive - Económicas - UBA/=ANALISIS DE DATO PARA LA ECONOMIA Y NEGOCIOS=/Project-celulares-y-tecnologia-scraping/raw/claro_celulares_multipagina.csv"
)

# 3. FUNCIONES AUXILIARES ------------------------------------------------------
#' Extrae la ficha técnica de un producto leyendo la estructura del HTML.
#' @param remote_driver La instancia del cliente de Selenium.
#' @return Un tibble de 1 fila con la información del producto.
extrae_ficha_html <- function(remote_driver) {
  
  # (a) Hacer scroll y clic en el botón "Especificaciones"
  tryCatch({
    spec_button <- remote_driver$findElement("xpath", "//button[normalize-space(.)='ESPECIFICACIONES']")
    remote_driver$executeScript("arguments[0].scrollIntoView(true);", list(spec_button))
    Sys.sleep(0.5)
    spec_button$clickElement()
    Sys.sleep(1)
  }, error = function(e) {
    warning("No se pudo encontrar o hacer clic en el botón 'ESPECIFICACIONES'.")
  })
  
  # (b) Leer el código fuente de la página DESPUÉS del clic
  html_content <- read_html(remote_driver$getPageSource()[[1]])
  
  # (c) Extraer datos básicos
  nombre_prod <- html_content %>%
    html_element("[data-test='product_title']") %>%
    html_text2()
  
  precio_lista_raw <- html_content %>%
    html_element("[data-test='transparency-law-text']") %>%
    html_text2()
  precio_lista <- str_extract(precio_lista_raw, "\\$ [\\d\\.,]+") %>% str_trim()
  
  precio_promo <- html_content %>%
    html_element("[data-test='offer-price']") %>%
    html_text2() %>% str_trim()
  
  # (d) Extraer dinámicamente TODAS las especificaciones
  # Scrapeo de la tabla de especificaciones principal
  spec_nodes_table <- html_content %>%
    html_elements("div.Pdp_divFlex__TSPiC")
  
  # Scrapeo de las especificaciones con íconos
  spec_nodes_icon <- html_content %>%
    html_elements("div.Pdp_divIconFlex__cQCSX")
  
  specs_list <- list()
  
  if (length(spec_nodes_table) > 0) {
    specs_list_table <- map(spec_nodes_table, function(node) {
      label <- node %>% html_element("div[class*='Pdp_divTitulo__']") %>% html_text2()
      value <- node %>% html_element("div[class*='Pdp_divDescripcion__']") %>% html_text2()
      tibble(label = str_trim(label), value = str_trim(value))
    })
    specs_list <- append(specs_list, specs_list_table)
  }
  
  if (length(spec_nodes_icon) > 0) {
    specs_list_icon <- map(spec_nodes_icon, function(node) {
      label <- node %>% html_element("div.Pdp_titleImg__p6ExU") %>% html_text2()
      value <- node %>% html_element("div.Pdp_descriptionImg__koISR") %>% html_text2()
      tibble(label = str_trim(label), value = str_trim(value))
    })
    specs_list <- append(specs_list, specs_list_icon)
  }
  
  specs <- tibble()
  if (length(specs_list) > 0) {
    # Combinar todas las especificaciones, eliminar duplicados y pivotar
    specs_df <- bind_rows(specs_list) %>%
      filter(!is.na(label) & label != "") %>%
      distinct(label, .keep_all = TRUE)
    
    specs <- specs_df %>%
      pivot_wider(names_from = label, values_from = value)
    
  } else {
    warning("No se encontró ninguna tabla de especificaciones.")
  }
  
  # (e) Consolidar todo
  base_info <- tibble(
    producto_url = remote_driver$getCurrentUrl()[[1]],
    nombre = nombre_prod,
    precio_lista = precio_lista,
    precio_promo = precio_promo
  )
  
  return(bind_cols(base_info, specs))
}


# 4. INICIALIZAR SELENIUM ------------------------------------------------------
driver <- rsDriver(
  browser   = "firefox",
  chromever = NULL,
  port      = 4437L,
  check     = FALSE
)
remote_driver <- driver$client
remote_driver$setTimeout("page load", 15000)

# 5. NAVEGACIÓN HASTA LISTADO DE CELULARES ------------------------------------
message("➡️ Navegando a la página principal de Tienda Claro...")
remote_driver$navigate(CFG$home_url)
Sys.sleep(2)

tryCatch({
  message("➡️ Buscando y haciendo clic en la sección 'Celulares'...")
  celulares_link <- remote_driver$findElement("xpath", "//a[.//div[normalize-space(.)='Celulares']]")
  celulares_link$clickElement()
  message("✅ Clic en 'Celulares' exitoso.")
}, error = function(e) {
  warning("⚠️ No se pudo hacer clic en 'Celulares', usando URL de fallback. Error: ", e$message)
  remote_driver$navigate(CFG$cel_fallback)
})
Sys.sleep(3)

# 6. BUCLE PRINCIPAL -----------------------------------------------------------
rows <- list()

message("➡️ Haciendo scroll para cargar todos los productos...")
n_products_found <- 0
attempts <- 0

repeat {
  remote_driver$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(2)
  
  current_products <- remote_driver$findElements("css selector", "a[href^='/pdp/equipos/']")
  
  if (length(current_products) == n_products_found) {
    attempts <- attempts + 1
    message("...No se encontraron nuevos productos (Intento ", attempts, "/3)")
    if (attempts >= 3) {
      message("✅ No se cargaron más productos. Se encontraron ", n_products_found, " en total.")
      break
    }
  } else {
    n_products_found <- length(current_products)
    message("...Productos encontrados: ", n_products_found)
    attempts <- 0
  }
}

product_urls <- remote_driver$findElements("css selector", "a[href^='/pdp/equipos/']") %>%
  map_chr(~.x$getElementAttribute("href")[[1]]) %>%
  unique()

# El script ahora procesará los productos hasta el límite definido en CFG
n_to_scrape <- min(length(product_urls), CFG$max_products)
message("➡️  Se iniciará el scraping de ", n_to_scrape, " productos.")

for (i in seq_len(n_to_scrape)) {
  current_url <- product_urls[i]
  message("📄 Procesando producto ", i, " de ", n_to_scrape, ": ", basename(current_url))
  
  remote_driver$navigate(current_url)
  Sys.sleep(2)
  
  fila <- tryCatch({
    extrae_ficha_html(remote_driver)
  }, error = function(e) {
    warning("❗️ Error extrayendo datos del producto ", current_url, ": ", e$message)
    tibble(
      producto_url = current_url,
      nombre = "ERROR EN EXTRACCIÓN",
      precio_lista = NA_character_,
      precio_promo = NA_character_
    )
  })
  
  rows <- append(rows, list(fila))
}

# 7. CONSOLIDAR Y GUARDAR ------------------------------------------------------
if (length(rows) > 0) {
  data_final <- bind_rows(rows) %>% distinct()
  
  message("➡️ Intentando guardar el archivo CSV...")
  message("Asegúrate de que el archivo ", basename(CFG$out_csv), " no esté abierto.")
  
  tryCatch({
    output_dir <- dirname(CFG$out_csv)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    write_csv(data_final, CFG$out_csv, na = "")
    message("✅ Archivo CSV guardado con ", nrow(data_final), " filas en: ", CFG$out_csv)
  }, error = function(e) {
    message("\n❗️❗️❗️ ERROR AL GUARDAR EL ARCHIVO CSV ❗️❗️❗️")
    message("R no pudo escribir en la ruta: ", CFG$out_csv)
    message("Causa más probable: El archivo está abierto en otro programa (como Excel).")
    message("Por favor, cierra el archivo e intenta de nuevo.")
    message("Error original de R: ", e$message)
  })
  
} else {
  message("⚠️ No se extrajo ninguna fila. No se guardó ningún archivo CSV.")
}

# 8. CERRAR SELENIUM -----------------------------------------------------------
remote_driver$close()
driver$server$stop()
message("✅ Sesión de Selenium cerrada.")


