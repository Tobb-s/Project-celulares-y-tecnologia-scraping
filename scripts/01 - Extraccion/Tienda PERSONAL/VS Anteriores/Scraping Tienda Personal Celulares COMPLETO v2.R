# ==============================================================================
# SCRAPING “Tienda Personal – Celulares” · Versión multi-producto + multi-página
# ------------------------------------------------------------------------------
# • Recorre las 8 páginas de resultados (o hasta que no haya más)
# • Entra en los 12 productos de cada página
# • Clic en “Más info del producto” para extraer ficha + solapa “Legales”
# • Normaliza y acumula cada producto en un tibble
# • Guarda CSV con todas las filas en /raw
# • Mantiene la estructura general del “Esqueleto del Script de Scraping (e)”
# ==============================================================================

# 1. LIBRERÍAS -----------------------------------------------------------------
library(tidyverse)   # dplyr, tibble, purr
library(rvest)       # html_element, html_text2
library(RSelenium)   # rsDriver, remote_driver
library(xml2)        # read_html
library(stringr)     # str_extract, str_trim

# 2. CONFIGURACIÓN -------------------------------------------------------------
CFG <- list(
  home_url       = "https://www.personal.com.ar/",
  tienda_txt     = "Tienda",
  cel_txt        = "Celulares",
  cel_fallback   = "https://tienda.personal.com.ar/celulares",
  max_pages      = 8L,
  out_csv        = "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/raw/personal_celulares_multipagina.csv",
  
  # ----------- Mapeo etiqueta de la web → nombre de columna tibble -----------
  spec_map = tribble(
    ~label_html,          ~col_name,
    "Sistema Operativo",  "Sistema Operativo",
    "Procesador",         "Procesador",
    "RAM",                "RAM (GB)",
    "Memoria Interna",    "Almacenamiento interno (GB)",
    "Tamaño de pantalla", "Pantalla (Pulgadas)",
    "Cámara principal",   "Cámara Principal (MP)",
    "Cámara frontal",     "Cámara frontal (MP)",
    "NFC",                "NFC"
  ),
  
  # Etiquetas de precios (solapa “Legales”)
  legal_labels = c(
    "PRECIO DE LISTA",
    "PRECIO DE LISTA SIN IMPUESTOS",
    "PRECIO DE PROMOCIÓN",
    "PRECIO DE PROMOCIÓN SIN IMPUESTOS",
    "PRECIO CONEXIÓN TOTAL",
    "PRECIO CONEXIÓN TOTAL SIN IMPUESTOS"
  )
)

# 3. FUNCIONES AUXILIARES ------------------------------------------------------
reemplaza_vacio <- function(x) if (length(x) == 0 || is.na(x) || x == "") "" else x

# --- extrae_ficha(): lee ficha + precios legales y devuelve tibble de 1 fila ---
extrae_ficha <- function(remote_driver) {
  # (a) Seleccionar pestaña “Más info del producto”
  tabs <- remote_driver$findElements("css selector", "div.l0q4lv2n > div.l0q4lv2o")
  tabs[[2]]$clickElement()                  # «Más info del producto»
  Sys.sleep(1.5)
  html_ficha <- read_html(remote_driver$getPageSource()[[1]])
  
  # (b) XPaths para las especificaciones
  spec_xpath <- setNames(
    CFG$spec_map$label_html %>%
      map_chr(~sprintf(
        "//div[contains(@class,'l0q4lv38') and normalize-space(.)='%s']/following-sibling::span[1]",
        .x
      )),
    CFG$spec_map$col_name
  )
  
  specs_raw <- imap_chr(spec_xpath, function(xpath, col_name) {
    nodo <- html_element(html_ficha, xpath = xpath)
    reemplaza_vacio(html_text2(nodo))
  })
  
  specs <- imap_chr(specs_raw, function(val, col_name) {
    v <- str_trim(val)
    case_when(
      col_name %in% c("RAM (GB)", "Almacenamiento interno (GB)")      ~ str_extract(v, "\\d+"),
      col_name %in% c("Cámara Principal (MP)", "Cámara frontal (MP)") ~ str_extract(v, "\\d+"),
      col_name == "Pantalla (Pulgadas)"                               ~ str_extract(v, "\\d+(?:\\.\\d+)?"),
      TRUE ~ v
    )
  })
  
  # (c) Precios legales
  tabs[[3]]$clickElement()                  # «Legales»
  Sys.sleep(1.5)
  html_leg <- read_html(remote_driver$getPageSource()[[1]])
  leg_txt  <- html_text2(html_leg)
  
  legales <- setNames(
    map_chr(CFG$legal_labels, function(lbl) {
      patt <- paste0(lbl, "\\s*:?.*?\\$?\\s*([0-9\\.]+)")
      m <- str_match(leg_txt, patt)
      reemplaza_vacio(m[,2])
    }),
    CFG$legal_labels
  )
  
  # Financiación (cuotas sin interés)
  fin_raw <- html_leg %>%
    html_element("div.l0q4lv1h > div.l0q4lv1i") %>%
    html_text2() %>%
    reemplaza_vacio()
  fin_parts <- str_split(fin_raw, "\\n")[[1]] %>% str_trim()
  legales["Financiación"] <- paste(fin_parts, collapse = " | ")
  
  # (d) Tibble 1 fila
  tibble(
    producto_url = remote_driver$getCurrentUrl()[[1]],
    nombre       = html_element(html_ficha, "h1") %>% html_text2() %>% reemplaza_vacio()
  ) %>%
    bind_cols(as_tibble_row(specs)) %>%
    bind_cols(as_tibble_row(legales))
}

# 4. INICIALIZAR SELENIUM ------------------------------------------------------
driver <- rsDriver(
  browser    = "firefox",
  chromever  = NULL,
  port       = 4435L,
  check      = FALSE
)
remote_driver <- driver$client

# 5. NAVEGACIÓN HASTA LISTADO DE CELULARES ------------------------------------
remote_driver$navigate(CFG$home_url)
Sys.sleep(2)

try({
  remote_driver$findElement("xpath", sprintf("//a[normalize-space(text())='%s']", CFG$tienda_txt))$clickElement()
}, silent = TRUE)
Sys.sleep(2)

try({
  remote_driver$findElement("xpath", sprintf("//a[normalize-space(text())='%s']", CFG$cel_txt))$clickElement()
}, silent = TRUE)
Sys.sleep(2)

if (!grepl("/celulares", remote_driver$getCurrentUrl()[[1]]))
  remote_driver$navigate(CFG$cel_fallback)
Sys.sleep(2)

# 6. BUCLE PRINCIPAL -----------------------------------------------------------
rows <- list()
page_ix <- 1L
repeat {
  message("🗂️  Página ", page_ix)
  Sys.sleep(2)
  
  cards   <- remote_driver$findElements("css selector", "a[data-testid='product-card-container']")
  n_cards <- length(cards)
  if (n_cards == 0) {
    message("⚠️  Sin tarjetas encontradas, se detiene.")
    break
  }
  
  for (i in seq_len(n_cards)) {
    message("  📄 Producto ", i)
    
    # refrescamos la lista cada iteración → evitamos elementos "stale"
    cards <- remote_driver$findElements("css selector", "a[data-testid='product-card-container']")
    cards[[i]]$clickElement()
    Sys.sleep(1.8)
    
    fila <- tryCatch(extrae_ficha(remote_driver), error = function(e) {
      warning("Error en producto: ", e$message)
      tibble()
    })
    rows <- append(rows, list(fila))
    
    # Volver al listado conservando la página actual
    try(remote_driver$goBack(), silent = TRUE)
    Sys.sleep(1.8)
  }
  
  # ---- PAGINACIÓN (link text “Siguiente”) -----------------------------------
  siguiente <- tryCatch(
    remote_driver$findElement("link text", "Siguiente"),
    error = function(e) NULL
  )
  
  if (is.null(siguiente) || page_ix >= CFG$max_pages) {
    message("✅ Fin de paginación.")
    break
  }
  
  remote_driver$executeScript("arguments[0].scrollIntoView(true);", list(siguiente))
  Sys.sleep(1)
  siguiente$clickElement()
  page_ix <- page_ix + 1L
  Sys.sleep(2.5)
}

# 7. CONSOLIDAR Y GUARDAR ------------------------------------------------------
data_final <- bind_rows(rows) %>% distinct()
write_csv(data_final, CFG$out_csv)
message("✅ CSV guardado en: ", CFG$out_csv)

# 8. CERRAR SELENIUM -----------------------------------------------------------
remote_driver$close()
driver$server$stop()
