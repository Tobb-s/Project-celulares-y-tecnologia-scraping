# ==============================================================================
# SCRAPING “Tienda Personal – Celulares” (Versión PORTÁTIL)
# ------------------------------------------------------------------------------
# • Recorre las páginas de resultados.
# • Entra en los productos de cada página.
# • Clic en “Más info del producto” para extraer ficha + solapa “Legales”.
# • Normaliza y acumula cada producto en un tibble.
# • Guarda CSV con todas las filas en /raw de forma portátil.
# ==============================================================================

# 1. LIBRERÍAS -----------------------------------------------------------------
# install.packages(c("tidyverse", "rvest", "RSelenium", "xml2", "stringr", "here"))
library(tidyverse)   # dplyr, tibble, purr
library(rvest)       # html_element, html_text2
library(RSelenium)   # rsDriver, remote_driver
library(xml2)        # read_html
library(stringr)     # str_extract, str_trim
library(here)        # Paquete para manejar rutas de archivo de forma portátil

# 2. CONFIGURACIÓN -------------------------------------------------------------
CFG <- list(
  home_url       = "https://www.personal.com.ar/",
  tienda_txt     = "Tienda",
  cel_txt        = "Celulares",
  cel_fallback   = "https://tienda.personal.com.ar/celulares",
  max_pages      = 5L,
  # --- RUTA DE SALIDA PORTÁTIL ---
  # El archivo se guardará en una carpeta 'raw' dentro del directorio del proyecto.
  out_csv        = here("raw", "personal_celulares_multipagina.csv"),
  
  # ----------- Mapeo etiqueta de la web → nombre de columna tibble  
  
  spec_map = tribble(
    ~label_html,             ~col_name,
    "Sistema Operativo",     "Sistema Operativo",
    "Procesador",            "Procesador",
    "RAM",                   "RAM (GB)",
    "Memoria Interna",       "Almacenamiento interno (GB)",
    "Tamaño de pantalla",    "Pantalla (Pulgadas)",
    "Camara principal",      "Camara Principal (MP)",
    "Camara frontal",        "Camara frontal (MP)",
    "NFC",                   "NFC"
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

# 3. FUNCIONES AUXILIARES ---------------------------------------------------------------------------------
reemplaza_vacio <- function(x) if (length(x) == 0 || is.na(x) || x == "") "" else x

# --- extrae_ficha(): lee ficha + precios legales y devuelve tibble de 1 fila, manejando pestañas faltantes ---
extrae_ficha <- function(remote_driver) {
  # (a) Intentar pestaña “Más info del producto”
  info_html <- NULL
  tryCatch({
    tabs <- remote_driver$findElements(
      "css selector", "div.l0q4lv2n > div.l0q4lv2o"
    )
    if (length(tabs) >= 2) {
      tabs[[2]]$clickElement()              # «Más info...»
      Sys.sleep(1.5)
      info_html <- read_html(remote_driver$getPageSource()[[1]])
    } else {
      warning("⚠️ Pestaña “Más info del producto” no encontrada.")
    }
  }, error = function(e) {
    warning("❌ Error al abrir “Más info del producto”: ", e$message)
  })
  
  # Si no abrimos ficha, devolvemos tibble con todo vacío
  if (is.null(info_html)) {
    return(tibble(
      producto_url = remote_driver$getCurrentUrl()[[1]],
      nombre       = "",
      # columnas de specs vacías
      !!!setNames(rep(list(""), length(CFG$spec_map$col_name)),
                  CFG$spec_map$col_name),
      # columnas de legales + Financiación vacías
      !!!setNames(rep(list(""), length(CFG$legal_labels) + 1),
                  c(CFG$legal_labels, "Financiación"))
    ))
  }
  
  # (b) Extraer specs desde info_html
  spec_xpath <- setNames(
    CFG$spec_map$label_html %>%
      map_chr(~ sprintf(
        "//div[contains(@class,'l0q4lv38') and normalize-space(.)='%s']
         /following-sibling::span[1]",
        .x
      )),
    CFG$spec_map$col_name
  )
  specs_raw <- imap_chr(spec_xpath, function(xpath, col_name) {
    nodo <- html_element(info_html, xpath = xpath)
    reemplaza_vacio(html_text2(nodo))
  })
  specs <- imap_chr(specs_raw, function(val, col_name) {
    v <- str_trim(val)
    case_when(
      col_name %in% c("RAM (GB)", "Almacenamiento interno (GB)")     ~ str_extract(v, "\\d+"),
      col_name %in% c("Camara Principal (MP)", "Camara frontal (MP)") ~ str_extract(v, "\\d+"),
      col_name == "Pantalla (Pulgadas)"                              ~ str_extract(v, "\\d+(?:\\.\\d+)?"),
      TRUE ~ v
    )
  })
  
  # (c) Intentar pestaña “Legales”
  leg_txt <- ""
  leg_html <- NULL
  tryCatch({
    tabs <- remote_driver$findElements(
      "css selector", "div.l0q4lv2n > div.l0q4lv2o"
    )
    if (length(tabs) >= 3) {
      tabs[[3]]$clickElement()              # «Legales»
      Sys.sleep(1.5)
      leg_html <- read_html(remote_driver$getPageSource()[[1]])
      leg_txt  <- html_text2(leg_html)
    } else {
      warning("⚠️ Pestaña “Legales” no encontrada.")
    }
  }, error = function(e) {
    warning("❌ Error al abrir “Legales”: ", e$message)
  })
  
  # Extraer precios legales
  legales <- setNames(
    map_chr(CFG$legal_labels, function(lbl) {
      patt <- paste0(lbl, "\\s*:?.*?\\$?\\s*([0-9\\.]+)")
      m <- str_match(leg_txt, patt)
      reemplaza_vacio(if (!is.na(m[,2])) m[,2] else "")
    }),
    CFG$legal_labels
  )
  
  # Financiación (cuotas sin interés), solo si leg_html fue leído
  fin_txt <- ""
  if (!is.null(leg_html)) {
    fin_txt <- tryCatch({
      leg_html %>%
        html_element("div.l0q4lv1h > div.l0q4lv1i") %>%
        html_text2() %>%
        reemplaza_vacio()
    }, error = function(e) "")
  }
  fin_parts <- if (fin_txt != "") str_split(fin_txt, "\\n")[[1]] %>% str_trim() else character()
  legales["Financiación"] <- if (length(fin_parts) > 0)
    paste(fin_parts, collapse = " | ") else ""
  
  # (d) Devolver tibble completo
  tibble(
    producto_url = remote_driver$getCurrentUrl()[[1]],
    nombre       = html_element(info_html, "h1") %>% html_text2() %>% reemplaza_vacio()
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
data_final <- bind_rows(rows) %>% distinct() %>% mutate(sitio="Personal")

# Creamos la carpeta 'raw' si no existe, para evitar errores.
dir.create(dirname(CFG$out_csv), showWarnings = FALSE, recursive = TRUE)

write_csv(data_final, CFG$out_csv, na = "")
message("✅ CSV guardado en: ", CFG$out_csv)

View(data_final)

# 8. CERRAR SELENIUM -----------------------------------------------------------
remote_driver$close()
driver$server$stop()
