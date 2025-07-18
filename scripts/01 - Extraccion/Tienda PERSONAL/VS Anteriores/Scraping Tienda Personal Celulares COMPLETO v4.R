# ==============================================================================
# SCRAPING “Tienda Personal – Celulares” · Versión multi-producto + multi-página
# ------------------------------------------------------------------------------
# • Recorre las 8 páginas de resultados (o hasta que no haya más)
# • Entra en los 12 productos de cada página
# • Clic en “Más info del producto” para extraer ficha + solapa “Legales”
# • Normaliza y acumula cada producto en un tibble
# • Guarda CSV con todas las filas en /raw
# • Omite productos que tardan más de 10 segundos en scrapearse
# ==============================================================================

# 1. LIBRERÍAS -----------------------------------------------------------------
library(tidyverse)   # dplyr, tibble, purr
library(rvest)       # html_element, html_text2
library(RSelenium)   # rsDriver, remote_driver
library(xml2)        # read_html
library(stringr)     # str_extract, str_trim
library(R.utils)     # trae withTimeout()

# 2. CONFIGURACIÓN --------------------------------------------------------------------------------------------------------------------
CFG <- list(
  home_url       = "https://www.personal.com.ar/",
  tienda_txt     = "Tienda",
  cel_txt        = "Celulares",
  cel_fallback   = "https://tienda.personal.com.ar/celulares",
  max_pages      = 5L,
  out_csv        = "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/raw/personal_celulares_multipagina.csv",
  
  # ----------- Mapeo etiqueta de la web → nombre de columna tibble ----------------------------------------------------------
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
      tabs[[2]]$clickElement()                  # «Más info...»
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
      col_name %in% c("RAM (GB)", "Almacenamiento interno (GB)")      ~ str_extract(v, "\\d+"),
      col_name %in% c("Cámara Principal (MP)", "Cámara frontal (MP)") ~ str_extract(v, "\\d+"),
      col_name == "Pantalla (Pulgadas)"                               ~ str_extract(v, "\\d+(?:\\.\\d+)?"),
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
      tabs[[3]]$clickElement()                  # «Legales»
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

# 4. INICIALIZAR SELENIUM -----------------------------------------------------------------------------------
driver <- rsDriver(
  browser    = "firefox",
  chromever  = NULL,
  port       = 4435L,
  check      = FALSE
)
remote_driver <- driver$client

# 5. NAVEGACIÓN HASTA LISTADO DE CELULARES -------------------------------------------------------------------
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
rows          <- list()
page_ix       <- 1L
productos_om  <- integer()    # lleva el índice de los omitidos

repeat {
  message("🗂️  Página ", page_ix)
  Sys.sleep(2)
  
  # 6.1. Recoger tarjetas
  cards   <- remote_driver$findElements("css selector",
                                        "a[data-testid='product-card-container']")
  n_cards <- length(cards)
  if (n_cards == 0) {
    message("⚠️  Sin tarjetas encontradas, se detiene.")
    break
  }
  
  # 6.2. Iterar cada producto
  for (i in seq_len(n_cards)) {
    message("  📄 Producto ", i)
    
    # 6.2.1. Clic en el i-ésimo (refrescando la lista)
    cards <- remote_driver$findElements("css selector",
                                        "a[data-testid='product-card-container']")
    try(cards[[i]]$clickElement(), silent = TRUE)
    Sys.sleep(1.8)
    
    # 6.2.2. Extraer ficha con watchdog de 10 s y captura de errores genéricos
    fila <- tryCatch({
      withTimeout(
        expr      = extrae_ficha(remote_driver),
        timeout   = 10,
        onTimeout = "silent"
      )
    }, error = function(e) {
      warning(sprintf("❌ Error inesperado en producto %d: %s", i, e$message))
      NULL
    })
    
    # 6.2.3. Si hubo timeout o error, saltamos al siguiente producto
    if (is.null(fila) || nrow(fila) == 0) {
      message(sprintf("⏭ Producto %d omitido.", i))
      productos_om <- c(productos_om, i)
      try(remote_driver$goBack(), silent = TRUE)
      Sys.sleep(1.5)
      next
    }
    
    # 6.2.4. Guardar fila exitosa
    rows <- append(rows, list(fila))
    
    # 6.2.5. Volver al listado
    try(remote_driver$goBack(), silent = TRUE)
    Sys.sleep(1.8)
  }
  
  # 6.3. Paginación: siguiente página (LÓGICA ORIGINAL RESTAURADA)
  siguiente <- tryCatch(
    remote_driver$findElement("link text", "Siguiente"),
    error = function(e) NULL
  )
  if (is.null(siguiente) || page_ix >= CFG$max_pages) {
    message("✅ Fin de paginación.")
    break
  }
  remote_driver$executeScript("arguments[0].scrollIntoView(true);",
                              list(siguiente))
  Sys.sleep(1)
  try(siguiente$clickElement(), silent = TRUE)
  page_ix <- page_ix + 1L
  Sys.sleep(2.5)
}

# Mensaje final con los omitidos
message("Productos omitidos (por timeout o error): ",
        if (length(productos_om)) paste(productos_om, collapse = ", ") else "ninguno")
# 7. CONSOLIDAR Y GUARDAR -------------------------------------------------------------------------------
data_final <- bind_rows(rows) %>% distinct()
write_csv(data_final, CFG$out_csv)
message("✅ CSV guardado en: ", CFG$out_csv)

# 8. CERRAR SELENIUM ------------------------------------------------------------------------------------
remote_driver$close()
driver$server$stop()