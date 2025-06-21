# ==============================================================================
# SCRAPING “Tienda Personal – Celulares” · Versión single-product 20-Jun-2025
# -------------------------------------------------------------------------------
# • Navega a la home, abre Tienda → Celulares, entra al primer producto  
# • Hace click en “Más info del producto”  
# • Extrae todas las especificaciones de la ficha con XPaths robustos  
# • Normaliza los valores numéricos donde corresponda  
# • Construye un tibble de una sola fila y lo guarda en CSV  
# ==============================================================================

# 1. LIBRERÍAS -----------------------------------------------------------------
library(tidyverse)   # dplyr, tibble, purrr
library(rvest)       # html_element, html_text2
library(RSelenium)   # rsDriver, remote_driver
library(xml2)        # read_html
library(stringr)     # str_extract, str_trim

# 2. CONFIGURACIÓN -------------------------------------------------------------
CFG <- list(
  home_url     = "https://www.personal.com.ar/",
  tienda_txt   = "Tienda",
  cel_txt      = "Celulares",
  cel_fallback = "https://tienda.personal.com.ar/celulares",
  
  # Mapeo etiqueta de la web → nombre de columna en el tibble
  spec_map = tribble(
    ~label_html,          ~col_name,
    "Sistema Operativo",  "Sistema Operativo",
    "Procesador",         "Procesador",
    "RAM",                "RAM (GB)",
    "Memoria Interna",    "Almacenamiento interno (GB)",
    "Tamaño de pantalla", "Pantalla (Pulgadas)",
    "Cámara principal",   "Cámara Principal (MP)",
    "Cámara frontal",     "Cámara frontal(MP)",
    "NFC",                "NFC"
  ),
  
  # Etiquetas de precios (solapa “Legales")
  legal_labels = c(
    "PRECIO DE LISTA",
    "PRECIO DE LISTA SIN IMPUESTOS",
    "PRECIO DE PROMOCIÓN",
    "PRECIO DE PROMOCIÓN SIN IMPUESTOS"
  )
)

# 3. FUNCIONES AUXILIARES ------------------------------------------------------
reemplaza_vacio <- function(x) if (length(x)==0 || is.na(x) || x=="") "" else x

wait_css <- function(selector, timeout = 10) {
  end_time <- Sys.time() + timeout
  while (Sys.time() < end_time) {
    el <- try(remote_driver$findElement("css selector", selector), silent = TRUE)
    if (!inherits(el, "try-error") &&
        el$isElementDisplayed()[[1]] &&
        el$isElementEnabled()[[1]]) {
      return(el)
    }
    Sys.sleep(0.5)
  }
  stop("Timeout esperando selector CSS: ", selector)
}

# 4. INICIALIZAR SELENIUM ------------------------------------------------------
driver <- rsDriver(
  browser    = "firefox",
  chromever  = NULL,
  port       = 4321L,
  check      = FALSE,
  phantomver = NULL
)
remote_driver <- driver$client

# 5. NAVEGACIÓN HASTA EL PRIMER PRODUCTO ---------------------------------------
remote_driver$navigate(CFG$home_url)
Sys.sleep(2)

# • Click “Tienda”
try({
  wait_css(sprintf("a:contains('%s')", CFG$tienda_txt))$clickElement()
}, silent = TRUE)
Sys.sleep(2)

# • Click “Celulares” (o fallback directo)
try({
  wait_css(sprintf("a:contains('%s')", CFG$cel_txt))$clickElement()
}, silent = TRUE)
Sys.sleep(2)
if (!grepl("/celulares", remote_driver$getCurrentUrl()[[1]]))
  remote_driver$navigate(CFG$cel_fallback)
Sys.sleep(2)

# Entrar al primer producto de la lista
first_card <- remote_driver$findElements("css selector", "a[data-testid='product-card-container']")[[1]]
first_card$clickElement()
Sys.sleep(4)

# 6. ABRIR PESTAÑA “Más info del producto” --------------------------------------
wait_css("div.l0q4lv2n")
tabs <- remote_driver$findElements("css selector", "div.l0q4lv2n > div.l0q4lv2o")
tabs[[2]]$clickElement()  # «Más info del producto»
Sys.sleep(2)

# 7. EXTRAER ESPECIFICACIONES --------------------------------------------------
html_ficha <- read_html(remote_driver$getPageSource()[[1]])

spec_xpath <- setNames(
  CFG$spec_map$label_html %>%
    map_chr(~sprintf(
      "//div[contains(@class,'l0q4lv38') and normalize-space(.)='%s']/following-sibling::span[1]",
      .x
    )),
  CFG$spec_map$col_name
)

message("---- SELECTORES USADOS ----")
walk2(names(spec_xpath), spec_xpath,
      ~message(.x, " → ", .y))

specs_raw <- imap_chr(spec_xpath, function(xpath, col_name) {
  nodo <- html_element(html_ficha, xpath = xpath)
  reemplaza_vacio(html_text2(nodo))
})

specs <- imap_chr(specs_raw, function(val, col_name) {
  v <- str_trim(val)
  case_when(
    col_name %in% c("RAM (GB)", "Almacenamiento interno (GB)")      ~ str_extract(v, "\\d+"),
    col_name %in% c("Cámara trasera (principal)", "Cámara frontal") ~ str_extract(v, "\\d+"),
    col_name == "Pantalla (Pulgadas)"                               ~ str_extract(v, "\\d+(?:\\.\\d+)?"),
    TRUE ~ v
  )
})

# 8. EXTRAER PRECIOS Y FINANCIACIÓN (SOLAPA “LEGALES”) -------------------------
tabs[[3]]$clickElement()
Sys.sleep(2)
html_leg <- read_html(remote_driver$getPageSource()[[1]])

# 8.1 Precios legales
legales <- setNames(
  map_chr(CFG$legal_labels, function(lbl) {
    patt <- paste0(lbl, "\\s*:?\\s*\\$?\\s*([0-9\\.]+)")
    m <- str_match(html_text2(html_leg), patt)
    reemplaza_vacio(m[,2])
  }),
  CFG$legal_labels
)

# 8.2 Financiación (cuotas sin interés)
fin_raw <- html_leg %>%
  html_element("div.l0q4lv1h > div.l0q4lv1i") %>%
  html_text2() %>%
  reemplaza_vacio()

fin_parts <- str_split(fin_raw, "\\n")[[1]] %>% str_trim()
legales["Financiación"] <- paste(fin_parts, collapse = " | ")

# 9. ARMAR TIBBLE FINAL Y GUARDAR ----------------------------------------------
data_final <- tibble(
  producto_url = remote_driver$getCurrentUrl()[[1]],
  nombre       = html_element(html_ficha, "h1") %>% html_text2() %>% reemplaza_vacio()
) %>%
  bind_cols(as_tibble_row(specs)) %>%
  bind_cols(as_tibble_row(legales))

print(data_final)    # muestra en consola
view(data_final)
# Guardar CSV en la ruta especificada
output_path <- "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/raw/personal_celu_unico.csv"
write_csv(data_final, output_path)
message("✅ CSV guardado en: ", output_path)

# 10. CERRAR SELENIUM ----------------------------------------------------------
remote_driver$close()
driver$server$stop()
