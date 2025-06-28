# ===============================================================================
#  SCRAPING “Tienda Personal – Celulares”  ·  versión 2025-06-20
#  -----------------------------------------------------------
#  • Captura directamente el **primer <span>** que sigue al rótulo (Sistema Operativo,
#    Cámara principal, etc.) —ya no busca primero un hermano genérico.
#  • Mantiene toda la lógica anterior (scroll, clics, normalización, CSV).
#  • Comentarios detallados ≈ “qué hace / busca” en cada bloque o línea clave.
#  Requiere: tidyverse, rvest, RSelenium, xml2, stringr
# ===============================================================================

# ------------------------------------------------------------- 1. LIBRERÍAS ----
library(tidyverse)   # dplyr, tibble, map_*, %>% …
library(rvest)       # parseo HTML (CSS / XPath, html_text2)
library(RSelenium)   # control del navegador vía WebDriver
library(xml2)        # read_html() para pageSource
library(stringr)     # str_* helpers

# ------------------------------------------------------------- 2. CONFIGURACIÓN ----
CFG <- list(
  # URLs principales -----------------------------------------------------------
  home_url      = "https://www.personal.com.ar/",
  tienda_txt    = "Tienda",
  cel_txt       = "Celulares",
  cel_fallback  = "https://tienda.personal.com.ar/celulares",
  
  # Etiquetas de la ficha técnica ----------------------------------------------
  spec_labels   = c(
    "Sistema Operativo",
    "Procesador",
    "RAM",
    "Memoria Interna",
    "Cámara principal",
    "Cámara frontal",
    "NFC"
  ),
  
  # Campos monetarios en solapa “Legales” --------------------------------------
  legal_labels  = c(
    "PRECIO DE LISTA",
    "PRECIO DE LISTA SIN IMPUESTOS",
    "PRECIO DE PROMOCIÓN",
    "PRECIO DE PROMOCIÓN SIN IMPUESTOS",
    "FINANCIACIÓN"
  )
)

# ------------------------------------------------------------- 3. AUXILIARES ----
# • Reemplaza NULL / vacío por ""
reemplaza_vacio <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || (is.character(x) && x == "")) "" else x
}

# • Espera explícita usando **CSS selector**
wait_for_element <- function(selector, timeout = 10) {
  end_time <- Sys.time() + timeout
  while (Sys.time() < end_time) {
    try({
      el <- remote_driver$findElement("css selector", selector)
      if (el$isElementDisplayed()[[1]] && el$isElementEnabled()[[1]])
        return(el)
    }, silent = TRUE)
    Sys.sleep(0.5)
  }
  stop("Elemento CSS no encontrado: ", selector)
}

# • EXTRA: busca directamente el primer <span> hermano del rótulo
extrae_texto <- function(html, etiquetas) {
  setNames(
    map_chr(etiquetas, function(lbl) {
      xpath <- sprintf(
        "//*[normalize-space(text())='%s']/parent::*/span[1]", lbl
      )
      nodo <- html_element(html, xpath = xpath)
      reemplaza_vacio(html_text2(nodo))
    }),
    etiquetas
  )
}

# • Normaliza valores (“128 GB” → “128”, “50+2 MP” → “50”, etc.)
normaliza_valor <- function(nombre, valor_raw) {
  v <- trimws(valor_raw)
  dplyr::case_when(
    nombre == "Memoria Interna"  ~ sub(".*?(\\d+).*", "\\1", v),
    nombre == "RAM"              ~ sub(".*?(\\d+).*", "\\1", v),
    nombre == "Cámara principal" ~ sub(".*?(\\d+).*", "\\1", v),
    nombre == "Cámara frontal"   ~ sub(".*?(\\d+).*", "\\1", v),
    nombre == "NFC"              ~ sub("NFC\\s*:?\\s*(\\w+).*", "\\1", v, ignore.case = TRUE),
    TRUE                         ~ v
  )
}

# ------------------------------------------------------------- 4. SELENIUM -----
# ► Inicialización EXACTA pedida + parche anti-PhantomJS
driver <- rsDriver(
  browser    = "firefox",  # cambiá a "chrome" si preferís
  chromever  = NULL,       # versión autodetectada
  port       = 4321L,
  check      = FALSE,      # evita descargas automáticas
  phantomver = NULL        # NO PhantomJS (evita error 402)
)
remote_driver <- driver[["client"]]

# ------------------------------------------------------------- 5. NAVEGACIÓN ----
remote_driver$navigate(CFG$home_url)
Sys.sleep(2)

# • Click “Tienda”
try({
  wait_for_element(sprintf("a:contains('%s')", CFG$tienda_txt))$clickElement()
}, silent = TRUE)
Sys.sleep(2)

# • Click “Celulares” (o fallback directo)
try({
  wait_for_element(sprintf("a:contains('%s')", CFG$cel_txt))$clickElement()
}, silent = TRUE)
Sys.sleep(4)
if (!grepl("/celulares", remote_driver$getCurrentUrl()[[1]]))
  remote_driver$navigate(CFG$cel_fallback)
Sys.sleep(4)

# -------------------------------------------------- 6. ENTRAR A PRIMER PRODUCTO ----
card <- remote_driver$
  findElements("css selector", "a[data-testid='product-card-container']")[[1]]
card$clickElement()
Sys.sleep(4)

# ---------- 6.1 SCROLL PROGRESIVO HASTA “Más info del producto” ----
tab_container_css <- "div.l0q4lv2n"  # contenedor de solapas
paso_px <- 300
repet   <- 0

while (TRUE) {
  cont <- tryCatch(
    remote_driver$findElements("css", tab_container_css),
    error = function(e) NULL
  )
  if (length(cont) > 0 &&
      tryCatch(cont[[1]]$isElementDisplayed()[[1]], error = function(e) FALSE)) break
  remote_driver$executeScript(
    sprintf("window.scrollBy(0, %d);", paso_px),
    list()
  )
  Sys.sleep(0.4)
  repet <- repet + 1
  if (repet > 30) stop("No se encontró el contenedor de solapas (l0q4lv2n)")
}

# ---------- 6.2 CLICKEAR “Más info del producto” -------------------------------
tabs <- remote_driver$findElements(
  "css", paste0(tab_container_css, " > div.l0q4lv2o")
)
if (length(tabs) < 2) stop("No se detectaron las solapas de ficha técnica")

remote_driver$executeScript(
  "arguments[0].scrollIntoView({behavior:'auto',block:'center'});",
  list(tabs[[2]])
)
Sys.sleep(0.5)
tryCatch(
  tabs[[2]]$clickElement(),
  error = function(e) remote_driver$executeScript("arguments[0].click();", list(tabs[[2]]))
)
Sys.sleep(2)

# --------------------------- 7. EXTRAER ESPECIFICACIONES -----------------------
html       <- read_html(remote_driver$getPageSource()[[1]])
specs_raw  <- extrae_texto(html, CFG$spec_labels)

# ▸ Normalización + renombrado amigable
specs <- purrr::imap_chr(specs_raw, normaliza_valor) |>
  `names<-`(c(
    "Sistema Operativo",
    "Procesador",
    "RAM (GB)",
    "Almacenamiento Interno (GB)",
    "Cámara Trasera (MP)",
    "Cámara Frontal (MP)",
    "NFC"
  ))

# --------------------------- 8. SOLAPA “LEGALES” -------------------------------
remote_driver$executeScript(
  "arguments[0].scrollIntoView({behavior:'auto',block:'center'});",
  list(tabs[[3]])
)
Sys.sleep(0.4)
tryCatch(
  tabs[[3]]$clickElement(),
  error = function(e) remote_driver$executeScript("arguments[0].click();", list(tabs[[3]]))
)
Sys.sleep(2)

html_leg <- read_html(remote_driver$getPageSource()[[1]])
legales  <- setNames(
  map_chr(CFG$legal_labels, function(lab) {
    patt <- paste0(lab, "\\s*:?\\s*\\$?\\s*([0-9\\.]+)")
    m    <- str_match(html_text2(html_leg), patt)
    reemplaza_vacio(m[, 2])
  }),
  CFG$legal_labels
)

# --------------------------- 9. RESULTADO FINAL --------------------------------
data_final <- tibble(
  producto_url = remote_driver$getCurrentUrl()[[1]],
  nombre       = html_element(html, "h1") |> html_text2() |> reemplaza_vacio()
) |>
  bind_cols(as_tibble_row(specs)) |>
  bind_cols(as_tibble_row(legales))

print(data_final)
View(data_final)

# --------------------------- 10. GUARDAR + CERRAR ------------------------------
# Ruta de salida corregida:
output_path <- "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/raw/personal_celu_unico.csv"

# Escribir CSV
write_csv(data_final, output_path)
message("✅ CSV guardado en: ", output_path)

# Cerrar sesión Selenium
remote_driver$close()
driver$server$stop()
