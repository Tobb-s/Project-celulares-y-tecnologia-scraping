# ==============================================================================
# SCRAPING “Tienda Claro – Celulares” (Versión 4.5 — PORTÁTIL)
# ------------------------------------------------------------------------------
# • 100 % funcional sin helper externo.
# • Abre “Especificaciones”, INTENTA click en “Medios de pago” y, si no hay modal,
#   lee la leyenda de cuotas desde el JSON incrustado (__NEXT_DATA__).
# • Adaptado con el paquete 'here' para portabilidad.
# ==============================================================================

# 1. LIBRERÍAS -----------------------------------------------------------------
# install.packages(c("tidyverse", "rvest", "RSelenium", "xml2", "stringr", "jsonlite", "here"))
library(tidyverse)
library(rvest)
library(RSelenium)
library(xml2)
library(stringr)
library(jsonlite)      # ← necesario para fallback JSON
library(here)          # Paquete para manejar rutas de archivo de forma portátil

# 2. CONFIGURACIÓN -------------------------------------------------------------
CFG <- list(
  home_url     = "https://tienda.claro.com.ar/",
  cel_txt      = "Celulares",
  cel_fallback = "https://tienda.claro.com.ar/plp/equipos",
  max_products = 48L,    # cambia si querés más
  # --- RUTA DE SALIDA PORTÁTIL ---
  # El archivo se guardará en una carpeta 'raw' dentro del directorio del proyecto.
  out_csv      = here("raw", "claro_celulares_multipagina.csv")
)

# ---------------------------------------------------------------------------
# AUXILIAR • Extrae ficha + cuotas (1 fila tibble)
# ---------------------------------------------------------------------------
extrae_ficha_html <- function(remote_driver) {
  
  ## (a) ESPECIFICACIONES ----------------------------------------------------
  tryCatch({
    spec_btn <- remote_driver$findElement(
      "xpath", "//button[normalize-space(.)='ESPECIFICACIONES']"
    )
    remote_driver$executeScript(
      "arguments[0].scrollIntoView(true);", list(spec_btn)
    )
    Sys.sleep(0.8); spec_btn$clickElement(); Sys.sleep(0.8)
  }, error = function(e)
    warning("No se pudo hacer clic en 'ESPECIFICACIONES'")
  )
  
  ## (b) MEDIOS DE PAGO · clic en el botón ----------------------------------
  tryCatch({
    mp_btn <- remote_driver$findElement(
      "xpath", "//button[translate(normalize-space(.), 'ÁÉÍÓÚáéíóú',
                                   'AEIOUaeiou') = 'Medios de pago']"
    )
    remote_driver$executeScript(
      "arguments[0].scrollIntoView(true);", list(mp_btn)
    )
    remote_driver$executeScript("arguments[0].click();", list(mp_btn))
    Sys.sleep(1.5)              # deja que React monte el modal
  }, error = function(e)
    warning("No se pudo hacer clic en 'Medios de pago'")
  )
  
  ## (c) Capturar DOM con el modal abierto ----------------------------------
  html_content <- read_html(remote_driver$getPageSource()[[1]])
  
  ## ——————————————————————————————————————————
  ## (d) Financiamiento  · “Hasta X cuotas…”
  ## ——————————————————————————————————————————
  financiacion_plazo <- html_content %>% 
    html_elements("div[data-test='payment-quotas']") %>% 
    html_text2() %>% 
    str_squish() %>% 
    paste(collapse = " | ")
  
  ## Fallback JSON si sigue vacío
  if (!nzchar(financiacion_plazo)) {
    json_raw <- html_content %>% 
      html_element("script#__NEXT_DATA__") %>% 
      html_text2()
    cuotas_txt <- tryCatch({
      flat <- unlist(jsonlite::fromJSON(json_raw, simplifyVector = TRUE),
                     use.names = FALSE)
      unique(flat[str_detect(flat, regex("cuotas?.*tarjeta", ignore_case = TRUE))])
    }, error = function(e) character(0))
    if (length(cuotas_txt) > 0)
      financiacion_plazo <- paste(cuotas_txt, collapse = " | ")
  }
  
  # (e) Datos básicos ---------------------------------------------------------
  nombre_prod <- html_content %>%
    html_element("[data-test='product_title']") %>% html_text2()
  
  Precio_comprador <- html_content %>%
    html_element("div.PdpStock_prices_offer__Nbw2R[data-test='offer-price']") %>%
    html_text2() %>% str_extract("\\$?[\\d\\.,]+")
  
  Precio_anterior <- html_content %>%
    html_element("div.PdpStock_prices_real__bpBpU[data-test='display-price']") %>%
    html_text2() %>% str_extract("\\$?[\\d\\.,]+")
  
  precio_sin_impuestos <- html_content %>%
    html_element("[data-test='transparency-law-text']") %>%
    html_text2() %>% str_extract("\\$?[\\d\\.,]+")
  
  # (f) Tabla de especificaciones --------------------------------------------
  spec_nodes_table <- html_content %>% html_elements("div.Pdp_divFlex__TSPiC")
  spec_nodes_icon  <- html_content %>% html_elements("div.Pdp_divIconFlex__cQCSX")
  specs_list <- list()
  
  if (length(spec_nodes_table) > 0)
    specs_list <- append(specs_list,
                         map(spec_nodes_table, \(node)
                             tibble(
                               label = node %>% html_element("div[class*='Pdp_divTitulo__']") %>%
                                 html_text2() %>% str_trim(),
                               value = node %>% html_element("div[class*='Pdp_divDescripcion__']") %>%
                                 html_text2() %>% str_trim()
                             )
                         )
    )
  
  if (length(spec_nodes_icon) > 0)
    specs_list <- append(specs_list,
                         map(spec_nodes_icon, \(node)
                             tibble(
                               label = node %>% html_element("div.Pdp_titleImg__p6ExU") %>%
                                 html_text2() %>% str_trim(),
                               value = node %>% html_element("div.Pdp_descriptionImg__koISR") %>%
                                 html_text2() %>% str_trim()
                             )
                         )
    )
  
  specs_df <- bind_rows(specs_list) %>%
    filter(!is.na(label) & label != "") %>%
    distinct(label, .keep_all = TRUE) %>%
    pivot_wider(names_from = label, values_from = value) %>%
    mutate(
      `Cámara frontal` = str_remove(`Cámara frontal`, "\\s*mpx$"),
      `Memoria RAM`    = str_remove(`Memoria RAM`,    "\\s*GB$"),
      `Cámara trasera` = str_remove(`Cámara trasera`, "\\s*mpx$")
    )
  
  # (g) Consolidar y devolver -------------------------------------------------
  base_info <- tibble(
    producto_url         = remote_driver$getCurrentUrl()[[1]],
    nombre               = nombre_prod,
    Precio_comprador     = Precio_comprador,
    Precio_anterior      = Precio_anterior,
    precio_sin_impuestos = precio_sin_impuestos,
    financiacion_plazo   = ifelse(nzchar(financiacion_plazo),
                                  financiacion_plazo, NA_character_)
  )
  
  bind_cols(base_info, specs_df)
}

# 3. INICIALIZAR SELENIUM ------------------------------------------------------
driver <- rsDriver(browser = "firefox", chromever = NULL,
                   port = 4435L, check = FALSE)
remote_driver <- driver$client
remote_driver$setTimeout("page load", 5000)

# 4. NAVEGACIÓN HASTA LISTADO DE CELULARES ------------------------------------
message("➡️ Navegando a Tienda Claro…")
remote_driver$navigate(CFG$home_url)
Sys.sleep(1.5)

tryCatch({
  celulares_link <- remote_driver$findElement(
    "xpath", "//a[.//div[normalize-space(.)='Celulares']]"
  )
  celulares_link$clickElement()
  message("✅ Entramos a ‘Celulares’.")
}, error = function(e) {
  warning("No se pudo hacer clic en ‘Celulares’. Voy al fallback.")
  remote_driver$navigate(CFG$cel_fallback)
})
Sys.sleep(1.5)

# 5. SCROLL PARA CARGAR PRODUCTOS --------------------------------------------
message("➡️ Scrolling para cargar todos los productos…")
n_products_found <- 0; attempts <- 0
repeat {
  remote_driver$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(1.5)
  current_products <- remote_driver$findElements(
    "css selector", "a[href^='/pdp/equipos/']"
  )
  if (length(current_products) == n_products_found) {
    attempts <- attempts + 1
    if (attempts >= 3) break
  } else {
    n_products_found <- length(current_products); attempts <- 0
  }
}

product_urls <- remote_driver$findElements(
  "css selector", "a[href^='/pdp/equipos/']"
) |> map_chr(~ .x$getElementAttribute("href")[[1]]) |> unique()

n_to_scrape <- min(length(product_urls), CFG$max_products)
message("➡️ Scrapearemos ", n_to_scrape, " productos.")

# 6. BUCLE PRINCIPAL -----------------------------------------------------------
rows <- vector("list", n_to_scrape)
for (i in seq_len(n_to_scrape)) {
  url <- product_urls[i]
  message("• Producto ", i, "/", n_to_scrape, ": ", basename(url))
  remote_driver$navigate(url); Sys.sleep(1.5)
  rows[[i]] <- tryCatch(
    extrae_ficha_html(remote_driver),
    error = function(e) tibble(
      producto_url = url, nombre = "ERROR EXTRACCIÓN",
      Precio_anterior = NA, Precio_comprador = NA
    )
  )
}

# 7. CONSOLIDAR + CSV ----------------------------------------------------------
if (length(rows) && any(lengths(rows) > 0)) {
  data_final <- bind_rows(rows) |> distinct() |> mutate(sitio = "Claro")
  
  # Creamos la carpeta 'raw' si no existe, para evitar errores.
  dir.create(dirname(CFG$out_csv), recursive = TRUE, showWarnings = FALSE)
  
  write_csv(data_final, CFG$out_csv, na = "")
  message("✅ CSV guardado con ", nrow(data_final), " filas en: ", CFG$out_csv)
} else {
  message("⚠️ No se extrajo ninguna fila.")
}

# 8. CERRAR SELENIUM -----------------------------------------------------------
remote_driver$close(); driver$server$stop()
message("✅ Sesión Selenium cerrada.")
