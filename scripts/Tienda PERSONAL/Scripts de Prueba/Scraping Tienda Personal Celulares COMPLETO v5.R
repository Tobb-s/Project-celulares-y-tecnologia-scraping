# ==============================================================================
# SCRAPING “Tienda Personal – Celulares” · multi-página  · v8-compat
# ==============================================================================

# 1. LIBRERÍAS -----------------------------------------------------------------
library(tidyverse)
library(rvest)
library(RSelenium)
library(xml2)
library(stringr)
library(R.utils)

# 2. CONFIGURACIÓN -------------------------------------------------------------
CFG <- list(
  start_url    = "https://tienda.personal.com.ar/celulares",
  max_pages    = 8L,
  timeout_prod = 30,
  out_csv      = "C:/Users/tobia/OneDrive/Desktop/Project-celulares-y-tecnologia-scraping/raw/personal_celulares_multipagina_v8.csv",
  spec_map = tribble(
    ~label_html,           ~col_name,
    "Sistema Operativo",   "Sistema Operativo",
    "Procesador",          "Procesador",
    "RAM",                 "RAM (GB)",
    "Memoria Interna",     "Almacenamiento interno (GB)",
    "Tamaño de pantalla",  "Pantalla (Pulgadas)",
    "Cámara principal",    "Cámara Principal (MP)",
    "Cámara frontal",      "Cámara frontal (MP)",
    "NFC",                 "NFC"
  ),
  legal_labels = c(
    "PRECIO DE LISTA",
    "PRECIO DE LISTA SIN IMPUESTOS",
    "PRECIO DE PROMOCIÓN",
    "PRECIO DE PROMOCIÓN SIN IMPUESTOS",
    "PRECIO CONEXIÓN TOTAL",
    "PRECIO CONEXIÓN TOTAL SIN IMPUESTOS"
  )
)

# 3. AUXILIARES ----------------------------------------------------------------
reemplaza_vacio <- function(x) if (length(x)==0 || is.na(x) || x=="") "" else x

scroll_step <- function(rd, n = 4){
  for(i in seq_len(n)){
    rd$executeScript("window.scrollBy(0, window.innerHeight);")
    Sys.sleep(0.6)
  }
}

extrae_ficha <- function(rd){
  cur_url <- rd$getCurrentUrl()[[1]]
  
  # --- Más info --------------------------------------------------------------
  info_html <- tryCatch({
    tabs <- rd$findElements("css selector", "div.l0q4lv2n > div.l0q4lv2o")
    if(length(tabs) >= 2){
      tabs[[2]]$clickElement(); Sys.sleep(1.2)
      read_html(rd$getPageSource()[[1]])
    } else NULL
  }, error = function(e) NULL)
  if(is.null(info_html)) return(tibble(producto_url = cur_url))
  
  # --- Especificaciones ------------------------------------------------------
  spec_xpath <- setNames(
    map_chr(CFG$spec_map$label_html,
            function(lbl) sprintf(
              "//div[contains(@class,'l0q4lv38') and normalize-space(.)='%s']/following-sibling::span[1]", lbl)),
    CFG$spec_map$col_name)
  
  specs_raw <- imap_chr(spec_xpath,
                        function(xp, nm) reemplaza_vacio(
                          html_text2(html_element(info_html, xpath = xp))))
  
  specs <- imap_chr(specs_raw, function(v, col){
    v <- str_trim(v)
    if(col %in% c("RAM (GB)", "Almacenamiento interno (GB)"))
      str_extract(v,"\\d+")
    else if(col %in% c("Cámara Principal (MP)", "Cámara frontal (MP)"))
      str_extract(v,"\\d+")
    else if(col == "Pantalla (Pulgadas)")
      str_extract(v,"\\d+(?:\\.\\d+)?")
    else v
  })
  
  # --- Legales ---------------------------------------------------------------
  leg_html <- tryCatch({
    tabs <- rd$findElements("css selector", "div.l0q4lv2n > div.l0q4lv2o")
    if(length(tabs) >= 3){
      tabs[[3]]$clickElement(); Sys.sleep(1.2)
      read_html(rd$getPageSource()[[1]])
    } else NULL
  }, error = function(e) NULL)
  
  leg_txt <- if(!is.null(leg_html)) html_text2(leg_html) else ""
  legales <- setNames(
    map_chr(CFG$legal_labels, function(lbl){
      m <- str_match(leg_txt, paste0(lbl,"\\s*:?.*?\\$?\\s*([0-9\\.]+)"))
      reemplaza_vacio(if(!is.na(m[,2])) m[,2] else "")
    }),
    CFG$legal_labels)
  
  fin_txt <- tryCatch({
    if(!is.null(leg_html))
      html_element(leg_html,"div.l0q4lv1h > div.l0q4lv1i") %>% html_text2() %>% reemplaza_vacio()
    else ""
  }, error = function(e) "")
  legales["Financiación"] <- if(fin_txt!="") paste(str_trim(unlist(str_split(fin_txt,"\\n"))),collapse=" | ") else ""
  
  # --- Tibble final ----------------------------------------------------------
  tibble(producto_url = cur_url,
         nombre = html_element(info_html,"h1") %>% html_text2() %>% reemplaza_vacio()) %>% 
    bind_cols(as_tibble_row(specs)) %>% 
    bind_cols(as_tibble_row(legales))
}

# 4. ARRANCAR SELENIUM ---------------------------------------------------------
driver <- rsDriver(
  browser    = "firefox",
  chromever  = NULL,
  port       = 4435L,
  check      = FALSE
)
remote_driver <- driver$client


# 5. LOOP DE PÁGINAS -----------------------------------------------------------
rows <- list(); omitidos <- character(); page_ix <- 1L

repeat{
  message(sprintf("\n📄 Página %d – extrayendo URLs …", page_ix))
  scroll_step(rd, 4)
  cards <- tryCatch(rd$findElements("css selector","a[data-testid='product-card-container']"),
                    error=function(e) list())
  if(length(cards)==0){message("Sin productos: fin."); break}
  urls <- vapply(cards, function(x) x$getElementAttribute("href")[[1]], character(1))
  message("  • ", length(urls), " productos.")
  
  for(k in seq_along(urls)){
    u <- urls[k]; message(sprintf("  └─ [%d/%d] %s", k, length(urls), u))
    try(rd$navigate(u), silent = TRUE); Sys.sleep(1)
    fila <- tryCatch(withTimeout(extrae_ficha(rd), timeout = CFG$timeout_prod, onTimeout = "silent"),
                     error=function(e) NULL)
    if(is.null(fila) || !"nombre" %in% names(fila) || fila$nombre=="")
      omitidos <- c(omitidos, u)
    else
      rows <- append(rows, list(fila))
  }
  
  if(page_ix >= CFG$max_pages){message("Máx. páginas alcanzado."); break}
  
  next_num <- page_ix + 1L; got_next <- FALSE
  
  # 5A. scroll + botón Siguiente ----------------------------------------------
  scroll_step(rd, 5)
  siguiente <- tryCatch(
    rd$findElement("xpath","//*[(@aria-label='Siguiente' or contains(@class,'next') or contains(.,'Siguiente')) and (self::a or self::button)]"),
    error=function(e) NULL)
  if(!is.null(siguiente)){
    message("▶️  Click en «Siguiente»"); siguiente$clickElement(); got_next <- TRUE
  }
  
  # 5B. link con ?page= --------------------------------------------------------
  if(!got_next){
    page_src <- rd$getPageSource()[[1]]; doc <- read_html(page_src)
    hrefs <- doc %>% html_nodes("a[href*='page=']") %>% html_attr("href") %>% unique()
    nums  <- as.integer(str_extract(hrefs,"(?<=page=)\\d+"))
    cand  <- hrefs[which(nums>page_ix)][order(nums[nums>page_ix])][1]
    if(!is.na(cand)){
      if(!grepl("^https?://",cand)) cand <- paste0("https://tienda.personal.com.ar", cand)
      message("▶️  Navegando a página ", next_num, " (link page=)")
      rd$navigate(cand); got_next <- TRUE
    }
  }
  
  # 5C. URL manual -------------------------------------------------------------
  if(!got_next){
    cur <- rd$getCurrentUrl()[[1]]
    base <- sub("(\\?|&)page\\=.*","", cur)
    sep  <- ifelse(grepl("\\?", base), "&", "?")
    cand <- paste0(base, sep, "page=", next_num)
    message("▶️  Navegando a página ", next_num, " (URL manual)")
    rd$navigate(cand); got_next <- TRUE
  }
  
  # 5D. esperar tarjetas -------------------------------------------------------
  if(got_next){
    scroll_step(rd, 3)
    t0 <- Sys.time(); ok <- FALSE
    while(!ok && difftime(Sys.time(), t0, units = "secs") < 20){
      ok <- tryCatch(
        length(rd$findElements("css selector","a[data-testid='product-card-container']"))>0,
        error=function(e) FALSE)
      if(!ok){Sys.sleep(1); scroll_step(rd,1)}
    }
    if(!ok){warning("⚠️ Timeout página ", next_num); break}
    page_ix <- next_num
  } else {
    message("No se avanzó a página ", next_num,"; fin."); break
  }
}

# 6. CONSOLIDAR ---------------------------------------------------------------
if(length(rows)>0){
  final <- bind_rows(rows) %>% distinct()
  write_csv(final, CFG$out_csv)
  message("\n✅ ", nrow(final), " filas guardadas en: ", CFG$out_csv)
}else message("\n⚠️ Sin filas para guardar.")
if(length(omitidos)>0){
  message("\nProductos omitidos:")
  walk(omitidos, function(u) message("  - ", u))
}

# 7. CIERRE -------------------------------------------------------------------
try(rd$close(), silent = TRUE)
try(drv$server$stop(), silent = TRUE)
message("\n👋 Script finalizado.")
