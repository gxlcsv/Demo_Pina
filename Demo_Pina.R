# Demo_Pina.R — Demostración Proyecto Piña — v5.0
# ==============================================================================
# App Shiny para clasificar y visualizar el estado de bloques de piña a partir
# de índices vegetativos (clorofila, nitrógeno, biomasa, estrés) y temperatura.
#
# Características:
# - Carga vectorial (GPKG) y tabla (Excel/CSV) desde rutas definidas en el script.
# - Unión por bloque usando la columna "Name" (puede cambiarse en CONFIG$columns).
# - Normalización robusta de IDs (evita desajustes por mayúsculas, guiones, ceros).
# - Clasificación por índice con 3 modos: Percentiles, Jenks, o Manual (sliders).
# - Reglas de estrés: por defecto "estrés alto = bueno" (puedes invertirlo con un checkbox).
# - “Score final” = promedio de índices + (opcional) peso de Temperatura.
# - Rango de fechas (filtra todo: mapa y gráficas).
# - Zoom automático a capa + botón “Zoom a bloques”.
# - Paletas fijas: semáforo para índices; azul-amarillo-rojo para temperatura.
# - Bajo el mapa: medianas temporales (índices y temperatura) del rango de fechas.
# - En “Gráficas”: comparar 1–2 bloques para un índice seleccionado.
# - Descargas: CSV y GPKG clasificado del mes activo.
#
# Requisitos mínimos:
# - "Bloques.gpkg" con una capa poligonal o puntual que contenga la columna "Name".
# - "Datos_Indice_Vegetativos.xlsx" con columnas de índices y temperatura por bloque/fecha.
#
# ------------------------------------------------------------------------------


# --------------------- RUTAS -----------------------------------------
# Directorio de datos. Cambia solo esta variable para mover todo el proyecto.
# Directorio de datos relativo al proyecto
data_dir <- "data"  # <-- así funciona igual en tu PC y en Connect

# Rutas absolutas a los archivos de entrada (vector y tabla).
path_vector <- file.path(data_dir, "Bloques.gpkg")
path_table  <- file.path(data_dir, "Datos_Indice_Vegetativos.xlsx")


# --------------------- CONFIG -----------------------------------------
# En CONFIG defines:
# - Ruta y capa del GPKG (si hay varias capas).
# - Hoja del Excel.
# - CRS asumido si el vector no trae proyección.
# - Mapeo de columnas clave: por defecto usamos "Name" como ID en vector y tabla.
CONFIG <- list(
  vector_path = path_vector,
  gpkg_layer  = "Bloques",   # Usa sf::st_layers(path_vector) para listar nombres disponibles
  table_path  = path_table,
  table_sheet = "Hoja1",
  assumed_crs = 4326,        # Si el GPKG no trae CRS, se asume EPSG:4326
  columns = list(
    id_vector = "Name",      # Columna de ID en el vector (GPKG)
    id_table  = "Name",      # Columna de ID en la tabla (Excel/CSV) — debe relacionar con id_vector
    date   = NULL,           # Si el Excel ya trae una fecha completa, pon aquí el nombre (ej: "Fecha")
    year   = NULL,           # Alternativa a date: columna Año / Year
    month  = NULL,           # Alternativa a date: columna Mes / Month
    # Índices vegetativos y temperatura. Pon los nombres exactos si ya los sabes.
    clorofila = "Clorofila",
    nitrogeno = "Nitrogeno",
    biomasa   = "NDVI",
    estres    = "Estres",
    temp      = "Temperatura"
  )
)


# ---- Paquetes ----
# En producción (Posit Connect) NO instales paquetes en runtime.
# Cárgalos y, si falta alguno, aborta con un mensaje claro.
libs <- c("shiny","leaflet","sf","sp","dplyr","stringr","DT","readxl","readr",
          "forcats","plotly","tools","tidyr","classInt","lubridate","htmltools","purrr")

# Carga silenciosa; 'require' devuelve TRUE/FALSE por paquete
ok <- sapply(libs, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
if (!all(ok)) {
  missing_libs <- paste(libs[!ok], collapse = ", ")
  stop(paste0("Faltan paquetes: ", missing_libs,
              "\nInstálalos localmente (o verifica que Connect los provea) y vuelve a publicar."))
}

options(shiny.maxRequestSize = 200*1024^2)   # Permite archivos relativamente grandes

# (solo para desarrollo local) Manifest opcional:
# if (interactive()) rsconnect::writeManifest(appPrimaryDoc = "Demo_Pina.R")


# --------------------- Helpers ----------------------------------------
# Utilidades usadas en varias partes del código

# Convierte un vector o named vector a "choices" para inputs
to_choices <- function(x) { n <- names(x); if (is.null(n)) setNames(as.list(x), x) else setNames(as.list(unname(x)), n) }

# Quita tildes; útil para normalizar nombres de columnas/meses
strip_accents <- function(x) { x <- gsub("[áÁ]","a",x); x <- gsub("[éÉ]","e",x); x <- gsub("[íÍ]","i",x); x <- gsub("[óÓ]","o",x); x <- gsub("[úÚüÜ]","u",x); x }
strip_all <- function(x) strip_accents(tolower(x))

# Normaliza IDs de bloque:
# - Recorta espacios, pasa a mayúscula y deja solo A–Z/0–9.
# - Si hay dígitos: elimina ceros a la izquierda (B01 -> 1).
# - Evita descalces entre "B1" vs "B01" o "B-01".
normalize_id <- function(x) {
  s <- as.character(x); s <- trimws(s); s <- toupper(s)
  digits <- gsub("[^0-9]", "", s, perl = TRUE)
  has_digits <- nzchar(digits)
  id_num <- digits
  id_num[has_digits] <- sub("^0+", "", id_num[has_digits])
  id_num[id_num == ""] <- NA_character_
  alnum <- gsub("[^A-Z0-9]", "", s, perl = TRUE)
  ifelse(has_digits, id_num, alnum)
}

# Lector vectorial robusto: GPKG/GeoJSON/ZIP(shp). Asegura geometrías válidas y CRS 4326.
read_vector_any <- function(filepath, layer = NULL) {
  fpath <- try(normalizePath(filepath, winslash = "/", mustWork = TRUE), silent = TRUE)
  if (inherits(fpath,"try-error")) fpath <- filepath
  ext <- tolower(tools::file_ext(fpath))
  if (ext == "gpkg") {
    lyrlist <- try(sf::st_layers(fpath)$name, silent = TRUE)
    if (is.null(layer) || !nzchar(layer)) if (!inherits(lyrlist,"try-error") && length(lyrlist)) layer <- lyrlist[1]
    v <- try(sf::st_read(fpath, layer = layer, quiet = TRUE), silent = TRUE)
    if (inherits(v,"try-error")) { v <- sf::st_read(fpath, quiet = TRUE) }
  } else if (ext == "zip") {
    td <- tempfile("shp_"); dir.create(td); utils::unzip(fpath, exdir = td)
    all_files <- list.files(td, recursive = TRUE, full.names = TRUE)
    shp <- all_files[tolower(tools::file_ext(all_files)) == "shp"]
    if (!length(shp)) stop("El .zip no contiene un .shp válido.")
    v <- sf::st_read(shp[1], quiet = TRUE)
  } else if (ext %in% c("geojson","json")) {
    v <- sf::st_read(fpath, quiet = TRUE)
  } else stop("Formato vectorial no soportado: ", ext)
  
  v <- sf::st_make_valid(v)               # Geometrías válidas
  v <- sf::st_zm(v, drop = TRUE, what = "ZM") # Quita Z/M si existen
  if (is.na(sf::st_crs(v)) && !is.null(CONFIG$assumed_crs)) v <- sf::st_set_crs(v, CONFIG$assumed_crs)
  try({ v <- sf::st_transform(v, 4326) }, silent = TRUE) # A WGS84 para Leaflet
  v <- v[!sf::st_is_empty(v), ]
  v
}

# Lector tabular robusto: Excel (por hoja) o CSV (auto detecta coma/;).
read_table_any <- function(filepath, sheet = NULL) {
  fpath <- try(normalizePath(filepath, winslash = "/", mustWork = TRUE), silent = TRUE)
  if (inherits(fpath,"try-error")) fpath <- filepath
  ext <- tolower(tools::file_ext(fpath))
  if (ext %in% c("xlsx","xls")) {
    if (is.null(sheet)) sheet <- readxl::excel_sheets(fpath)[1]
    as.data.frame(readxl::read_excel(fpath, sheet = sheet))
  } else if (ext == "csv") {
    tb <- tryCatch(readr::read_csv(fpath, show_col_types = FALSE),
                   error = function(e) readr::read_csv2(fpath, show_col_types = FALSE))
    as.data.frame(tb)
  } else stop("Formato tabular no soportado: ", ext)
}

# Parser flexible de fechas. Acepta:
# - Date nativo
# - Números Excel (base 1899-12-30)
# - YYYYMM/ YYYY-MM / "mes-año" en español/inglés
# - Múltiples formatos dd/mm/yyyy, etc.
smart_parse_date <- function(x) {
  if (inherits(x,"Date")) return(as.Date(x))
  if (is.numeric(x)) {
    if (any(x>=20000 & x<=60000, na.rm=TRUE)) return(as.Date(x, origin = "1899-12-30"))
    if (all(x %% 1 == 0) && any(x>=190001 & x<=209912, na.rm=TRUE)) {
      yy <- x %/% 100; mm <- x %% 100; mm[mm<1|mm>12] <- 1
      return(as.Date(sprintf("%04d-%02d-01", yy, mm)))
    }
  }
  s <- trimws(as.character(x)); s[s==""] <- NA_character_
  low <- tolower(iconv(s, to = "ASCII//TRANSLIT"))
  rep_es <- c("enero"="january","febrero"="february","marzo"="march","abril"="april","mayo"="may","junio"="june",
              "julio"="july","agosto"="august","septiembre"="september","setiembre"="september","octubre"="october",
              "noviembre"="november","diciembre"="december","ene"="jan","feb"="feb","mar"="mar","abr"="apr","may"="may",
              "jun"="jun","jul"="jul","ago"="aug","sep"="sep","set"="sep","oct"="oct","nov"="nov","dic"="dec")
  low2 <- low; for (k in names(rep_es)) low2 <- gsub(k, rep_es[[k]], low2)
  low2 <- gsub("[[:space:]/_.]+", "-", low2)
  low2 <- ifelse(grepl("^[0-9]{4}-[0-9]{1,2}$", low2), paste0(low2,"-01"), low2)
  low2 <- ifelse(grepl("^[0-9]{1,2}-[0-9]{4}$", low2),
                 {mm<-sub("-.*$","",low2); yy<-sub("^[^-]*-","",low2); sprintf("%s-%02d-01",yy,as.integer(mm))},
                 low2)
  ords <- c("Y-m-d","d-m-Y","m-d-Y","Y/m/d","d/m/Y","m/Y","Y/m","Y-m","b-Y","d-b-Y","B-Y","d-B-Y")
  as.Date(suppressWarnings(lubridate::parse_date_time(low2, orders = ords, exact = FALSE)))
}

# Detecta familia geométrica para pintar en Leaflet (point/line/polygon)
geom_family <- function(x) {
  t <- unique(as.character(sf::st_geometry_type(x, by_geometry = TRUE)))
  if (!length(t)) return("other")
  if (all(grepl("POINT", t))) return("point")
  if (all(grepl("LINESTRING", t))) return("line")
  if (all(grepl("POLYGON", t))) return("polygon")
  if (any(grepl("POLYGON", t))) return("polygon")
  if (any(grepl("LINESTRING", t))) return("line")
  if (any(grepl("POINT", t))) return("point")
  "other"
}

# Añade geometrías al mapa probando varias estrategias si la primera falla:
# - sf nativo -> cast a MULTI -> cast a tipo simple -> coerción a Spatial
safe_add <- function(proxy, fam, dat, add_args) {
  try1 <- try({
    if (fam == "point") do.call(leaflet::addCircleMarkers, c(list(map = proxy, data = dat), add_args))
    else if (fam == "line") do.call(leaflet::addPolylines,  c(list(map = proxy, data = dat), add_args))
    else do.call(leaflet::addPolygons, c(list(map = proxy, data = dat), add_args))
  }, silent = TRUE)
  if (!inherits(try1,"try-error")) return(try1)
  dat2 <- try({ if (fam=="polygon") sf::st_cast(dat,"MULTIPOLYGON") else if (fam=="line") sf::st_cast(dat,"MULTILINESTRING") else sf::st_cast(dat,"POINT") }, silent=TRUE)
  if (!inherits(dat2,"try-error")) {
    try2 <- try({
      if (fam == "point") do.call(leaflet::addCircleMarkers, c(list(map = proxy, data = dat2), add_args))
      else if (fam == "line") do.call(leaflet::addPolylines,  c(list(map = proxy, data = dat2), add_args))
      else do.call(leaflet::addPolygons, c(list(map = proxy, data = dat2), add_args))
    }, silent = TRUE)
    if (!inherits(try2,"try-error")) return(try2)
  }
  spdat <- try(suppressWarnings(methods::as(dat,"Spatial")), silent = TRUE)
  if (!inherits(spdat,"try-error")) {
    try3 <- try({
      if (fam == "point") do.call(leaflet::addCircleMarkers, c(list(map = proxy, data = spdat), add_args))
      else if (fam == "line") do.call(leaflet::addPolylines,  c(list(map = proxy, data = spdat), add_args))
      else do.call(leaflet::addPolygons, c(list(map = proxy, data = spdat), add_args))
    }, silent = TRUE)
    if (!inherits(try3,"try-error")) return(try3)
  }
  stop(try1)
}

# Expande un bbox en porcentaje para evitar que el fitBounds quede muy ajustado
expand_bbox <- function(bb, pad = 0.04) {
  xmn <- as.numeric(bb["xmin"]); xmx <- as.numeric(bb["xmax"]); ymn <- as.numeric(bb["ymin"]); ymx <- as.numeric(bb["ymax"])
  if (!all(is.finite(c(xmn,xmx,ymn,ymx)))) return(NULL)
  if (xmx-xmn==0) { xmn <- xmn-0.002; xmx <- xmx+0.002 }
  if (ymx-ymn==0) { ymn <- ymn-0.002; ymx <- ymx+0.002 }
  dx <- xmx-xmn; dy <- ymx-ymn
  c(xmn - dx*pad, ymn - dy*pad, xmx + dx*pad, ymx + dy*pad)  # vector numérico sin nombres
}

# Mapeo automático de columnas a partir de CONFIG y nombres reales en tabla/vector.
# Si CONFIG$columns trae nombres explícitos, se usan.
# Si no, intentamos descubrir las columnas por aproximación (match exacto y por patrón).
resolve_colmap <- function(tb, v) {
  CM <- CONFIG$columns
  pick <- function(cands, pool_names) {
    if (length(pool_names) == 0) return("")
    nn <- strip_all(pool_names)
    for (c in cands) { hit <- which(nn == c); if (length(hit)) return(pool_names[hit[1]]) }
    for (c in cands) { hit <- which(grepl(c, nn)); if (length(hit)) return(pool_names[hit[1]]) }
    ""
  }
  cnv  <- names(v);  cnt  <- names(tb);  cntn <- strip_all(cnt)
  id_vector <- if (!is.null(CM$id_vector) && CM$id_vector %in% cnv) CM$id_vector else pick(c("name","nombre","bloque","id","codigo"), cnv)
  id_table  <- if (!is.null(CM$id_table)  && CM$id_table  %in% cnt) CM$id_table  else pick(c("name","nombre","bloque","id","codigo"), cnt)
  date  <- if (!is.null(CM$date) && CM$date %in% cnt) CM$date else {
    k <- which(sapply(tb, inherits, what = "Date") | grepl("fecha|date|fech", cntn)); if (length(k)) cnt[k[1]] else ""
  }
  year  <- if (!is.null(CM$year)  && CM$year  %in% cnt) CM$year  else pick(c("año","ano","anio","year"), cnt)
  month <- if (!is.null(CM$month) && CM$month %in% cnt) CM$month else pick(c("mes","month"), cnt)
  clorofila <- if (!is.null(CM$clorofila) && CM$clorofila %in% cnt) CM$clorofila else pick(c("clorofila","chlorofila","clor","chlor","ci","cig","mcari"), cnt)
  nitrogeno <- if (!is.null(CM$nitrogeno) && CM$nitrogeno %in% cnt) CM$nitrogeno else pick(c("nitrogeno","nitrógeno","ndre","gndvi","cigreen","cig","n_index","nindex","n_idx"), cnt)
  biomasa   <- if (!is.null(CM$biomasa)   && CM$biomasa   %in% cnt) CM$biomasa   else pick(c("biomasa","ndvi","osavi","vigorcultivo","vegetacion","veg"), cnt)
  estres    <- if (!is.null(CM$estres)    && CM$estres    %in% cnt) CM$estres    else pick(c("estres","estrés","stress","msi","ndwi","tsi","pri","psii"), cnt)
  temp      <- if (!is.null(CM$temp)      && CM$temp      %in% cnt) CM$temp      else pick(c("temperatura","temp","tmedia","t_prom","t"), cnt)
  list(id_vector=id_vector, id_table=id_table, date=date, year=year, month=month,
       clorofila=clorofila, nitrogeno=nitrogeno, biomasa=biomasa, estres=estres, temp=temp)
}

# Paletas fijas (semáforo para índices; azul-amarillo-rojo para temperatura).
LEVELS_CLASE <- c("DEFICIENTE","REGULAR","BUENO")
TEMP_LABELS  <- c("Temperatura Baja","Temperatura Media","Temperatura Alta")
pal_index <- c("DEFICIENTE"="#d73027","REGULAR"="#fee08b","BUENO"="#1a9850")
pal_temp  <- c("Temperatura Baja"="#4575b4","Temperatura Media"="#fee08b","Temperatura Alta"="#d73027")


# ------------------------- UI ------------------------
ui <- fluidPage(
  titlePanel("Demostración Proyecto Piña"),
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Parámetros"),
                 # Modo de clasificación por índice:
                 # - pct: terciles por percentiles
                 # - jenks: 3 clases por JENKS (mejor cuando hay modas)
                 # - manual: slider para t1/t2 por índice en el mes activo
                 radioButtons("class_mode","Modo de umbrales por índice",
                              choices = to_choices(c("Automático (Percentiles)"="pct",
                                                     "Automático (Jenks)"="jenks",
                                                     "Manual (barras por índice)"="manual")),
                              selected = "jenks"),
                 # Por defecto, "estrés alto = bueno" (en piña suele interpretarse así).
                 # Marca el checkbox para invertirlo.
                 checkboxInput("invert_stress","Invertir Estrés (marca si 'alto = malo')", value = FALSE),
                 # Peso de temperatura en el score final (0 = ignora T°, 1 = solo T°)
                 sliderInput("temp_weight","Peso relativo de Temperatura", min=0, max=1, value=0.2, step=0.05),
                 
                 # Rango de 'Temperatura Media' (por defecto 35–40 °C).
                 numericInput("temp_mid_min","Temperatura Media — mínimo (°C)", value = 35, step = 0.5),
                 numericInput("temp_mid_max","Temperatura Media — máximo (°C)", value = 40, step = 0.5),
                 
                 # “Color por”: score final o una de las capas de clasificación
                 selectInput("view_mode","Color por",
                             choices = to_choices(c("Score final"="score","Clorofila"="clorofila",
                                                    "Nitrógeno"="nitrogeno","Biomasa"="biomasa",
                                                    "Estrés"="estres","Temperatura"="temp")),
                             selected = "score"),
                 
                 # Sliders manuales por índice (aparecen solo si "manual")
                 conditionalPanel(
                   condition = "input.class_mode == 'manual'",
                   h4("Umbrales manuales (por índice)"),
                   uiOutput("ui_thr_clorofila"),
                   uiOutput("ui_thr_nitrogeno"),
                   uiOutput("ui_thr_biomasa"),
                   uiOutput("ui_thr_estres"),
                   tags$small("Sugeridos desde la distribución del mes seleccionado.")
                 ),
                 tags$hr(),
                 # Botón para re-centrar el mapa al bbox de la capa
                 actionButton("btn_recenter", "Zoom a bloques")
    ),
    mainPanel(width = 8,
              tabsetPanel(id = "tabs",
                          tabPanel("Mapa",
                                   fluidRow(
                                     column(4, uiOutput("ui_month")),         # selector de mes (YYYY-MM) dentro del rango
                                     column(5, uiOutput("ui_daterange")),     # rango de fechas global (afecta mapa y gráficas bajo el mapa)
                                     column(3, checkboxInput("show_medians","Mostrar medianas", value = TRUE))
                                   ),
                                   leafletOutput("map", height = 520),
                                   conditionalPanel("input.show_medians == true", br()),
                                   conditionalPanel("input.show_medians == true", h4("Mediana de todos los Bloques — Índices")),
                                   conditionalPanel("input.show_medians == true", plotlyOutput("plt_idx_median_map", height = 280)),
                                   conditionalPanel("input.show_medians == true", h4("Mediana de todos los Bloques — Temperatura")),
                                   conditionalPanel("input.show_medians == true", plotlyOutput("plt_temp_median_map", height = 240))
                          ),
                          tabPanel("Tabla", DTOutput("tabla")),
                          tabPanel("Gráficas",
                                   # Comparador 1–2 bloques y selección del índice a graficar
                                   fluidRow(
                                     column(7, uiOutput("ui_blocks_plot_g")),
                                     column(5, uiOutput("ui_index_pick"))
                                   ),
                                   br(),
                                   plotlyOutput("plt_idx_comp", height = 360)
                          ),
                          tabPanel("Descargas",
                                   downloadButton("dl_csv", "CSV clasificado (mes)"),
                                   downloadButton("dl_gpkg", "GPKG clasificado (mes)")
                          )
              )
    )
  )
)


# ----------------------- Server ----------------------
server <- function(input, output, session) {
  # rv: contenedor reactivo de estado compartido en toda la sesión
  rv <- reactiveValues(
    v=NULL, tb=NULL, cm=NULL,          # v: sf de bloques; tb: tabla cruda; cm: mapeo de columnas
    month_choices=NULL, key_month=NULL,# meses disponibles (YYYY-MM) y mes activo
    bbx=NULL, did_zoom=FALSE,          # bbox para zoom y flag para no repetir zoom inicial
    date_min=NULL, date_max=NULL       # fechas min/max detectadas en la tabla
  )
  
  # ----- Mapa base: dos fondos + control de capas
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
      addLayersControl(baseGroups = c("Satélite","Claro"),
                       overlayGroups = c("Clasificación"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng=-74, lat=4, zoom=5)  # Vista genérica (Colombia). Luego hacemos fitBounds al cargar la capa.
  })
  # Evita que el mapa “suspenda” eventos al cambiar de pestaña
  try(outputOptions(output, "map", suspendWhenHidden = FALSE), silent = TRUE)
  
  # Helper: realiza el fitBounds con validación defensiva
  do_zoom <- function(bbx) {
    if (is.null(bbx)) return(invisible(NULL))
    vals <- as.numeric(bbx)
    if (length(vals)!=4 || any(!is.finite(vals))) return(invisible(NULL))
    try(leafletProxy("map") %>% fitBounds(vals[1], vals[2], vals[3], vals[4]), silent = TRUE)
    invisible(TRUE)
  }
  
  # ----- Carga de datos (una sola vez al inicio)
  observeEvent(TRUE, {
    # Validaciones tempranas de rutas
    if (!file.exists(CONFIG$vector_path)) { showNotification("No existe vector_path.", type="error", duration=8); return(NULL) }
    if (!file.exists(CONFIG$table_path))  { showNotification("No existe table_path.",  type="error", duration=8); return(NULL) }
    
    # Lecturas robustas
    v  <- try(read_vector_any(CONFIG$vector_path, layer = CONFIG$gpkg_layer), silent = TRUE)
    if (inherits(v,"try-error")) { showNotification("Error leyendo GPKG.", type="error", duration=8); return(NULL) }
    tb <- try(read_table_any(CONFIG$table_path, sheet = CONFIG$table_sheet), silent = TRUE)
    if (inherits(tb,"try-error")) { showNotification("Error leyendo Excel.", type="error", duration=8); return(NULL) }
    
    rv$v  <- v
    rv$cm <- resolve_colmap(tb, v)  # Mapeo de columnas en función de CONFIG + nombres reales
    rv$tb <- tb
    
    # Prepara bbox para zoom inicial
    bb  <- try(sf::st_bbox(v), silent = TRUE)
    if (!inherits(bb,"try-error")) rv$bbx <- expand_bbox(bb, pad=0.04)
    if (!is.null(rv$bbx)) do_zoom(rv$bbx)  # Primer zoom automático
  }, once = TRUE)
  
  # Reafirma el zoom tras el primer flush de UI (cubre casos de timing)
  session$onFlushed(function() {
    isolate({ if (!isTRUE(rv$did_zoom) && !is.null(rv$bbx)) { do_zoom(rv$bbx); rv$did_zoom <- TRUE } })
  }, once = FALSE)
  
  # Si vuelves a la pestaña Mapa, recenter si aún no hay zoom aplicado
  observeEvent(input$tabs, { if (identical(input$tabs, "Mapa") && !is.null(rv$bbx)) do_zoom(rv$bbx) }, ignoreInit = TRUE)
  
  # ----- Consolidación de fecha (crea tb$Fecha si no existía)
  tb_with_date <- reactive({
    req(rv$tb, rv$cm)
    tb <- rv$tb; cm <- rv$cm
    
    # 1) Si existe una columna DATE (completa), úsala para construir Year/Month
    if (nzchar(cm$date)) {
      f <- smart_parse_date(tb[[cm$date]])
      tb$Fecha <- f
      tb$Year  <- ifelse(is.na(f), NA_integer_, as.integer(format(f,"%Y")))
      tb$Month <- ifelse(is.na(f), NA_integer_, as.integer(format(f,"%m")))
    }
    # 2) Si no, intenta con Year/Month por separado
    if (is.null(tb$Fecha) || all(is.na(tb$Fecha))) {
      if (nzchar(cm$year))  tb$Year  <- suppressWarnings(as.integer(tb[[cm$year]]))
      if (nzchar(cm$month)) {
        mcol <- tb[[cm$month]]; mtxt <- tolower(iconv(as.character(mcol), to="ASCII//TRANSLIT"))
        map_m <- c("enero"=1,"febrero"=2,"marzo"=3,"abril"=4,"mayo"=5,"junio"=6,"julio"=7,
                   "agosto"=8,"septiembre"=9,"setiembre"=9,"octubre"=10,"noviembre"=11,"diciembre"=12,
                   "ene"=1,"feb"=2,"mar"=3,"abr"=4,"may"=5,"jun"=6,"jul"=7,"ago"=8,"sep"=9,"set"=9,"oct"=10,"nov"=11,"dic"=12)
        mm <- suppressWarnings(as.integer(mcol)); if (all(is.na(mm))) mm <- unname(map_m[mtxt]); tb$Month <- mm
      }
      if (!("Year" %in% names(tb)) || !("Month" %in% names(tb))) return(NULL)
      tb$Fecha <- as.Date(sprintf("%04d-%02d-01", as.integer(tb$Year), as.integer(tb$Month)))
    }
    
    # Normaliza el ID de tabla (string no vacío)
    tmp_id <- normalize_id(tb[[rv$cm$id_table]])
    tb$.id_norm <- as.character(tmp_id)
    tb$.id_norm[!nzchar(tb$.id_norm)] <- NA_character_
    
    # Guarda límites de fecha para inicializar el dateRangeInput
    rv$date_min <- suppressWarnings(min(tb$Fecha, na.rm = TRUE))
    rv$date_max <- suppressWarnings(max(tb$Fecha, na.rm = TRUE))
    tb
  })
  
  # ----- UI: Rango de Fechas (filtra mapa y series bajo el mapa y en “Gráficas”)
  output$ui_daterange <- renderUI({
    req(rv$date_min, rv$date_max)
    dateRangeInput("rng_dates","Rango de Fechas",
                   start = rv$date_min, end = rv$date_max,
                   min = rv$date_min, max = rv$date_max,
                   format = "yyyy-mm-dd")
  })
  
  # ----- Tabla filtrada por rango de fechas
  tb_range <- reactive({
    tb <- tb_with_date(); req(tb)
    if (is.null(input$rng_dates) || any(is.na(input$rng_dates))) return(tb)
    tb %>% dplyr::filter(Fecha >= input$rng_dates[1], Fecha <= input$rng_dates[2])
  })
  
  # ----- Meses disponibles (dentro del rango) y selector de mes activo
  observe({
    tb <- tb_range(); req(tb)
    ym <- tb %>% dplyr::mutate(ym = sprintf("%04d-%02d", Year, Month)) %>% dplyr::pull(ym) %>% unique() %>% sort()
    rv$month_choices <- ym
    if (is.null(rv$key_month) || !(rv$key_month %in% ym)) rv$key_month <- if (length(ym)) tail(ym,1) else NULL
    output$ui_month <- renderUI(selectInput("key_month","Mes", choices = to_choices(ym), selected = rv$key_month))
  })
  observeEvent(input$key_month, { rv$key_month <- input$key_month }, ignoreInit = TRUE)
  
  # ----- Sliders manuales: genera el UI por índice según datos del mes activo
  render_thr_ui <- function(colname, input_id, label) {
    req(tb_range(), rv$key_month, nzchar(colname))
    tb <- tb_range()
    parts <- strsplit(rv$key_month,"-")[[1]]
    yy <- as.integer(parts[1]); mm <- as.integer(parts[2])
    
    x <- suppressWarnings(as.numeric(tb[tb$Year==yy & tb$Month==mm, colname]))
    x <- x[is.finite(x)]
    if (!length(x)) return(NULL)
    
    rng <- range(x, na.rm=TRUE)
    if (rng[1]==rng[2]) {
      # Si todos los valores son iguales, “abre” un poco para evitar cortes coincidentes
      eps <- max(1e-6, 1e-6*max(abs(rng[1]),1,na.rm=TRUE))
      rng <- c(rng[1]-eps, rng[2]+eps)
    }
    # Cortes sugeridos por terciles si falla Jenks o manual
    qs <- try(stats::quantile(x, probs=c(1/3,2/3), na.rm=TRUE, names=FALSE), silent=TRUE)
    if (inherits(qs,"try-error") || any(!is.finite(qs)) || qs[2]<=qs[1]) {
      qs <- rng[1] + c(0.33,0.66)*(rng[2]-rng[1])
    }
    
    sliderInput(
      inputId = input_id, label = label,
      min = floor(rng[1]*1000)/1000, max = ceiling(rng[2]*1000)/1000,
      value = round(qs,3), step = signif((rng[2]-rng[1])/200, 2)
    )
  }
  output$ui_thr_clorofila <- renderUI({ if (input$class_mode=="manual" && nzchar(rv$cm$clorofila)) render_thr_ui(rv$cm$clorofila, "thr_clorofila", "Clorofila: cortes (Def-Rec / Rec-Bueno)") })
  output$ui_thr_nitrogeno <- renderUI({ if (input$class_mode=="manual" && nzchar(rv$cm$nitrogeno)) render_thr_ui(rv$cm$nitrogeno, "thr_nitrogeno", "Nitrógeno: cortes (Def-Rec / Rec-Bueno)") })
  output$ui_thr_biomasa   <- renderUI({ if (input$class_mode=="manual" && nzchar(rv$cm$biomasa))   render_thr_ui(rv$cm$biomasa,   "thr_biomasa",   "Biomasa (NDVI): cortes (Def-Rec / Rec-Bueno)") })
  output$ui_thr_estres    <- renderUI({ if (input$class_mode=="manual" && nzchar(rv$cm$estres))    render_thr_ui(rv$cm$estres,    "thr_estres",    "Estrés: cortes (Def-Rec / Rec-Bueno)") })
  
  # --------- CLASIFICACIÓN por mes (para mapa) ----------
  # Une la capa vectorial con la tabla del mes activo y calcula:
  # - Clase por índice (DEF/REG/BUEN) según modo elegido.
  # - temp_band (Baja/Media/Alta) y temp_CL (DEF/REG/BUEN) por temperatura.
  # - Score final = promedio de índices + peso de temperatura (opcional).
  classify_month <- reactive({
    req(rv$v, tb_range(), rv$cm, rv$key_month)
    
    v <- rv$v
    v$.id_norm <- as.character(normalize_id(v[[rv$cm$id_vector]]))
    
    tb <- tb_range()
    # Si no hay tabla o mes activo, devuelve la geometría con columnas vacías
    if (is.null(tb) || is.null(rv$key_month)) {
      out <- v
      out$Score <- NA_real_
      out$Clase <- factor(NA, levels = LEVELS_CLASE)
      out$clorofila_CL <- out$nitrogeno_CL <- out$biomasa_CL <- out$estres_CL <- factor(NA, levels = LEVELS_CLASE)
      out$temp_CL <- factor(NA, levels = LEVELS_CLASE)
      out$temp_band <- factor(NA, levels = TEMP_LABELS)
      attr(out, "__bbox__") <- tryCatch(expand_bbox(sf::st_bbox(out), pad=0.04), error=function(e) NULL)
      return(out)
    }
    
    # Filtra el mes activo (YYYY-MM) y prepara el .id_norm de la tabla
    parts <- strsplit(rv$key_month,"-")[[1]]
    yy <- as.integer(parts[1]); mm <- as.integer(parts[2])
    slice <- tb %>% dplyr::filter(Year == yy, Month == mm)
    
    if (!nrow(slice)) {
      # No hay registros ese mes -> devuelve sf con columnas NA
      out <- v
      out$Score <- NA_real_
      out$Clase <- factor(NA, levels = LEVELS_CLASE)
      out$clorofila_CL <- out$nitrogeno_CL <- out$biomasa_CL <- out$estres_CL <- factor(NA, levels = LEVELS_CLASE)
      out$temp_CL <- factor(NA, levels = LEVELS_CLASE)
      out$temp_band <- factor(NA, levels = TEMP_LABELS)
      attr(out, "__bbox__") <- tryCatch(expand_bbox(sf::st_bbox(out), pad=0.04), error=function(e) NULL)
      return(out)
    }
    
    if (!(".id_norm" %in% names(slice))) slice$.id_norm <- normalize_id(slice[[rv$cm$id_table]])
    slice$.id_norm <- as.character(slice$.id_norm)
    slice$.id_norm[!nzchar(slice$.id_norm)] <- NA_character_
    
    cols <- rv$cm
    on_names <- c("clorofila","nitrogeno","biomasa","estres")
    # Índices presentes en la tabla
    present <- on_names[sapply(on_names, function(nm) nzchar(cols[[nm]]) && cols[[nm]] %in% names(slice))]
    
    # Corta un vector numérico en 3 clases robustamente (evita “breaks no únicos”)
    .safe_cut <- function(x, t1, t2, dir=+1) {
      if (!any(is.finite(x))) return(factor(rep(NA_character_, length(x)), levels=LEVELS_CLASE))
      rng <- range(x, na.rm=TRUE)
      if (!is.finite(t1) || !is.finite(t2)) {
        if (rng[1]==rng[2]) { t1 <- rng[1]-1e-6; t2 <- rng[2]+1e-6 } else { t1 <- rng[1]+(rng[2]-rng[1])/3; t2 <- rng[1]+2*(rng[2]-rng[1])/3 }
      }
      if (t2 <= t1) { eps <- max(1e-9, 1e-6*max(abs(rng), na.rm=TRUE)); t1 <- t1-eps; t2 <- t2+eps }
      br <- c(-Inf,t1,t2,Inf); labs <- if (dir>=0) LEVELS_CLASE else rev(LEVELS_CLASE)
      cut(x, breaks=br, labels=labs, include.lowest=TRUE, right=TRUE)
    }
    # Clasifica un índice según el modo (pct/jenks/manual) y dirección (estrés puede invertirse)
    class_one <- function(x, mode, dir=+1, thr=NULL) {
      x <- suppressWarnings(as.numeric(x))
      if (!any(is.finite(x))) return(factor(rep(NA_character_, length(x)), levels=LEVELS_CLASE))
      ux <- unique(x[is.finite(x)])
      if (length(ux) <= 1) { labs <- if (dir>=0) LEVELS_CLASE else rev(LEVELS_CLASE); return(factor(rep("REGULAR", length(x)), levels=labs)) }
      if (mode=="pct") {
        q <- try(stats::quantile(x, probs=c(1/3,2/3), na.rm=TRUE, names=FALSE), silent=TRUE)
        if (inherits(q,"try-error") || any(!is.finite(q))) { rng <- range(x, na.rm=TRUE); q <- rng[1] + c(1/3,2/3)*(rng[2]-rng[1]) }
        .safe_cut(x, q[1], q[2], dir=dir)
      } else if (mode=="jenks") {
        br <- try(classInt::classIntervals(x[is.finite(x)], n=3, style='jenks')$brks, silent=TRUE)
        if (inherits(br,"try-error") || length(br)<4 || !all(is.finite(br))) { rng <- range(x, na.rm=TRUE); q <- rng[1]+c(1/3,2/3)*(rng[2]-rng[1]); .safe_cut(x, q[1], q[2], dir=dir) }
        else .safe_cut(x, br[2], br[3], dir=dir)
      } else {
        if (is.null(thr) || length(thr)!=2) return(factor(rep(NA_character_, length(x)), levels=LEVELS_CLASE))
        .safe_cut(x, min(thr), max(thr), dir=dir)
      }
    }
    # Clasifica el score final S en 3 clases según el modo
    class_score <- function(s, mode) {
      s <- suppressWarnings(as.numeric(s))
      if (!any(is.finite(s))) return(factor(rep(NA_character_, length(s)), levels=LEVELS_CLASE))
      us <- unique(s[is.finite(s)]); if (length(us) <= 1) return(factor(rep("REGULAR", length(s)), levels=LEVELS_CLASE))
      if (mode=="pct") { q <- stats::quantile(s, probs=c(1/3,2/3), na.rm=TRUE, names=FALSE); return(.safe_cut(s, q[1], q[2], dir=+1)) }
      if (mode=="jenks") {
        br <- try(classInt::classIntervals(s[is.finite(s)], n=3, style='jenks')$brks, silent=TRUE)
        if (inherits(br,"try-error") || length(br)<4 || !all(is.finite(br))) { q <- stats::quantile(s, probs=c(1/3,2/3), na.rm=TRUE, names=FALSE); return(.safe_cut(s,q[1],q[2], dir=+1)) }
        return(.safe_cut(s, br[2], br[3], dir=+1))
      }
      q <- stats::quantile(s, probs=c(1/3,2/3), na.rm=TRUE, names=FALSE); .safe_cut(s, q[1], q[2], dir=+1)
    }
    
    # Clasificación por índice (y score numérico 0/0.5/1 para promediar)
    cls_idx <- list(); sc_idx <- list()
    if ("clorofila" %in% present) { thr <- if (input$class_mode=="manual") input$thr_clorofila else NULL; cls_idx$clorofila <- class_one(slice[[cols$clorofila]], input$class_mode, dir=+1, thr=thr); sc_idx$clorofila <- c("DEFICIENTE"=0,"REGULAR"=0.5,"BUENO"=1)[as.character(cls_idx$clorofila)] }
    if ("nitrogeno" %in% present) { thr <- if (input$class_mode=="manual") input$thr_nitrogeno else NULL; cls_idx$nitrogeno <- class_one(slice[[cols$nitrogeno]], input$class_mode, dir=+1, thr=thr); sc_idx$nitrogeno <- c("DEFICIENTE"=0,"REGULAR"=0.5,"BUENO"=1)[as.character(cls_idx$nitrogeno)] }
    if ("biomasa"   %in% present) { thr <- if (input$class_mode=="manual") input$thr_biomasa   else NULL; cls_idx$biomasa   <- class_one(slice[[cols$biomasa]],   input$class_mode, dir=+1, thr=thr); sc_idx$biomasa   <- c("DEFICIENTE"=0,"REGULAR"=0.5,"BUENO"=1)[as.character(cls_idx$biomasa)] }
    if ("estres"    %in% present) { thr <- if (input$class_mode=="manual") input$thr_estres    else NULL; dir_st <- if (isTRUE(input$invert_stress)) -1 else +1; cls_idx$estres <- class_one(slice[[cols$estres]], input$class_mode, dir=dir_st, thr=thr); sc_idx$estres <- c("DEFICIENTE"=0,"REGULAR"=0.5,"BUENO"=1)[as.character(cls_idx$estres)] }
    
    idx_mat  <- if (length(sc_idx)) do.call(cbind, sc_idx) else NULL
    idx_mean <- if (!is.null(idx_mat)) rowMeans(idx_mat, na.rm = TRUE) else rep(NA_real_, nrow(slice))
    
    # Tratamiento de Temperatura:
    # - “Temperatura Media”: dentro de [temp_mid_min, temp_mid_max] es óptimo (score=1)
    # - Fuera del rango: penaliza linealmente (score 0–1), con denominador = ancho rango o 6 (suelo)
    temp_score <- NULL; temp_CL <- NULL; temp_band <- NULL; t_val <- NULL
    if (nzchar(cols$temp) && cols$temp %in% names(slice)) {
      t_val <- as.numeric(slice[[cols$temp]])
      tmin <- suppressWarnings(as.numeric(input$temp_mid_min)); tmax <- suppressWarnings(as.numeric(input$temp_mid_max))
      if (!is.finite(tmin) || !is.finite(tmax) || tmax <= tmin) { tmin <- 35; tmax <- 40 }
      dist <- ifelse(t_val < tmin, tmin - t_val, ifelse(t_val > tmax, t_val - tmax, 0))
      denom <- max(tmax - tmin, 6)
      temp_score <- pmax(0, 1 - (dist/denom))
      temp_CL <- factor(ifelse(!is.finite(t_val), NA_character_,
                               ifelse(dist == 0, "BUENO",
                                      ifelse(dist <= 2, "REGULAR", "DEFICIENTE"))),
                        levels = LEVELS_CLASE)
      temp_band <- factor(ifelse(!is.finite(t_val), NA_character_,
                                 ifelse(t_val < tmin, "Temperatura Baja",
                                        ifelse(t_val <= tmax, "Temperatura Media", "Temperatura Alta"))),
                          levels = TEMP_LABELS)
    }
    
    # Score final S: promedio índices (+ mezcla con temperatura según peso)
    S <- idx_mean
    if (!is.null(temp_score) && !all(is.na(temp_score))) {
      w <- input$temp_weight
      if (!is.na(w) && w > 0) {
        if (all(is.na(S))) S <- temp_score else S <- (1 - w) * S + w * temp_score
      }
    }
    
    # Tabla intermedia (por bloque) para unir al sf
    res_slice <- dplyr::tibble(
      .id_norm = slice$.id_norm,
      Score    = S,
      Clase    = class_score(S, input$class_mode)
    )
    for (nm in names(cls_idx)) res_slice[[paste0(nm, "_CL")]] <- cls_idx[[nm]]
    if (!is.null(temp_CL))   res_slice[["temp_CL"]]   <- temp_CL
    if (!is.null(temp_band)) res_slice[["temp_band"]] <- temp_band
    if (!is.null(t_val) && nzchar(cols$temp)) res_slice[[cols$temp]] <- t_val
    
    # Unión sf + clasificación por .id_norm
    out <- dplyr::left_join(v, res_slice, by = ".id_norm")
    if ("temp_band" %in% names(out)) out$temp_band <- factor(out$temp_band, levels = TEMP_LABELS)
    if ("Clase" %in% names(out))     out$Clase     <- factor(out$Clase, levels = LEVELS_CLASE)
    attr(out, "__bbox__") <- tryCatch(expand_bbox(sf::st_bbox(out), pad=0.04), error=function(e) NULL)
    out
  })
  
  # -------------------- MAPA: pintar y leyenda --------------------
  observe({
    dat <- classify_month(); req(dat)
    # Qué columna usar para colorear
    cls_col <- switch(input$view_mode,
                      score = "Clase",
                      clorofila = "clorofila_CL",
                      nitrogeno = "nitrogeno_CL",
                      biomasa   = "biomasa_CL",
                      estres    = "estres_CL",
                      temp      = "temp_band",
                      "Clase")
    
    if (!(cls_col %in% names(dat))) {
      showNotification(sprintf("No hay clasificación para '%s' en el mes seleccionado.", input$view_mode), type="warning", duration=6)
      return(NULL)
    }
    
    # Paleta y título de leyenda según modo
    if (input$view_mode == "temp") {
      pal <- colorFactor(pal_temp, levels = TEMP_LABELS, na.color = "#cccccc")
      ClaseVis <- dat[[cls_col]]
      legend_title <- sprintf("%s — Temperatura", rv$key_month)
    } else {
      pal <- colorFactor(pal_index, levels = LEVELS_CLASE, na.color = "#cccccc")
      ClaseVis <- dat[[cls_col]]
      legend_title <- sprintf("%s — %s", rv$key_month, stringr::str_to_title(ifelse(input$view_mode=="score","Score", input$view_mode)))
    }
    
    # Determina una etiqueta razonable para el popup/label
    lab_col <- {
      prefs <- c(rv$cm$id_vector, paste0(rv$cm$id_vector, c(".x",".y")),"Nombre", rv$cm$id_table, paste0(rv$cm$id_table, c(".x",".y")))
      found <- prefs[prefs %in% names(dat)]
      if (length(found)) found[1] else setdiff(names(dat), attr(dat,"sf_column"))[1]
    }
    lab_vec <- as.character(dat[[lab_col]])
    lbl <- sprintf("<strong>%s</strong><br/>Color por: %s<br/>Clase: %s<br/>Score: %s",
                   htmltools::htmlEscape(lab_vec),
                   htmltools::htmlEscape(ifelse(input$view_mode=="score","Score final", stringr::str_to_title(input$view_mode))),
                   htmltools::htmlEscape(as.character(ClaseVis)),
                   ifelse(is.null(dat$Score),"NA", formatC(dat$Score, digits=3, format="f"))) |> lapply(htmltools::HTML)
    
    # Limpia capa y controles, y pinta según familia geométrica
    proxy <- leafletProxy("map") %>% clearGroup("Clasificación") %>% clearControls()
    fam <- geom_family(dat)
    add_args <- if (fam == "point") {
      list(radius=6, stroke=TRUE, color="#333333", fillOpacity=0.85, label=lab_vec, popup=lbl, fillColor=pal(ClaseVis), group="Clasificación")
    } else if (fam == "line") {
      list(weight=3, color="#333333", opacity=0.9, label=lab_vec, popup=lbl, group="Clasificación")
    } else {
      list(weight=1, color="#333333", fillOpacity=0.85, label=lab_vec, popup=lbl, fillColor=pal(ClaseVis), group="Clasificación")
    }
    proxy <- safe_add(proxy, fam, dat, add_args)
    proxy %>% addLegend(position="bottomright", pal=pal, values=ClaseVis, title=legend_title, opacity=1)
    
    # Re-centrado automático cada vez que cambia el mes o el modo
    bbx <- attr(dat, "__bbox__"); if (!is.null(bbx)) do_zoom(bbx) else if (!is.null(rv$bbx)) do_zoom(rv$bbx)
  })
  
  # --------- MAPA: gráficas de mediana (Índices + Temperatura) ----------
  # Series temporales (medianas por fecha, usando el rango de fechas activo)
  output$plt_idx_median_map <- renderPlotly({
    req(isTRUE(input$show_medians))
    tb <- tb_range(); req(tb)
    idx_cols <- c(clorofila=rv$cm$clorofila, nitrogeno=rv$cm$nitrogeno, biomasa=rv$cm$biomasa, estres=rv$cm$estres)
    idx_cols <- idx_cols[nzchar(idx_cols)]
    validate(need(length(idx_cols) > 0, "No hay columnas de índices configuradas en CONFIG$columns."))
    
    df <- tb %>% dplyr::group_by(Fecha) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(unname(idx_cols)), ~median(., na.rm=TRUE)), .groups="drop") %>%
      tidyr::pivot_longer(cols = dplyr::all_of(unname(idx_cols)), names_to = "indice", values_to = "valor")
    validate(need(nrow(df) > 0, "Sin datos para graficar."))
    validate(need(any(is.finite(df$valor)), "Valores no numéricos en índices."))
    
    p <- plotly::plot_ly()
    for (ind in unique(df$indice)) {
      sub <- df[df$indice==ind, ]
      p <- p %>% add_lines(data=sub, x=~Fecha, y=~valor, name=ind,
                           hovertemplate="%{x}<br>%{y}<extra></extra>")
    }
    p %>% layout(title="Índices — Mediana de Bloques (Rango Seleccionado)",
                 xaxis=list(title="Fecha"), yaxis=list(title="Valor índice"))
  })
  
  output$plt_temp_median_map <- renderPlotly({
    req(isTRUE(input$show_medians))
    tb <- tb_range(); req(tb)
    colT <- rv$cm$temp; validate(need(nzchar(colT), "Configura la columna de Temperatura en CONFIG$columns$temp."))
    validate(need(colT %in% names(tb), sprintf("Falta la columna '%s' en la tabla.", colT)))
    df <- tb %>% dplyr::group_by(Fecha) %>% dplyr::summarise(Temp = median(.data[[colT]], na.rm=TRUE), .groups="drop")
    names(df)[2] <- colT
    validate(need(nrow(df) > 0, "No hay datos para calcular la mediana de temperatura."))
    validate(need(any(is.finite(df[[colT]])), "Temperatura sin valores numéricos válidos."))
    plot_ly(df, x=~Fecha, y=df[[colT]], type="scatter", mode="lines", name="Temp") %>%
      layout(title="Temperatura — Mediana de Bloques (Rango Seleccionado)", xaxis=list(title="Fecha"), yaxis=list(title="°C"))
  })
  
  # -------------------- TABLA --------------------
  # Limpia columnas internas, renombra "temp_band" a “Estado Temperatura” y coloca “Nombre” primero.
  output$tabla <- renderDT({
    dat <- classify_month(); req(dat)
    df <- sf::st_drop_geometry(dat)
    
    id_tab <- rv$cm$id_table
    if (!is.null(id_tab) && nzchar(id_tab)) {
      if (id_tab %in% names(df)) df$Nombre <- df[[id_tab]]
      else if (paste0(id_tab, ".y") %in% names(df)) df$Nombre <- df[[paste0(id_tab, ".y")]]
    }
    
    drop_cols <- c("Name.x","Name.y","name.x","name.y",".id_norm","id_norm","score","Score")
    df <- dplyr::select(df, -dplyr::any_of(drop_cols))
    
    if ("temp_band" %in% names(df)) df <- dplyr::rename(df, `Estado Temperatura` = temp_band)
    if ("Nombre" %in% names(df)) df <- dplyr::relocate(df, Nombre)
    
    DT::datatable(df, options = list(pageLength=10, scrollX=TRUE), rownames=FALSE)
  })
  
  # -------------------- GRÁFICAS (comparación) --------------------
  # Selector de 1–2 bloques (usa los nombres “humanos” de la capa vectorial)
  observe({
    req(rv$v, rv$cm$id_vector)
    lab_vals <- unique(as.character(rv$v[[rv$cm$id_vector]]))
    lab_vals <- lab_vals[!is.na(lab_vals)]
    output$ui_blocks_plot_g <- renderUI(
      selectizeInput("blocks_plot_g","Bloques a Comparar (1–2)",
                     choices = lab_vals, selected = character(0),
                     multiple = TRUE, options = list(maxItems = 2, placeholder = "Seleccione 1 o 2 Bloques"))
    )
  })
  # Selector de índice a graficar (usa nombres reales de la tabla)
  observe({
    idx_map <- c(Clorofila=rv$cm$clorofila, `Nitrógeno`=rv$cm$nitrogeno, Biomasa=rv$cm$biomasa, `Estrés`=rv$cm$estres)
    idx_map <- idx_map[nzchar(idx_map)]
    output$ui_index_pick <- renderUI(selectInput("index_pick","Índice a Graficar", choices = idx_map))
  })
  
  # Convierte los nombres “humanos” elegidos a .id_norm para filtrar la tabla
  selected_id_norms <- reactive({
    req(rv$v, rv$cm$id_vector)
    sels <- input$blocks_plot_g
    if (is.null(sels) || !length(sels)) return(character(0))
    normalize_id(sels)
  })
  
  # Serie temporal del índice elegido para 1–2 bloques seleccionados
  output$plt_idx_comp <- renderPlotly({
    tb <- tb_range(); req(tb)
    idx_col <- input$index_pick; validate(need(nzchar(idx_col), "Seleccione un índice."))
    sels <- selected_id_norms()
    validate(need(length(sels) >= 1, "Seleccione al menos un bloque para comparar."))
    
    df <- tb %>% dplyr::filter(.id_norm %in% sels) %>% dplyr::select(Fecha, .id_norm, dplyr::all_of(idx_col))
    validate(need(nrow(df) > 0, "No hay datos para esos bloques/índice en el rango seleccionado."))
    validate(need(any(is.finite(df[[idx_col]])), "La serie no tiene valores numéricos válidos."))
    
    p <- plotly::plot_ly()
    for (e in unique(df$.id_norm)) {
      sub <- df[df$.id_norm==e, ]
      p <- p %>% add_lines(data=sub, x=~Fecha, y=sub[[idx_col]], name=paste("Bloque", e),
                           hovertemplate="%{x}<br>%{y}<extra></extra>")
    }
    ttl <- ifelse(length(sels)==1, paste0("Índice · Bloque ", sels[1]),
                  paste0("Índice · ", paste(sels, collapse=" vs ")))
    p %>% layout(title=ttl, xaxis=list(title="Fecha"), yaxis=list(title="Valor índice"))
  })
  
  # -------------------- Descargas --------------------
  output$dl_csv <- downloadHandler(
    filename = function() paste0("pina_clasificado_", rv$key_month, ".csv"),
    content = function(file) { dat <- classify_month(); req(dat); readr::write_csv(sf::st_drop_geometry(dat), file) }
  )
  output$dl_gpkg <- downloadHandler(
    filename = function() paste0("pina_clasificado_", rv$key_month, ".gpkg"),
    content = function(file) { dat <- classify_month(); req(dat); sf::st_write(dat, file, delete_dsn = TRUE, quiet = TRUE) }
  )
  
  # -------------------- Botón: Zoom a bloques --------------------
  # Intenta primero con el bbox del sf ya clasificado (que incluye unión),
  # luego con el bbox de la capa cruda, y finalmente con el último bbox guardado.
  observeEvent(input$btn_recenter, {
    dat <- classify_month()
    if (!is.null(dat)) {
      bbx <- attr(dat, "__bbox__"); if (!is.null(bbx)) { do_zoom(bbx); return(invisible()) }
    }
    if (!is.null(rv$v)) {
      bb  <- try(sf::st_bbox(rv$v), silent=TRUE)
      if (!inherits(bb,"try-error")) { do_zoom(expand_bbox(bb, pad=0.04)); return(invisible()) }
    }
    if (!is.null(rv$bbx)) do_zoom(rv$bbx)
  })
}

# Lanza la app
shinyApp(ui, server)




