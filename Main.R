
# -----------------------------------------------------------------------------#
# ---- Creación de gráficos para las salidas del CPT y EREG
# -----------------------------------------------------------------------------#

# -----------------------------------------------------------------------------#
# ---- PASO 1. Crear archivo con PID, para verificar salud del contenedor ----

# Open a new file for writing
fileConn <- file("/tmp/plotter.pid", "w")
# Write some text to the file
writeLines(as.character(Sys.getpid()), fileConn)
# Close the file connection
close(fileConn)

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 2. Inicializar entorno ----

# i. Cambiar carpeta de trabajo actual a la de este script
getScriptPath <- function(){
  cmd.args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
  script.dir <- dirname(regmatches(cmd.args, m))
  if(length(script.dir) == 0)
    stop("Can't determine script dir: please call the script with Rscript")
  if(length(script.dir) > 1)
    stop("Can't determine script dir: more than one '--file' argument detected")
  return(script.dir)
}
setwd( getScriptPath() )


# ii. Borrar objetos existentes en el ambiente
rm(list = ls()); gc()

# iii. Configurar huso horario en UTC
Sys.setenv(TZ = "UTC")

# iv. Cargar paquetes a utilizar
list.of.packages <- c("dplyr")
for (pack in list.of.packages) {
  if (! require(pack, character.only = TRUE)) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

# v. Verificar si están instalados los paquetes necesarios
list.of.packages <- c("sf", "sp", "stringr", "logger", "tibble", "tools",
                      "RColorBrewer", "grDevices", "rlang", "purrr", "rlang", 
                      "viridis", "glue", "here", "lubridate", "tidync",
                      "optparse")
for (pack in list.of.packages) {
  if(pack %in% rownames(installed.packages()) == FALSE) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

rm(pack, list.of.packages); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 3. Cargar librerias propias e iniciar script ----

# Cargar librerias
source(here::here("lib", "Configuracion.R"), echo = FALSE, chdir = TRUE)

# Cargar clases
source(here::here("Helpers.R"), echo = FALSE, chdir = TRUE)
source(here::here("DatosEntrada.R"), echo = FALSE, chdir = TRUE)

# Cargar expresiones regulares (ya definidas de acuerdo a la configuración)
source(here::here("ExpresionesRegulares.R"), echo = FALSE, chdir = TRUE)

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 4. Leer archivo de configuracion y definir parámetros globales -----

# Parsear argumentos
option_list = list(
  optparse::make_option(
    c("--year"), type="integer", default=lubridate::year(lubridate::now()), 
    help="Year that should be considered as initial condition (Default: %default)."),
  optparse::make_option(
    c("--month"), type="integer", default=lubridate::month(lubridate::now()), 
    help="Month that should be considered as initial condition (Default: %default).")
)
opt = optparse::parse_args(
  optparse::OptionParser(option_list=option_list))

# Leer configuración
global_config <- Config$new(here::here('config.yaml'))

# Obtener condiciones inciales
global_ic <- global_config$get_config('initial_conditions')
if ( is.null(global_ic) ) {
  global_ic <- list()
  global_ic$month <- opt$month
  global_ic$year <- opt$year
}

# Obtener configuración para pronos CPT
config_cpt <- global_config$get_config('acc-cpt')

# Obtener configuración para pronos EREG
config_ereg <- global_config$get_config('ereg')

# Obtener el dominio espacial de los gráficos
spatial_domain <- global_config$get_config("spatial_domain")

# Obtener los idiomas para los gráficos
output_langs <- global_config$get_config("output_langs")

# Obtener los gráficos a producir
output_plots <- global_config$get_config("output_plots")

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 5. Identificar archivos a graficar -----


# Definir estructura
base_files_struct <- tibble::tibble(
  type = character(),
  variable = character(),
  basename = character(),
  det_fcst_file = character(),
  det_hcst_file = character(),
  prob_fcst_file = character(),
  prob_hcst_file = character(),
  prob_xtrm_file = character(),
  obs_file = character(),
  uncalibrated_fcst_file = character(),
  hcst_first_year = numeric(),
  hcst_last_year = numeric(),
  target_months = character(),
  obs_data_source = character()
)

# Identificar archivos ACC-CPT
cpt_base_files <- list.files(
  path = config_cpt$input_folders$calibrated_data$forecasts, 
  pattern = cpt_files_regex,
  full.names = FALSE) %>% tools::file_path_sans_ext()
if ( length(cpt_base_files) == 0 ) {
  cpt_base_files <- base_files_struct
} else {
  cpt_base_files <- cpt_base_files %>% 
    tibble::as_tibble_col("basename") %>%
    dplyr::mutate(
      type = 'acc-cpt',
      variable = stringr::str_extract(basename, cpt_regex_variables),
      basename = stringr::str_replace(basename, '_forecast$', '')
    ) %>%
    dplyr:: mutate(
      det_fcst_file = paste0(basename, '_forecast.nc'),
      det_hcst_file = paste0(basename, '_hindcast.nc'),
      prob_fcst_file = paste0(basename, '_prob_forecast.nc'),
      prob_hcst_file = paste0(basename, '_prob_hindcast.nc'),
      prob_xtrm_file = ''
    ) %>%
    dplyr::mutate(
      obs_file = paste0(
        stringr::str_extract(basename, cpt_regex_variables), '_', 
        stringr::str_extract(basename, cpt_regex_fuente_datos), '_',
        stringr::str_extract(basename, paste0('_', cpt_regex_months, '_')) %>%  
          stringr::str_replace_all('_', ''), '.nc')
    ) %>% 
    dplyr::mutate(
      uncalibrated_fcst_file = paste0(
        stringr::str_extract(basename, cpt_regex_modelos), '_',
        stringr::str_extract(basename, cpt_regex_pre_variables), '_',
        stringr::str_extract(basename, cpt_regex_init_month), '_',
        stringr::str_extract(basename, paste0('_', cpt_regex_months, '_')) %>%  
          stringr::str_replace_all('_', ''), '_',
        stringr::str_extract(basename, cpt_regex_hcst_years), '_',
        stringr::str_extract(basename, paste0('_', cpt_regex_fcst_years, '_')) %>%  
          stringr::str_replace_all('_', ''), '_1.nc')
    ) %>% 
    dplyr::rowwise() %>% dplyr::mutate(
      hcst_first_year = stringr::str_split(
        stringr::str_extract(basename, cpt_regex_hcst_years), '-') %>% 
        unlist() %>% dplyr::first() %>% as.numeric(),
      hcst_last_year = stringr::str_split(
        stringr::str_extract(basename, cpt_regex_hcst_years), '-') %>% 
        unlist() %>% dplyr::last() %>% as.numeric(),
      target_months = ifelse(
        stringr::str_extract(basename, paste0('_', cpt_regex_months, '_')) %>%  
          stringr::str_replace_all('_', '') %>% stringr::str_detect('-'),
        yes = paste(crange(global_ic$month+1, global_ic$month+3, 12), collapse='-'),
        no = stringr::str_extract(basename, paste0('_', cpt_regex_months, '_')) %>%  
          stringr::str_replace_all('_', '')) %>% as.character()
    ) %>% dplyr::ungroup() %>%
    dplyr::mutate(
      obs_data_source = stringr::str_extract(basename, cpt_regex_fuente_datos)
    ) %>%
    dplyr::select(base_files_struct %>% names())
}

# Identificar Archivos EREG-Climax
ereg_base_files <- list.files(
  path = config_ereg$input_folders$calibrated_data$forecasts, 
  pattern = ereg_files_regex,
  full.names = FALSE) %>% tools::file_path_sans_ext()
if ( length(ereg_base_files) == 0 ) {
  ereg_base_files <- base_files_struct
} else {
  ereg_base_files <- ereg_base_files %>% 
    tibble::as_tibble_col("basename") %>%
    dplyr::mutate(
      type = 'ereg',
      variable = dplyr::case_when(
        stringr::str_extract(basename, ereg_regex_variables) == 'prec' ~ 'prcp',
        stringr::str_extract(basename, ereg_regex_variables) == 'tref' ~ 't2m',
        TRUE ~ NA_character_)
    ) %>%
    dplyr:: mutate(
      det_fcst_file = paste0('determin_', basename, '.nc'),
      det_hcst_file = paste0('determin_', stringr::str_remove(basename, ereg_regex_init_year_str), '_hind.nc'),
      prob_fcst_file = paste0(basename, '.nc'),
      prob_hcst_file = paste0(stringr::str_remove(basename, ereg_regex_init_year_str), '_hind.nc'),
      prob_xtrm_file = stringr::str_replace(paste0(basename, '.nc'),
                                            pattern = paste0('^(', ereg_regex_variables, ')', 
                                                             '_(', ereg_regex_modelos, ')_'),
                                            replacement = '\\1_extremes_\\2_')
    ) %>%
    dplyr::mutate(
      trimester = stringr::str_extract(basename, ereg_regex_months)) %>%
    dplyr::rowwise(.) %>% dplyr::mutate(
      obs_file = paste0(
        'obs_',
        stringr::str_extract(basename, ereg_regex_variables), '_', 
        ifelse(
          all(global_ic$month < MonthsHelper$trimester_to_seq(trimester)),
          yes=1982, no=1983), '_',
        stringr::str_extract(basename, ereg_regex_months), 
        '.nc'),
      uncalibrated_fcst_file = NA_character_,
      hcst_first_year = ifelse(
        all(global_ic$month < MonthsHelper$trimester_to_seq(trimester)),
        yes=1982, no=1983),
      hcst_last_year = ifelse(
        all(global_ic$month < MonthsHelper$trimester_to_seq(trimester)),
        yes=2010, no=2011),
      target_months = paste(
        MonthsHelper$trimester_to_seq(trimester), collapse='-') 
    ) %>% dplyr::select(-trimester) %>% dplyr::ungroup() %>% 
    dplyr::mutate(
      obs_data_source = ifelse(variable == "prcp", "cpc-cmap-urd", "ghcn_cams") %>% 
        as.character()) %>%
    dplyr::select(base_files_struct %>% names())
}


# Unir archivos identificados en un único dataframe
all_base_files <- dplyr::bind_rows(list(cpt_base_files, ereg_base_files))

# Definir el df final, que contendrá los archivos a ser graficados
base_files <- base_files_struct

# Filtrar archivos por meses
if ( 'months' %in% global_config$get_config('output_trgt_type') ) {
  # Filtrar archivos por meses
  m_base_files <- purrr::map_dfr(
    .x = global_config$get_config('output_leadtimes'),
    .f = function(lt, c_base_files) {
      lt_month <- global_ic$month + lt
      if ( lt_month > 12 )
        lt_month <- lt_month %% 12
      # Solo se grafican los meses indicados según el leadtime
      c_base_files %>% dplyr::filter(
        target_months == lt_month)
    }, c_base_files = all_base_files)
  # Unir archivos filtradps en un único dataframe
  base_files <- dplyr::bind_rows(list(base_files, m_base_files))
  # Borrar objetos que ya no se va a utilizar
  rm(m_base_files); invisible(gc())
}

# Filtrar archivos por trimestres
if ( 'trimesters' %in% global_config$get_config('output_trgt_type') ) {
  # Filtrar archivos por trimestres
  t_base_files <- purrr::map_dfr(
    .x = global_config$get_config('output_leadtimes'),
    .f = function(lt, c_base_files) {
      lt_month <- global_ic$month + lt
      if ( lt_month > 12 )
        lt_month <- lt_month %% 12
      # Solo se grafican los trimestres indicados según el leadtime
      c_base_files %>% dplyr::filter(
        target_months == paste(crange(lt_month, lt_month+2, 12), collapse='-'))
    }, c_base_files = all_base_files)
  # Unir archivos filtradps en un único dataframe
  base_files <- dplyr::bind_rows(list(base_files, t_base_files))
  # Borrar objetos que ya no se va a utilizar
  rm(t_base_files); invisible(gc())
}

# Borrar objetos que ya no se va a utilizar
rm(all_base_files, cpt_base_files, ereg_base_files); invisible(gc())

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 6. Graficar -----

# Definir índices a iterar
indices <- c()
if ( nrow(base_files) > 0 ) {
  indices <- seq(nrow(base_files))
} else {
  logger::log_info('No se encontraron archivos a procesar!')
}

# Iterar archivos por índices
for ( i in indices ) {
  
  # Extraer datos sobre el/los archivo/s a graficar
  base_file <- base_files[i,]
  
  # Informar cual archivo se está procesando
  logger::log_info(glue::glue('Procesando archivo: {base_file$basename}'))
  
  # Obtner datos de entrada
  logger::log_info('Inicia lectura de archivos de entrada')
  datos_entrada <- DatosEntrada$new(base_file, global_config$config_file)
  logger::log_info('Finaliza lectura de archivos de entrada')
  
  
  #
  # Definir la grilla a ser utilizada para lel suaviado de los gráficos
  #
  
  
  # Crear grilla nueva. Los puntos de la grilla nueva no pueden coincidir
  # con los puntos de la grilla original, porque si coinciden la 
  # interpolación no produce ningún cambio en los gráficos.
  if ( global_config$get_config("unify_grid") ) {
    
    grid_resolution <- global_config$get_config("unify_grid_resolution")
    
    new_grid_sf <- tibble::as_tibble(
      expand.grid(longitude = seq(from = spatial_domain$wlo, 
                                  to = spatial_domain$elo, 
                                  by = grid_resolution), 
                  latitude = seq(from = spatial_domain$sla, 
                                 to = spatial_domain$nla, 
                                 by = grid_resolution))) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), 
                   remove = FALSE, crs = 4326)
  } else {
    
    data_df <- datos_entrada$pred_det_fcst_data$data
    delta <- min(spatial_dist(data_df$longitude)) / 2
    
    new_grid_sf <- tibble::as_tibble(
      expand.grid(longitude = seq(from = min(data_df$longitude) - delta, 
                                  to = max(data_df$longitude) + delta, 
                                  by = delta*2), 
                  latitude = seq(from = max(data_df$latitude) + delta, 
                                 to = spatial_domain$sla, 
                                 by = -delta*2))) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), 
                   remove = FALSE, crs = 4326)
  }
  
  
  #
  # Preparar máscara seca para ser aplicada antes de graficar
  #
  
  
  # Se setea esta variable a NULL porque se pasa siempre
  # a la función que grafica, pero, la idea que cuando
  # sea NULL, no se aplique ninguna máscara.
  dry_mask_trgt_months <- NULL
  
  
  # Definir máscara cuando corresponda
  if ( base_file$type == 'ereg' &&  base_file$variable == 'prcp' && config_ereg$apply_dry_mask ) {
    
    logger::log_info('Inicia obtención de la máscara a ser aplicada')
    
    # Leer archivo con mascara seca para EREG
    dry_mask_trgt_months <- DryMaskHelper$read_data(
      dry_mask_file = config_ereg$dry_mask_file, 
      trgt_months = base_file$target_months %>% 
        stringr::str_split('-') %>% 
        purrr::reduce(c))
    
    # Si la grilla de la máscara no coincide con la grilla de los datos
    # pronosticados, entonces se debe interpolar la máscara.
    if ( !are_points_joinables(new_grid_sf, dry_mask_trgt_months) ) {
      logger::log_info('Inicia interpolación de la máscara')
      
      # Interpolar máscara
      dry_mask_trgt_months <- DryMaskHelper$interpolate(
        dry_mask_df = dry_mask_trgt_months,
        dest_points_df = datos_entrada$pred_det_fcst_data$data,
        spatial_domain = spatial_domain
      ) %>% exclude_points_outside_buffered_crcsas()
      
    }  # fin de la interpolación
    
  }  # fin de la creación de la máscara
  
  
  # 
  # Crear gráfico de correlación
  #
  
  if ( "corr" %in% output_plots ) {
  
  logger::log_info('Inicia graficado de Correlación Histórica')
  
  # Definir df con los datos a graficar
  corr_df <- 
    CorrelationHelper$compute_correlation(
      datos_entrada$pred_det_hcst_data$data, 
      datos_entrada$obs_data$data, 
      base_file$variable,
      lower_training_year = base_file$hcst_first_year, 
      higher_training_year = base_file$hcst_last_year) %>% 
    dplyr::select(
      longitude, latitude, value = corr) %>%
    exclude_points_outside_buffered_crcsas() %>%
    { if ( global_config$get_config(base_file$type)$suavizar_graficos )
        InterpolationHelper$interp_kriging(
          data_df = ., cols_to_interp = "value", new_grid_sf = new_grid_sf)
      else . } %>%
    exclude_points_outside_crcsas() 
  
  # La interpolación puede hacer que haya puntos por sobre la línea de
  # los -10 grados de latitude, estas deben ser exluídas del gráfico.
  corr_df <- corr_df %>%
    dplyr::filter(latitude <= global_config$get_config('spatial_domain')$nla)
  
  # Los datos CHIRPS, por debajo de -49 no deben ser utilizados
  corr_df <- corr_df %>%
    { if ( base_file$obs_data_source == "chirps" )
        dplyr::mutate(.,
          value = ifelse(latitude < -46, NA_integer_, value))
      else . }
  
  # Aplicar unname a todas las columnas porque las columnas de 
  # tipo named causan errores el graficar.
  corr_df <- corr_df %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ unname(.x)))
  
  # Definir paleta de colores
  red_plt  <- head(rev(RColorBrewer::brewer.pal(3, 'Reds')), 2)
  blue_plt  <- tail(grDevices::colorRampPalette(
    colors = RColorBrewer::brewer.pal(9, 'Blues'))( 11 ), 9)
  paleta_completa <- c(red_plt, "#f1f1f1", blue_plt)
  
  # Crear gráfico
  for ( lang in output_langs ) {
    corr_plot <- PlotsHelper$graficar_mapa(
      data_df = corr_df, 
      gridded_data = are_points_gridded(datos_entrada$pred_det_hcst_data$data),
      main_title = PlotsHelper$definir_titulo("corr", base_file, lang), 
      legend_title = PlotsHelper$definir_titulo_leyenda("corr", base_file, lang), 
      data_type = base_file$type, lang = lang, 
      spatial_domain = list(
        nla = max(corr_df$latitude),
        sla = min(corr_df$latitude),
        wlo = min(corr_df$longitude),
        elo = max(corr_df$longitude)), 
      output_file_abspath = paste0(
        global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
        base_file$basename, "_corr_", lang,".html"),
      breaks = c(-0.5,-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
      colors = paleta_completa, 
      dry_mask_df = dry_mask_trgt_months,
      save_map = TRUE)
  }
  
  }  # fin del if que genera gráficos de correlación
  
  
  #
  # Graficar todos los años en el dataframe de forecasts
  #
  
  anhos_pronosticados <- unique(datos_entrada$pred_det_fcst_data$data$year)
  logger::log_info(glue::glue("Se van a generar gráficos para los siguientes años: {anhos_pronosticados}"))
  
  for (data_year in anhos_pronosticados) {
    
    # Determinar si los datos forman una grilla regular. Esto se debe
    # hacer antes de exluir puntos fuera del crcsas, en realidad, antes
    # de excluir cualquier punto en los datos.
    det_gridded_data <- are_points_gridded(
      points_df = datos_entrada$pred_det_fcst_data$data %>%
        dplyr::filter(year == data_year) %>%
        dplyr::select(longitude, latitude))
    prob_gridded_data <- are_points_gridded(
      points_df = datos_entrada$pred_prob_fcst_data$data %>%
        dplyr::filter(year == data_year) %>%
        dplyr::select(longitude, latitude))
    xtrm_gridded_data <- are_points_gridded(
      points_df = datos_entrada$pred_prob_xtrm_data$data %>%
        dplyr::filter(year == data_year) %>%
        dplyr::select(longitude, latitude))
    
    #
    # Crear gráfico de anomalías
    #
    
    if ( "anom" %in% output_plots ) {
    
    logger::log_info(glue::glue("Inicia el graficado de las Anomalías para el año: {data_year}"))
    
    # Definir df con los datos a graficar
    anom_df <- datos_entrada$pred_det_fcst_data$data %>%
      dplyr::filter(
        year == data_year) %>%
      dplyr::select(
        longitude, latitude, value = anomaly) %>%
      exclude_points_outside_buffered_crcsas() %>%
      { if ( global_config$get_config(base_file$type)$suavizar_graficos )
          InterpolationHelper$interp_kriging(
            data_df = ., cols_to_interp = "value", new_grid_sf = new_grid_sf)
        else . } %>%
      exclude_points_outside_crcsas() 
    
    # La interpolación puede hacer que haya puntos por sobre la línea de
    # los -10 grados de latitude, estas deben ser exluídas del gráfico.
    anom_df <- anom_df %>%
      dplyr::filter(latitude <= global_config$get_config('spatial_domain')$nla)
    
    # Los datos CHIRPS, por debajo de -49 no deben ser utilizados
    anom_df <- anom_df %>%
      { if ( base_file$obs_data_source == "chirps" )
          dplyr::mutate(.,
            value = ifelse(latitude < -46, NA_integer_, value))
        else . }
    
    # Aplicar unname a todas las columnas porque las columnas de 
    # tipo named causan errores el graficar.
    anom_df <- anom_df %>% 
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ unname(.x)))
    
    # Definir paleta de colores
    if (base_file$variable == 'prcp') {
      breaks <- c(-100,-50,-20,-10,-5,5,10,20,50,100)
      colores <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(9, 'BrBG'))(11)
      brown_plt <- head(colores, 5)
      green_plt <- tail(colores, 5)
      paleta_completa <- c(brown_plt, "#e5e8e8", green_plt)
    } else if (base_file$variable == 't2m') {
      breaks <- c(-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2)
      blue_plt <- head(rev(RColorBrewer::brewer.pal(7, 'Blues')), 5)
      red_plt <- tail(RColorBrewer::brewer.pal(7, 'Reds'), 5)
      paleta_completa <- c(blue_plt, "#e5e8e8", red_plt)
    }
    
    # Crear gráfico
    for ( lang in output_langs ) {
      anom_plot <- PlotsHelper$graficar_mapa(
        data_df = anom_df, 
        gridded_data = det_gridded_data,
        main_title = PlotsHelper$definir_titulo("anom", base_file, lang, data_year), 
        legend_title = PlotsHelper$definir_titulo_leyenda("anom", base_file, lang), 
        data_type = base_file$type, lang = lang,
        spatial_domain = list(
          nla = max(anom_df$latitude),
          sla = min(anom_df$latitude),
          wlo = min(anom_df$longitude),
          elo = max(anom_df$longitude)), 
        output_file_abspath = paste0(
          global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
          base_file$basename, "_anom_", lang, ".html"),
        breaks = breaks,
        colors = paleta_completa, 
        dry_mask_df = dry_mask_trgt_months,
        save_map = TRUE)
    }
    
    }  # fin del if que genera gráficos de anomalías
    
    
    #
    # Crear gráfico de valores determinísticos pronosticados
    #
    
    if ( "det.fcst" %in% output_plots ) {
    
    logger::log_info(glue::glue("Inicia el graficado del Pronóstico Determinístico para el año: {data_year}"))
    
    # Definir df con los datos a graficar
    det_fcst_df <- datos_entrada$pred_det_fcst_data$data %>%
      dplyr::filter(
        year == data_year) %>%
      dplyr::select(
        longitude, latitude, value = !!rlang::sym(base_file$variable)) %>%
      exclude_points_outside_buffered_crcsas() %>%
      { if ( global_config$get_config(base_file$type)$suavizar_graficos )
        InterpolationHelper$interp_kriging(
          data_df = ., cols_to_interp = "value", new_grid_sf = new_grid_sf)
        else . } %>%
      exclude_points_outside_crcsas() 
    
    # La interpolación puede hacer que haya puntos por sobre la línea de
    # los -10 grados de latitude, estas deben ser exluídas del gráfico.
    det_fcst_df <- det_fcst_df %>%
      dplyr::filter(latitude <= global_config$get_config('spatial_domain')$nla)
    
    # Los datos CHIRPS, por debajo de -49 no deben ser utilizados
    det_fcst_df <- det_fcst_df %>%
      { if ( base_file$obs_data_source == "chirps" )
        dplyr::mutate(.,
          value = ifelse(latitude < -46, NA_integer_, value))
        else . }
    
    # Aplicar unname a todas las columnas porque las columnas de 
    # tipo named causan errores el graficar.
    det_fcst_df <- det_fcst_df %>% 
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ unname(.x)))
    
    # Obtener la cantidad de meses objetivo (3 para seasonal y 1 para monthly)
    n_trgt_months <- stringr::str_split(base_file$target_months, '-') %>% 
      purrr::reduce(c) %>% as.numeric() %>% length()
    
    # Definir paleta de colores
    if (base_file$variable == 'prcp') {
      breaks <- c(1,25,50,100,150,200,250,300,400,500,600)
      if ( n_trgt_months > 1 )
        breaks <- c(1,25,50,100,150,200,300,500,750,1000,1500)
      paleta_completa <- grDevices::colorRampPalette(
        colors = RColorBrewer::brewer.pal(9, 'Blues'))( 12 )
    } else if (base_file$variable == 't2m') {
      breaks <- c(-9,-6,-3,3,6,9,12,15,18,21,24,27,30,33)
      paleta_completa <- viridis::turbo(15)
    }
    
    # Crear gráfico
    for ( lang in output_langs ) {
      det_fcst_plot <- PlotsHelper$graficar_mapa(
        data_df = det_fcst_df, 
        gridded_data = det_gridded_data,
        main_title = PlotsHelper$definir_titulo("det.fcst", base_file, lang, data_year), 
        legend_title = PlotsHelper$definir_titulo_leyenda("det.fcst", base_file, lang), 
        data_type = base_file$type, lang = lang,
        spatial_domain = list(
          nla = max(det_fcst_df$latitude),
          sla = min(det_fcst_df$latitude),
          wlo = min(det_fcst_df$longitude),
          elo = max(det_fcst_df$longitude)), 
        output_file_abspath = paste0(
          global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
          base_file$basename, "_det_fcst_", lang, ".html"),
        breaks = breaks,
        colors = paleta_completa, 
        dry_mask_df = dry_mask_trgt_months,
        save_map = TRUE)
    }
    
    }  # fin del if que genera gráficos determinísticos
  
  
    #
    # Crear gráfico de valores probabilísticos pronosticados
    #
    
    if ( "prob.fcst" %in% output_plots || 
         "b33.fcst" %in% output_plots || "a66.fcst" %in% output_plots) {
    
    logger::log_info(glue::glue("Inicia el graficado del Pronóstico Probabilístico para el año: {data_year}"))
    
    # Definir df con los datos a graficar
    prob_fcst_df <- datos_entrada$pred_prob_fcst_data$data %>%
      dplyr::filter(
        year == data_year) %>%
      dplyr::rename(
        prob_below = paste0('prob_', base_file$variable, '_below'),
        prob_normal = paste0('prob_', base_file$variable, '_normal'),
        prob_above = paste0('prob_', base_file$variable, '_above')) %>%
      dplyr::select(
        -init_time, -year, -month) %>%
      exclude_points_outside_buffered_crcsas() %>%
      { if ( global_config$get_config(base_file$type)$suavizar_graficos )
          InterpolationHelper$interp_kriging(
            data_df = ., 
            cols_to_interp = c("prob_below", "prob_normal", "prob_above"), 
            new_grid_sf = new_grid_sf)
        else . } %>%
      exclude_points_outside_crcsas() %>%
      FcstProbabilisticData$add_categories(
        prob_data_df = ., 
        below_col = "prob_below", 
        normal_col = "prob_normal", 
        above_col = "prob_above") 
    
    # La interpolación puede hacer que haya puntos por sobre la línea de
    # los -10 grados de latitude, estas deben ser exluídas del gráfico.
    prob_fcst_df <- prob_fcst_df %>%
      dplyr::filter(latitude <= global_config$get_config('spatial_domain')$nla)
    
    # Los datos CHIRPS, por debajo de -49 no deben ser utilizados
    prob_fcst_df <- prob_fcst_df %>%
      { if ( base_file$obs_data_source == "chirps" )
          dplyr::mutate(.,
            prob_below = ifelse(latitude < -46, NA_integer_, prob_below),
            prob_normal = ifelse(latitude < -46, NA_integer_, prob_normal),
            prob_above = ifelse(latitude < -46, NA_integer_, prob_above),
            category = ifelse(latitude < -46, NA_integer_, as.character(category)))
        else . }
    
    # Aplicar unname a todas las columnas porque las columnas de 
    # tipo named causan errores el graficar.
    prob_fcst_df <- prob_fcst_df %>% 
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ unname(.x)))
    
    # Definir paleta de colores de la NOAA
    # Ver: https://www.weather.gov/news/211409-temperature-precipitation-maps
    # Ver: https://www.cpc.ncep.noaa.gov/products/predictions/multi_season/13_seasonal_outlooks/color/churchill.php
    noaa_escala_azules <- tail(RColorBrewer::brewer.pal(9, "PuBu"), 7)
    noaa_escala_rojos <- tail(RColorBrewer::brewer.pal(9, "YlOrRd"), 7)
    noaa_escala_verdes <- tail(RColorBrewer::brewer.pal(9, "BuGn"), 7)
    noaa_escala_marrones <- tail(RColorBrewer::brewer.pal(9, "YlOrBr"), 7)
    
    # Definir paleta de colores para "prob.fcst"
    breaks_prob_fcst <- c(0.33, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1)
    if (base_file$variable == "prcp") {
      paleta_below <- noaa_escala_marrones
      paleta_above <- noaa_escala_verdes
    } else {
      paleta_below <- noaa_escala_azules
      paleta_above <-noaa_escala_rojos
    }
    paleta_normal <- tail(RColorBrewer::brewer.pal(9, 'Greys'), 7)
    
    # Crear gráfico
    if ( "prob.fcst" %in% output_plots ) {
      for ( lang in output_langs ) {
        prob_fcst_plot <- PlotsHelper$graficar_mapa_prob(
          data_df = prob_fcst_df, 
          gridded_data = prob_gridded_data,
          main_title = PlotsHelper$definir_titulo("prob.fcst", base_file, lang, data_year), 
          legend_title = PlotsHelper$definir_titulo_leyenda("prob.fcst", base_file, lang), 
          data_type = base_file$type, lang = lang,
          spatial_domain = list(
            nla = max(prob_fcst_df$latitude),
            sla = min(prob_fcst_df$latitude),
            wlo = min(prob_fcst_df$longitude),
            elo = max(prob_fcst_df$longitude)), 
          output_file_abspath = paste0(
            global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
            base_file$basename, "_prob_fcst_", lang, ".html"),
          breaks = breaks_prob_fcst,
          colors_below = paleta_below, 
          colors_normal = paleta_normal, 
          colors_above = paleta_above, 
          dry_mask_df = dry_mask_trgt_months,
          save_map = TRUE)
      }
    }
    
    # Definir paleta de colores de la NOAA
    # Ver: https://www.weather.gov/news/211409-temperature-precipitation-maps
    # Ver: https://www.cpc.ncep.noaa.gov/products/predictions/multi_season/13_seasonal_outlooks/color/churchill.php
    noaa_escala_azules <- tail(RColorBrewer::brewer.pal(8, "PuBu"), 7)
    noaa_escala_rojos <- tail(RColorBrewer::brewer.pal(8, "YlOrRd"), 7)
    noaa_escala_marrones <- tail(RColorBrewer::brewer.pal(8, "YlOrBr"), 7)
    noaa_escala_verdes <- tail(RColorBrewer::brewer.pal(8, "BuGn"), 7)
    
    # Definir paletas posibles según variable y tipo de probabilidad
    escala_temp_b33 <- c('#FFFFFF', noaa_escala_azules)
    escala_temp_a66 <- c('#FFFFFF', noaa_escala_rojos)
    escala_prcp_b33 <- c('#FFFFFF', noaa_escala_marrones)
    escala_prcp_a66 <- c('#FFFFFF', noaa_escala_verdes)
    
    # Definir paleta de colores para "b33.fcst" y "a66.fcst"
    breaks_prob_xtrm <- c(33, 40, 50, 60, 70, 80, 90, 100)
    paleta_below_33 <- if (base_file$variable == "prcp") escala_prcp_b33 else escala_temp_b33
    paleta_above_66 <- if (base_file$variable == "prcp") escala_prcp_a66 else escala_temp_a66
    
    # Crear gráfico
    if ( "b33.fcst" %in% output_plots ) {
      for ( lang in output_langs ) {
        prob_xtrm_plot_below_33 <- PlotsHelper$graficar_mapa(
          data_df = prob_fcst_df %>% 
            dplyr::select(longitude, latitude, value = prob_below) %>%
            dplyr::mutate(value = value * 100), 
          gridded_data = xtrm_gridded_data,
          main_title = PlotsHelper$definir_titulo("prob.below.33", base_file, lang, data_year), 
          legend_title = PlotsHelper$definir_titulo_leyenda("prob.below.33", base_file, lang), 
          data_type = base_file$type, lang = lang,
          spatial_domain = list(
            nla = max(prob_fcst_df$latitude),
            sla = min(prob_fcst_df$latitude),
            wlo = min(prob_fcst_df$longitude),
            elo = max(prob_fcst_df$longitude)), 
          output_file_abspath = paste0(
            global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
            base_file$basename, "_prob_below_33_", lang, ".html"),
          breaks = breaks_prob_xtrm,
          colors = paleta_below_33, 
          rev_legend = TRUE,
          dry_mask_df = dry_mask_trgt_months,
          include_first = TRUE,
          save_map = TRUE)
      }
    }
    if ( "a66.fcst" %in% output_plots ) {
      for ( lang in output_langs ) {
        prob_xtrm_plot_above_66 <- PlotsHelper$graficar_mapa(
          data_df = prob_fcst_df %>% 
            dplyr::select(longitude, latitude, value = prob_above) %>%
            dplyr::mutate(value = value * 100), 
          gridded_data = xtrm_gridded_data,
          main_title = PlotsHelper$definir_titulo("prob.above.66", base_file, lang, data_year), 
          legend_title = PlotsHelper$definir_titulo_leyenda("prob.above.66", base_file, lang), 
          data_type = base_file$type, lang = lang,
          spatial_domain = list(
            nla = max(prob_fcst_df$latitude),
            sla = min(prob_fcst_df$latitude),
            wlo = min(prob_fcst_df$longitude),
            elo = max(prob_fcst_df$longitude)), 
          output_file_abspath = paste0(
            global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
            base_file$basename, "_prob_above_66_", lang, ".html"),
          breaks = breaks_prob_xtrm,
          colors = paleta_above_66, 
          rev_legend = TRUE,
          dry_mask_df = dry_mask_trgt_months,
          include_first = TRUE,
          save_map = TRUE)
      }
    }
    
    }  # fin del if que genera gráficos probabilísticos
    
    
    #
    # Crear gráfico de valores pronosticados antes de ser calibrados
    #
    
    if ( "uncal.fcst" %in% output_plots && !is.null(datos_entrada$uncalibrated_fcst_data) ) {
      
      logger::log_info(glue::glue("Inicia el graficado del Pronóstico sin Calibrar para el año: {data_year}"))
      
      # Determinar si los datos forman una grilla regular. Esto se debe
      # hacer antes de exluir puntos fuera del crcsas, en realidad, antes
      # de excluir cualquier punto en los datos.
      uncal_gridded_data <- are_points_gridded(
        points_df = datos_entrada$uncalibrated_fcst_data$data %>%
          dplyr::filter(year == data_year) %>%
          dplyr::select(longitude, latitude))
      
      # Definir df con los datos a graficar
      uncal_fcst_df <- datos_entrada$uncalibrated_fcst_data$data %>%
        dplyr::filter(
          year == data_year) %>%
        dplyr::select(
          longitude, latitude, value = !!rlang::sym(base_file$variable)) %>%
        exclude_points_outside_crcsas()
      
      # Obtener la cantidad de meses objetivo (3 para seasonal y 1 para monthly)
      n_trgt_months <- stringr::str_split(base_file$target_months, '-') %>% 
        purrr::reduce(c) %>% as.numeric() %>% length()
      
      # Definir paleta de colores
      if (base_file$variable == 'prcp') {
        breaks <- c(1,25,50,100,150,200,250,300,400,500,600)
        if ( n_trgt_months > 1 )
          breaks <- c(1,25,50,100,150,200,300,500,750,1000,1500)
        paleta_completa <- grDevices::colorRampPalette(
          colors = RColorBrewer::brewer.pal(9, 'Blues'))( 12 )
      } else if (base_file$variable == 't2m') {
        breaks <- c(-9,-6,-3,3,6,9,12,15,18,21,24,27,30,33)
        paleta_completa <- viridis::turbo(15)
      }
      
      # Crear gráfico
      for ( lang in output_langs ) {
        uncal_fcst_plot <- PlotsHelper$graficar_mapa(
          data_df = uncal_fcst_df, 
          gridded_data = uncal_gridded_data,
          main_title = PlotsHelper$definir_titulo("uncal.fcst", base_file, lang, data_year), 
          legend_title = PlotsHelper$definir_titulo_leyenda("uncal.fcst", base_file, lang), 
          data_type = base_file$type, lang = lang,
          spatial_domain = list(
            nla = max(uncal_fcst_df$latitude),
            sla = min(uncal_fcst_df$latitude),
            wlo = min(uncal_fcst_df$longitude),
            elo = max(uncal_fcst_df$longitude)), 
          output_file_abspath = paste0(
            global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
            base_file$basename, "_uncal_fcst_", lang, ".html"),
          breaks = breaks,
          colors = paleta_completa, 
          dry_mask_df = dry_mask_trgt_months,
          save_map = TRUE)
      }
      
    }  # FIN DEL IF: if ( !is.null(datos_entrada$uncalibrated_fcst_data) )

    
    #
    # Crear gráficos de probabilidades de eventos extremos ubicados en el
    # 20% inferior, o en el 20% superior, de la distribución histórica
    #
    
    if ( "b20.fcst" %in% output_plots || "a80.fcst" %in% output_plots ||
         "dry.fcst" %in% output_plots || "wet.fcst" %in% output_plots ||
         "hot.fcst" %in% output_plots || "cold.fcst" %in% output_plots ) {
      
      logger::log_info(glue::glue("Inicia el graficado de Pronósticos Probabilísticos ",
                                  "de Eventos Extremos para el año: {data_year}"))
      
      # Definir df con los datos a graficar
      prob_xtrm_df <- datos_entrada$pred_prob_xtrm_data$data %>%
        dplyr::filter(
          year == data_year) %>%
        dplyr::rename(
          prob_below_20 = paste0('prob_', base_file$variable, '_below'),
          prob_a20_b80 = paste0('prob_', base_file$variable, '_normal'),
          prob_above_80 = paste0('prob_', base_file$variable, '_above')) %>%
        dplyr::select(
          -init_time, -year, -month) %>%
        exclude_points_outside_buffered_crcsas() %>%
        { if ( global_config$get_config(base_file$type)$suavizar_graficos )
          InterpolationHelper$interp_kriging(
            data_df = ., 
            cols_to_interp = c("prob_below_20", "prob_a20_b80", "prob_above_80"), 
            new_grid_sf = new_grid_sf,
            dependent_col = 'prob_a20_b80')
          else . } %>%
        exclude_points_outside_crcsas() %>%
        FcstProbabilisticData$add_categories(
          prob_data_df = ., 
          below_col = "prob_below_20", 
          normal_col = "prob_a20_b80", 
          above_col = "prob_above_80",
          cat_below = "below-20",
          cat_normal = "a20-b80",
          cat_above = "above-80") 
      
      # La interpolación puede hacer que haya puntos por sobre la línea de
      # los -10 grados de latitude, estas deben ser exluídas del gráfico.
      prob_xtrm_df <- prob_xtrm_df %>%
        dplyr::filter(latitude <= global_config$get_config('spatial_domain')$nla)
      
      # Los datos CHIRPS, por debajo de -49 no deben ser utilizados
      prob_xtrm_df <- prob_xtrm_df %>%
        { if ( base_file$obs_data_source == "chirps" )
          dplyr::mutate(.,
                        prob_below = ifelse(latitude < -46, NA_integer_, prob_below),
                        prob_normal = ifelse(latitude < -46, NA_integer_, prob_normal),
                        prob_above = ifelse(latitude < -46, NA_integer_, prob_above),
                        category = ifelse(latitude < -46, NA_integer_, as.character(category)))
          else . }
      
      # Aplicar unname a todas las columnas porque las columnas de 
      # tipo named causan errores el graficar.
      prob_xtrm_df <- prob_xtrm_df %>% 
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ unname(.x)))
      
      # Definir paleta de colores de la NOAA
      # Ver: https://www.weather.gov/news/211409-temperature-precipitation-maps
      # Ver: https://www.cpc.ncep.noaa.gov/products/predictions/multi_season/13_seasonal_outlooks/color/churchill.php
      noaa_escala_azules <- tail(RColorBrewer::brewer.pal(8, "PuBu"), 7)
      noaa_escala_rojos <- tail(RColorBrewer::brewer.pal(8, "YlOrRd"), 7)
      noaa_escala_marrones <- tail(RColorBrewer::brewer.pal(8, "YlOrBr"), 7)
      noaa_escala_verdes <- tail(RColorBrewer::brewer.pal(8, "BuGn"), 7)
      
      # Definir paletas posibles según variable y tipo de probabilidad
      escala_temp_b20 <- c('#FFFFFF', noaa_escala_azules)
      escala_temp_a80 <- c('#FFFFFF', noaa_escala_rojos)
      escala_prcp_b20 <- c('#FFFFFF', noaa_escala_marrones)
      escala_prcp_a80 <- c('#FFFFFF', noaa_escala_verdes)
      
      # Definir paleta de colores
      breaks <- c(30, 40, 50, 60, 70, 80, 90, 100)
      paleta_below_20 <- if (base_file$variable == "prcp") escala_prcp_b20 else escala_temp_b20
      paleta_above_80 <- if (base_file$variable == "prcp") escala_prcp_a80 else escala_temp_a80
      
      # Crear gráficos
      if ( "b20.fcst" %in% output_plots ) {
        for ( lang in output_langs ) {
          prob_xtrm_plot_below_20 <- PlotsHelper$graficar_mapa(
            data_df = prob_xtrm_df %>% 
              dplyr::select(longitude, latitude, value = prob_below_20) %>%
              dplyr::mutate(value = value * 100), 
            gridded_data = xtrm_gridded_data,
            main_title = PlotsHelper$definir_titulo("prob.below.20", base_file, lang, data_year), 
            legend_title = PlotsHelper$definir_titulo_leyenda("prob.below.20", base_file, lang), 
            data_type = base_file$type, lang = lang,
            spatial_domain = list(
              nla = max(prob_xtrm_df$latitude),
              sla = min(prob_xtrm_df$latitude),
              wlo = min(prob_xtrm_df$longitude),
              elo = max(prob_xtrm_df$longitude)), 
            output_file_abspath = paste0(
              global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
              base_file$basename, "_prob_below_20_", lang, ".html"),
            breaks = breaks,
            colors = paleta_below_20, 
            rev_legend = TRUE,
            dry_mask_df = dry_mask_trgt_months,
            include_first = TRUE,
            save_map = TRUE)
        }
      }
      if ( "a80.fcst" %in% output_plots ) {
        for ( lang in output_langs ) {
          prob_xtrm_plot_above_80 <- PlotsHelper$graficar_mapa(
            data_df = prob_xtrm_df %>% 
              dplyr::select(longitude, latitude, value = prob_above_80) %>%
              dplyr::mutate(value = value * 100), 
            gridded_data = xtrm_gridded_data,
            main_title = PlotsHelper$definir_titulo("prob.above.80", base_file, lang, data_year), 
            legend_title = PlotsHelper$definir_titulo_leyenda("prob.above.80", base_file, lang), 
            data_type = base_file$type, lang = lang,
            spatial_domain = list(
              nla = max(prob_xtrm_df$latitude),
              sla = min(prob_xtrm_df$latitude),
              wlo = min(prob_xtrm_df$longitude),
              elo = max(prob_xtrm_df$longitude)), 
            output_file_abspath = paste0(
              global_config$get_config(base_file$type)$output_folder$crcsas, "/", 
              base_file$basename, "_prob_above_80_", lang, ".html"),
            breaks = breaks,
            colors = paleta_above_80, 
            rev_legend = TRUE,
            dry_mask_df = dry_mask_trgt_months,
            include_first = TRUE,
            save_map = TRUE)
        }
      }
      
      # Definir paleta de colores de la NOAA
      # Ver: https://www.weather.gov/news/211409-temperature-precipitation-maps
      # Ver: https://www.cpc.ncep.noaa.gov/products/predictions/multi_season/13_seasonal_outlooks/color/churchill.php
      noaa_escala_azules <- tail(RColorBrewer::brewer.pal(8, "PuBu"), 7)
      noaa_escala_rojos <- tail(RColorBrewer::brewer.pal(8, "YlOrRd"), 7)
      noaa_escala_marrones <- tail(RColorBrewer::brewer.pal(8, "YlOrBr"), 7)
      noaa_escala_verdes <- tail(RColorBrewer::brewer.pal(8, "BuGn"), 7)
      
      # Definir paleta de colores
      breaks <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
      paleta_prcp_dry <- c('#FFFFFF', rep('#FFFFFF', 2), noaa_escala_marrones)
      paleta_prcp_wet <- c('#FFFFFF', rep('#FFFFFF', 2), noaa_escala_verdes)
      paleta_temp_hot <- c('#FFFFFF', rep('#FFFFFF', 2), noaa_escala_rojos)
      paleta_temp_cold <- c('#FFFFFF', rep('#FFFFFF', 2), noaa_escala_azules)
      
      # Crear gráficos
      if ( "dry.fcst" %in% output_plots && base_file$variable == 'prcp' ) {
        for ( lang in output_langs ) {
          prob_xtrm_dry_plot <- PlotsHelper$graficar_mapa(
            data_df = prob_xtrm_df %>% 
              dplyr::select(longitude, latitude, value = prob_below_20) %>%
              dplyr::mutate(value = value * 100), 
            gridded_data = xtrm_gridded_data,
            main_title = PlotsHelper$definir_titulo("prob.xtrm.dry", base_file, lang, data_year), 
            legend_title = PlotsHelper$definir_titulo_leyenda("prob.xtrm.dry", base_file, lang), 
            data_type = base_file$type, lang = lang,
            spatial_domain = list(
              nla = max(prob_xtrm_df$latitude),
              sla = min(prob_xtrm_df$latitude),
              wlo = min(prob_xtrm_df$longitude),
              elo = max(prob_xtrm_df$longitude)), 
            output_file_abspath = paste0(
              global_config$get_config(base_file$type)$output_folder$sissa, "/", 
              base_file$basename, "_prob_xtrm_dry_", lang, ".html"),
            breaks = breaks,
            colors = paleta_prcp_dry, 
            rev_legend = TRUE,
            dry_mask_df = dry_mask_trgt_months,
            include_first = TRUE,
            save_map = TRUE)
        }
      }
      if ( "wet.fcst" %in% output_plots && base_file$variable == 'prcp' ) {
        for ( lang in output_langs ) {
          prob_xtrm_wet_plot <- PlotsHelper$graficar_mapa(
            data_df = prob_xtrm_df %>% 
              dplyr::select(longitude, latitude, value = prob_above_80) %>%
              dplyr::mutate(value = value * 100), 
            gridded_data = xtrm_gridded_data,
            main_title = PlotsHelper$definir_titulo("prob.xtrm.wet", base_file, lang, data_year), 
            legend_title = PlotsHelper$definir_titulo_leyenda("prob.xtrm.wet", base_file, lang), 
            data_type = base_file$type, lang = lang,
            spatial_domain = list(
              nla = max(prob_xtrm_df$latitude),
              sla = min(prob_xtrm_df$latitude),
              wlo = min(prob_xtrm_df$longitude),
              elo = max(prob_xtrm_df$longitude)), 
            output_file_abspath = paste0(
              global_config$get_config(base_file$type)$output_folder$sissa, "/", 
              base_file$basename, "_prob_xtrm_wet_", lang, ".html"),
            breaks = breaks,
            colors = paleta_prcp_wet, 
            rev_legend = TRUE,
            dry_mask_df = dry_mask_trgt_months,
            include_first = TRUE,
            save_map = TRUE)
        }
      }
      if ( "hot.fcst" %in% output_plots && base_file$variable == 't2m' ) {
        for ( lang in output_langs ) {
          prob_xtrm_hot_plot <- PlotsHelper$graficar_mapa(
            data_df = prob_xtrm_df %>% 
              dplyr::select(longitude, latitude, value = prob_above_80) %>%
              dplyr::mutate(value = value * 100), 
            gridded_data = xtrm_gridded_data,
            main_title = PlotsHelper$definir_titulo("prob.xtrm.hot", base_file, lang, data_year), 
            legend_title = PlotsHelper$definir_titulo_leyenda("prob.xtrm.hot", base_file, lang), 
            data_type = base_file$type, lang = lang,
            spatial_domain = list(
              nla = max(prob_xtrm_df$latitude),
              sla = min(prob_xtrm_df$latitude),
              wlo = min(prob_xtrm_df$longitude),
              elo = max(prob_xtrm_df$longitude)), 
            output_file_abspath = paste0(
              global_config$get_config(base_file$type)$output_folder$sissa, "/", 
              base_file$basename, "_prob_xtrm_hot_", lang, ".html"),
            breaks = breaks,
            colors = paleta_temp_hot, 
            rev_legend = TRUE,
            dry_mask_df = dry_mask_trgt_months,
            include_first = TRUE,
            save_map = TRUE)
        }
      }
      if ( "cold.fcst" %in% output_plots && base_file$variable == 't2m' ) {
        for ( lang in output_langs ) {
          prob_xtrm_cold_plot <- PlotsHelper$graficar_mapa(
            data_df = prob_xtrm_df %>% 
              dplyr::select(longitude, latitude, value = prob_below_20) %>%
              dplyr::mutate(value = value * 100), 
            gridded_data = xtrm_gridded_data,
            main_title = PlotsHelper$definir_titulo("prob.xtrm.cold", base_file, lang, data_year), 
            legend_title = PlotsHelper$definir_titulo_leyenda("prob.xtrm.cold", base_file, lang), 
            data_type = base_file$type, lang = lang,
            spatial_domain = list(
              nla = max(prob_xtrm_df$latitude),
              sla = min(prob_xtrm_df$latitude),
              wlo = min(prob_xtrm_df$longitude),
              elo = max(prob_xtrm_df$longitude)), 
            output_file_abspath = paste0(
              global_config$get_config(base_file$type)$output_folder$sissa, "/", 
              base_file$basename, "_prob_xtrm_cold_", lang, ".html"),
            breaks = breaks,
            colors = paleta_temp_cold, 
            rev_legend = TRUE,
            dry_mask_df = dry_mask_trgt_months,
            include_first = TRUE,
            save_map = TRUE)
        }
      }
      
    }  # fin del if que  genera gráficos de probabilidades de eventos extremos
    
    
  }  # FIN DEL FOR: for (data_year in anhos_pronosticados)

}  # FIN DEL FOR: for ( i in 1:nrow(base_files) )  

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 7. Borrar archivo con PID, para verificar salud del contenedor ----

# Remove the file
file.remove("/tmp/plotter.pid")

# ------------------------------------------------------------------------------
