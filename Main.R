
# -----------------------------------------------------------------------------#
# ---- Creación de gráficos para las salidas del CPT y EREG
# -----------------------------------------------------------------------------#

# -----------------------------------------------------------------------------#
# ---- PASO 1. Inicializar entorno ----

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
#setwd( getScriptPath() )


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
                      "RColorBrewer", "grDevices", "rlang", "purrr", 
                      "viridis", "glue", "here")
for (pack in list.of.packages) {
  if(pack %in% rownames(installed.packages()) == FALSE) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

rm(pack, list.of.packages); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 2. Cargar librerias propias e iniciar script ----

# Cargar librerias
source(here::here("lib", "Configuracion.R"), echo = FALSE, chdir = TRUE)

# Cargar clases
source(here::here("Helpers.R"), echo = FALSE, chdir = TRUE)
source(here::here("DatosEntrada.R"), echo = FALSE, chdir = TRUE)

# Cargar expresiones regulares (ya definidas de acuerdo a la configuración)
source(here::here("ExpresionesRegulares.R"), echo = FALSE, chdir = TRUE)

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 3. Leer archivo de configuracion y definir parámetros globales -----

# Leer configuración
global_config <- Config$new(here::here('config.yaml'))

# Obtener condiciones inciales
global_ic <- global_config$get_config('initial_conditions')

# Obtener configuración para pronos CPT
config_cpt <- global_config$get_config('acc-cpt')

# Obtener configuración para pronos EREG
config_ereg <- global_config$get_config('ereg')

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 4. Identificar archivos a graficar -----

# Identificar archivos ACC-CPT
cpt_base_files <-
  tibble::tibble(
    type = 'acc-cpt',
    basename = list.files(
      path = config_cpt$input_folders$calibrated_data$forecasts, 
      pattern = cpt_files_regex,
      full.names = FALSE) %>% tools::file_path_sans_ext(),
    variable = stringr::str_extract(basename, cpt_regex_variables)
  ) %>%
  dplyr::select(
    type, variable, basename
  ) %>%
  dplyr:: mutate(
    det_fcst_file = paste0(basename, '_forecast.nc'),
    det_hcst_file = paste0(basename, '_hindcast.nc'),
    prob_fcst_file = paste0(basename, '_prob_forecast.nc'),
    prob_hcst_file = paste0(basename, '_prob_hindcast.nc')
  ) %>%
  dplyr::mutate(
    obs_file = paste0(
      stringr::str_extract(basename, cpt_regex_variables), '_', 
      stringr::str_extract(basename, cpt_regex_fuente_datos), '_',
      stringr::str_extract(basename, cpt_regex_months), '.nc')
  ) %>% 
  dplyr::mutate(
    uncalibrated_fcst_file = paste0(
      stringr::str_extract(basename, cpt_regex_modelos), '_',
      stringr::str_extract(basename, cpt_regex_pre_variables), '_',
      stringr::str_extract(basename, cpt_regex_init_month), '_',
      stringr::str_extract(basename, cpt_regex_months), '_',
      stringr::str_extract(basename, cpt_regex_hcst_years), '_',
      stringr::str_extract(basename, cpt_regex_fcst_years), '_1.nc')
  ) %>% 
  dplyr::rowwise() %>% dplyr::mutate(
    hcst_first_year = stringr::str_split(
      stringr::str_extract(basename, cpt_regex_hcst_years), '-') %>% 
      unlist() %>% dplyr::first() %>% as.numeric(),
    hcst_last_year = stringr::str_split(
      stringr::str_extract(basename, cpt_regex_hcst_years), '-') %>% 
      unlist() %>% dplyr::last() %>% as.numeric(),
    target_months = ifelse(
      stringr::str_detect(stringr::str_extract(basename, cpt_regex_months), '-'),
      yes = paste(crange(global_ic$month+1, global_ic$month+3, 12), collapse='-'),
      no = stringr::str_extract(basename, cpt_regex_months))
  ) %>% dplyr::ungroup()

# Identificar Archivos EREG-Climax
ereg_base_files <-
  tibble::tibble(
    type = 'ereg',
    basename = list.files(
      path = config_ereg$input_folders$calibrated_data$forecasts, 
      pattern = ereg_files_regex,
      full.names = FALSE) %>% tools::file_path_sans_ext(),
    variable = dplyr::case_when(
      stringr::str_extract(basename, ereg_regex_variables) == 'prec' ~ 'prcp',
      stringr::str_extract(basename, ereg_regex_variables) == 'tref' ~ 't2m',
      TRUE ~ NA_character_)
  ) %>%
  dplyr::select(
    type, variable, basename
  ) %>%
  dplyr:: mutate(
    det_fcst_file = paste0('determin_', basename, '.nc'),
    det_hcst_file = paste0('determin_', stringr::str_remove(basename, ereg_regex_init_year_str), '_hind.nc'),
    prob_fcst_file = paste0(basename, '.nc'),
    prob_hcst_file = paste0(stringr::str_remove(basename, ereg_regex_init_year_str), '_hind.nc')
  ) %>%
  dplyr::rowwise() %>% dplyr::mutate(
    trimester = stringr::str_extract(basename, ereg_regex_months),
    obs_file = paste0(
      'obs_',
      stringr::str_extract(basename, ereg_regex_variables), '_', 
      ifelse(
        all(global_ic$month < MonthsHelper$trimester_to_seq(trimester)),
        yes=1982, no=1983), '_',
      stringr::str_extract(basename, ereg_regex_months), 
      '.nc'),
    uncalibrated_fcst_file = NULL,
    hcst_first_year = ifelse(
      all(global_ic$month < MonthsHelper$trimester_to_seq(trimester)),
      yes=1982, no=1983),
    hcst_last_year = ifelse(
      all(global_ic$month < MonthsHelper$trimester_to_seq(trimester)),
      yes=2010, no=2011),
    target_months = paste(
      MonthsHelper$trimester_to_seq(trimester), collapse='-')
  ) %>% dplyr::select(-trimester) %>% dplyr::ungroup()

# Unir archivos identificados en un único dataframe
base_files <- dplyr::bind_rows(
  list(cpt_base_files, ereg_base_files)
)

# Borrar objetos que ya no se va a utilizar
rm(cpt_base_files, ereg_base_files); gc()

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 5. Graficar -----

for ( i in 1:nrow(base_files) ) {
  
  # Extraer datos sobre el/los archivo/s a graficar
  base_file <- base_files[i,]
  
  # Informar cual archivo se está procesando
  logger::log_info(glue::glue('Procesando archivo: {base_file$basename}'))
  
  # Obtner datos de entrada
  logger::log_info('Inicia lectura de archivos de entrada')
  datos_entrada <- DatosEntrada$new(base_file, global_config$config_file)
  logger::log_info('Finaliza lectura de archivos de entrada')
  
  
  #
  # Preparar máscara seca para ser aplicada antes de graficar
  #
  
  
  # Se setea esta variable a NULL porque se pasa siempre
  # a la función que grafica, pero, la idea que cuando
  # sea NULL, no se aplique ninguna máscara.
  dry_mask_trgt_months <- NULL
  
  
  # Definir máscara cuando corresponda
  if ( base_file$type == 'ereg' && config_ereg$apply_dry_mask ) {
    logger::log_info('Inicia obtención de la máscara a ser aplicada')
    
    # Leer archivo con mascara seca para EREG
    dry_mask_df = tidync::tidync(config_ereg$dry_mask_file) %>% 
      tidync::hyper_tibble(na.rm = FALSE) %>%
      dplyr::rename(
        longitude = X, latitude = Y) %>%
      dplyr::mutate(
        longitude = longitude - 360,
        dry_month = as.logical(prec),
        must_be_masked = dry_month) %>%
      dplyr::select(
        longitude, latitude, month, must_be_masked)
    
    # Recorrer el/los meses objetivo (target month) y marcar punto
    # como "a ser enmscarado" cuando al menos uno de los meses sea seco.
    dry_mask_trgt_months <- 
      purrr::reduce(
        .x = purrr::map(
          .x = stringr::str_split(base_file$target_months, '-') %>% 
            purrr::reduce(c),
          .f = function(m) {
            dry_mask_df %>% 
              dplyr::filter(month == m) %>%
              dplyr::rename(!!paste0('must_be_masked.', m) := must_be_masked) %>%
              dplyr::select(-month)
          }),
        .f = dplyr::inner_join,
        by = c("longitude", "latitude")) %>%
      dplyr::mutate(
        must_be_masked = if_any(dplyr::starts_with("must_be_masked."))) %>%
      dplyr::select(longitude, latitude, must_be_masked)
    
    
    # Si la grilla de la máscara no coincide con la grilla de los datos
    # pronosticados, entonces se debe interpolar la máscara.
    if ( !are_points_joinables(datos_entrada$pred_det_fcst_data$data, 
                               dry_mask_trgt_months) ) {
      logger::log_info('Inicia interpolación de la máscara')
      
      # Obtener dominio espacial
      dominio_espacial <- global_config$get_config('spatial_domain')
      
      # Reducir cantidad de puntos, excluir puntos fuera del dominio espacial
      dry_mask_trgt_months <- dry_mask_trgt_months %>%
        dplyr::filter(longitude >= dominio_espacial$wlo - 2, 
                      longitude <= dominio_espacial$elo + 2, 
                      latitude <= dominio_espacial$nla + 2, 
                      latitude >= dominio_espacial$sla - 2)
      reduced_orig_dry_mask <- dry_mask_trgt_months %>% sf::st_as_sf(
        coords = c("longitude", "latitude"), 
        remove = FALSE, crs = 4326) 
      
      # Definir valores que permiten definir la nueva grilla
      trgt_lons <- datos_entrada$pred_det_fcst_data$data$longitude
      trgt_lats <- datos_entrada$pred_det_fcst_data$data$latitude
      dist_between_lon <- min(spatial_dist(trgt_lons)) / 2
      dist_between_lat <- min(spatial_dist(trgt_lats)) / 2
      delta <- min(dist_between_lon, dist_between_lat)
      
      # Definir nueva grilla
      new_grid_sf <- tibble::as_tibble(
        expand.grid(longitude = seq(from = min(trgt_lons) - delta, 
                                    to = max(trgt_lons) + delta, 
                                    by = delta*2), 
                    latitude = seq(from = min(trgt_lats) - delta, 
                                   to = max(trgt_lats) + delta, 
                                   by = delta*2))) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), 
                     remove = FALSE, crs = 4326)
      
      # Interpolar resultados
      dry_mask_trgt_months <- 
        InterpolationHelper$interp_bilinear(
          data_df = dry_mask_trgt_months,
          cols_to_interp = "must_be_masked",
          new_points = new_grid_sf) %>%
        dplyr::mutate(
          must_be_masked = dplyr::case_when(
            must_be_masked <= 0 ~ as.logical(0),
            must_be_masked > 0 ~ as.logical(1),
            TRUE ~ NA
          )) %>%
        exclude_points_outside_buffered_crcsas()
      
    }  # fin de la interpolación
    
  }  # fin de la creación de la máscara
  
  
  # 
  # Crear gráfico de correlación
  #
  
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
        data_df = .,
        cols_to_interp = "value",
        delta = ifelse(base_file$type == "ereg", .5, .25),
        inplace = ifelse(base_file$type == "ereg", T, F))
      else . } %>%
    exclude_points_outside_crcsas()
  
  # Definir paleta de colores
  red_plt  <- head(rev(RColorBrewer::brewer.pal(3, 'Reds')), 2)
  blue_plt  <- tail(grDevices::colorRampPalette(
    colors = RColorBrewer::brewer.pal(9, 'Blues'))( 11 ), 9)
  paleta_completa <- c(red_plt, "#f1f1f1", blue_plt)
  
  # Crear gráfico
  corr_plot <- PlotsHelper$graficar_mapa(
    data_df = corr_df, 
    gridded_data = are_points_gridded(datos_entrada$pred_det_hcst_data$data),
    main_title = PlotsHelper$definir_titulo("corr", base_file), 
    legend_title = "Correlation", 
    spatial_domain = list(
      nla = max(corr_df$latitude),
      sla = min(corr_df$latitude),
      wlo = min(corr_df$longitude),
      elo = max(corr_df$longitude)), 
    output_file_abspath = paste0(
      global_config$get_config(base_file$type)$output_folder, "/", 
      base_file$basename, "_corr.html"),
    breaks = c(-0.5,-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
    colors = paleta_completa, 
    dry_mask_df = dry_mask_trgt_months,
    save_map = TRUE)
  
  
  #
  # Graficar todos los años en el dataframe de forecasts
  #
  
  anhos_pronosticados <- unique(datos_entrada$pred_det_fcst_data$data$year)
  logger::log_info(glue::glue("Se van a generar gráficos para los siguientes años: {anhos_pronosticados}"))
  
  for (data_year in anhos_pronosticados) {
    
    #
    # Crear gráfico de anomalías
    #
    
    logger::log_info(glue::glue("Inicia el graficado de las Anomalías para el año: {data_year}"))
    
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
    
    # Definir df con los datos a graficar
    anom_df <- datos_entrada$pred_det_fcst_data$data %>%
      dplyr::filter(
        year == data_year) %>%
      dplyr::select(
        longitude, latitude, value = anomaly) %>%
      exclude_points_outside_buffered_crcsas() %>%
      { if ( global_config$get_config(base_file$type)$suavizar_graficos )
        PlotsHelper$aplicar_suavizado(
          data_df = .,
          cols_to_interp = "value",
          delta = ifelse(base_file$type == "ereg", .5, .25),
          inplace = ifelse(base_file$type == "ereg", T, F))
        else . } %>%
      exclude_points_outside_crcsas()
    
    # Definir paleta de colores
    if (base_file$variable == 'prcp') {
      breaks <- c(-100,-50,-20,-10,-5,5,10,20,50,100)
      colores <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(9, 'BrBG'))(11)
      brown_plt <- head(colores, 5)
      green_plt <- tail(colores, 5)
      paleta_completa <- c(brown_plt, "#e5e8e8", green_plt)
      legend_title <- "Anomaly (mm)"
    } else if (base_file$variable == 't2m') {
      breaks <- c(-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2)
      blue_plt <- head(rev(RColorBrewer::brewer.pal(7, 'Blues')), 5)
      red_plt <- tail(RColorBrewer::brewer.pal(7, 'Reds'), 5)
      paleta_completa <- c(blue_plt, "#e5e8e8", red_plt)
      legend_title <- "Anomaly (°C)"
    }
    
    # Crear gráfico
    anom_plot <- PlotsHelper$graficar_mapa(
      data_df = anom_df, 
      gridded_data = det_gridded_data,
      main_title = PlotsHelper$definir_titulo("anom", base_file, data_year), 
      legend_title = legend_title, 
      spatial_domain = list(
        nla = max(anom_df$latitude),
        sla = min(anom_df$latitude),
        wlo = min(anom_df$longitude),
        elo = max(anom_df$longitude)), 
      output_file_abspath = paste0(
        global_config$get_config(base_file$type)$output_folder, "/", 
        base_file$basename, "_anom.html"),
      breaks = breaks,
      colors = paleta_completa, 
      dry_mask_df = dry_mask_trgt_months,
      save_map = TRUE)
    
    
    #
    # Crear gráfico de valores determinísticos pronosticados
    #
    
    logger::log_info(glue::glue("Inicia el graficado del Pronóstico Determinístico para el año: {data_year}"))
    
    # Definir df con los datos a graficar
    det_fcst_df <- datos_entrada$pred_det_fcst_data$data %>%
      dplyr::filter(
        year == data_year) %>%
      dplyr::select(
        longitude, latitude, value = !!rlang::sym(base_file$variable)) %>%
      exclude_points_outside_buffered_crcsas() %>%
      { if ( global_config$get_config(base_file$type)$suavizar_graficos )
        PlotsHelper$aplicar_suavizado(
          data_df = .,
          cols_to_interp = "value",
          delta = ifelse(base_file$type == "ereg", .5, .25),
          inplace = ifelse(base_file$type == "ereg", T, F))
        else . } %>%
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
      legend_title <- "Precipitation (mm)"
    } else if (base_file$variable == 't2m') {
      breaks <- c(-9,-6,-3,3,6,9,12,15,18,21,24,27,30,33)
      paleta_completa <- viridis::turbo(15)
      legend_title <- "Temperature 2m (°C)"
    }
    
    # Crear gráfico
    det_fcst_plot <- PlotsHelper$graficar_mapa(
      data_df = det_fcst_df, 
      gridded_data = det_gridded_data,
      main_title = PlotsHelper$definir_titulo("det.fcst", base_file, data_year), 
      legend_title = legend_title, 
      spatial_domain = list(
        nla = max(det_fcst_df$latitude),
        sla = min(det_fcst_df$latitude),
        wlo = min(det_fcst_df$longitude),
        elo = max(det_fcst_df$longitude)), 
      output_file_abspath = paste0(
        global_config$get_config(base_file$type)$output_folder, "/", 
        base_file$basename, "_det_fcst.html"),
      breaks = breaks,
      colors = paleta_completa, 
      dry_mask_df = dry_mask_trgt_months,
      save_map = TRUE)
  
  
    #
    # Crear gráfico de valores probabilísticos pronosticados
    #
    
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
        PlotsHelper$aplicar_suavizado(
          data_df = .,
          cols_to_interp = c("prob_below", "prob_normal", "prob_above"),
          delta = ifelse(base_file$type == "ereg", .5, .25),
          inplace = ifelse(base_file$type == "ereg", T, F))
        else . } %>%
      exclude_points_outside_crcsas() %>%
      FcstProbabilisticData$add_categories(
        prob_data_df = prob_fcst_df, 
        below_col = "prob_below", 
        normal_col = "prob_normal", 
        above_col = "prob_above")
    
    # Definir paleta de colores de la NOAA
    # Ver: https://www.weather.gov/news/211409-temperature-precipitation-maps
    noaa_escala_azules <- tail(RColorBrewer::brewer.pal(9, "PuBu"), 7)
    noaa_escala_rojos <- tail(RColorBrewer::brewer.pal(9, "YlOrBr"), 7)
    
    # Definir paleta de colores
    breaks <- c(0.33, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1)
    paleta_below <- if (base_file$variable == "prcp") noaa_escala_rojos else noaa_escala_azules
    paleta_normal <- tail(RColorBrewer::brewer.pal(9, 'Greys'), 7)
    paleta_above <- if (base_file$variable == "prcp") noaa_escala_azules else noaa_escala_rojos
    
    # Crear gráfico
    prob_fcst_plot <- PlotsHelper$graficar_mapa_prob(
      data_df = prob_fcst_df, 
      gridded_data = prob_gridded_data,
      main_title = PlotsHelper$definir_titulo("prob.fcst", base_file, data_year), 
      spatial_domain = list(
        nla = max(prob_fcst_df$latitude),
        sla = min(prob_fcst_df$latitude),
        wlo = min(prob_fcst_df$longitude),
        elo = max(prob_fcst_df$longitude)), 
      output_file_abspath = paste0(
        global_config$get_config(base_file$type)$output_folder, "/", 
        base_file$basename, "_prob_fcst.html"),
      breaks = breaks,
      colors_below = paleta_below, 
      colors_normal = paleta_normal, 
      colors_above = paleta_above, 
      dry_mask_df = dry_mask_trgt_months,
      save_map = TRUE)
    
    
    #
    # Crear gráfico de valores pronosticados antes de ser calibrados
    #
    
    if ( !is.null(datos_entrada$uncalibrated_fcst_data) ) {
      
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
        legend_title <- "Precipitation (mm)"
      } else if (base_file$variable == 't2m') {
        breaks <- c(-9,-6,-3,3,6,9,12,15,18,21,24,27,30,33)
        paleta_completa <- viridis::turbo(15)
        legend_title <- "Temperature 2m (°C)"
      }
      
      # Crear gráfico
      uncal_fcst_plot <- PlotsHelper$graficar_mapa(
        data_df = uncal_fcst_df, 
        gridded_data = uncal_gridded_data,
        main_title = PlotsHelper$definir_titulo("uncal.fcst", base_file, data_year), 
        legend_title = legend_title, 
        spatial_domain = list(
          nla = max(uncal_fcst_df$latitude),
          sla = min(uncal_fcst_df$latitude),
          wlo = min(uncal_fcst_df$longitude),
          elo = max(uncal_fcst_df$longitude)), 
        output_file_abspath = paste0(
          global_config$get_config(base_file$type)$output_folder, "/", 
          base_file$basename, "_uncal_fcst.html"),
        breaks = breaks,
        colors = paleta_completa, 
        dry_mask_df = dry_mask_trgt_months,
        save_map = TRUE)
      
    }  # FIN DEL IF: if ( !is.null(datos_entrada$uncalibrated_fcst_data) )
  
  }  # FIN DEL FOR: for (data_year in anhos_pronosticados)

}  # FIN DEL FOR: for ( i in 1:nrow(base_files) )  

# ------------------------------------------------------------------------------
