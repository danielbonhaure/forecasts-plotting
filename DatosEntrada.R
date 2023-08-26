
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "tibble", "stringr", "purrr", "here")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))

# Cargar librerias
source(here::here("lib", "Helpers.R"), echo = FALSE, chdir = TRUE)
source(here::here("lib", "Configuracion.R"), echo = FALSE, chdir = TRUE)
source(here::here("lib", "DatosEntradaObservados.R"), echo = FALSE, chdir = TRUE)
source(here::here("lib", "DatosEntradaPronosticados.R"), echo = FALSE, chdir = TRUE)


# La clase DatosEntrada tiene por objetivo:
# 1- Encapsular la lógica que determina los archivos a leer. Los archivos 
#    con datos observados y pronosticados pueden ser especificados como un
#    solo archivo, una lista de archivos o un pattern.
# 2- Llamar a los métodos que agrupan los datos observados, agregan
#    categorías, interpolan los datos pronosticados para ajustarlos a la 
#    grilla de los datos observados, etc.
#
DatosEntrada <- R6::R6Class(
  classname = "DatosEntrada",
  public = list(
    variable = character(),
    files_info = tibble::tibble(
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
      target_months = character()
    ),
    obs_data = ObsDeterministicData,
    pred_det_fcst_data = FcstDeterministicData,
    pred_det_hcst_data = FcstDeterministicData,
    pred_prob_fcst_data = FcstProbabilisticData,
    pred_prob_hcst_data = FcstProbabilisticData,
    pred_prob_xtrm_data = FcstProbabilisticData,
    uncalibrated_fcst_data = NULL,
    
    initialize = function(files_info, config_file) {
      # Verificar tibble recibido
      if ( !tibble::is_tibble(files_info) )
        stop('El parámetro recibido no es un tibble')
      if ( nrow(files_info) == 0 )
        stop('El tibble recibido no contiene información')
      if ( nrow(files_info) >= 2 )
        stop('El tibble recibido debe tener una sola fila')
      
      # Guardar variable
      self$variable = files_info$variable
      
      # Guardar información recibida
      self$files_info = files_info
      
      # Leer configuración
      private$pv_config <- Config$new(config_file)
      
      # Obtener datos
      private$pv_get_obs_data()
      private$pv_get_pred_det_fcst_data()
      private$pv_get_pred_det_hcst_data()
      private$pv_get_pred_prob_fcst_data()
      private$pv_get_pred_prob_hcst_data()
      private$pv_get_pred_prob_xtrm_data()
      private$pv_get_uncalibrated_fcst_data()
    }
  ),
  private = list(
    pv_config = Config,
    
    pv_get_obs_data = function() {
      # Extraer configuración
      obs_config <- private$pv_config$get_config(self$files_info$type)
      
      # Identificar archivos con los datos
      obs_file <- paste0(obs_config$input_folders$observed_data, 
                         '/', self$files_info$obs_file)
      
      # Crear estrategia de corrección
      correction_strategy <- NULL
      if ( !is.null(obs_config$correction_strategies$observed_data) ) {
        correction_factory <- CorrectionStrategyFactory$new(
          strategy = obs_config$correction_strategies$observed_data)
        correction_strategy <- correction_factory$create_obj(
          df_to_get_sd = self$obs_data$data, value_col = self$variable)
      }
      
      # Crear factoría
      obs_data_factory <- ObsDataFactory$new(
        self$variable)
      
      # Definir parámetros para creación del objeto 
      # que permite obtener los datos
      params <- list()
      params[['data_file']] <- obs_file
      params[['data_variable']] <- self$variable
      params[['time']] <- 'init_time'
      if (!is.null(private$pv_config$get_config("spatial_domain")) && self$files_info$type == 'pycpt')
        params[['bbox']] <- private$pv_config$get_config("spatial_domain")
      if (!is.null(correction_strategy))
        params[['correction_strategy']] <- correction_strategy
      # Crear objeto que permite acceder a los datos
      self$obs_data <- do.call(obs_data_factory$create_det_obj, params)
      
      # Agrupar datos
      self$obs_data$group_data_by_months(
        stringr::str_split(self$files_info$target_months, '-') %>% 
          purrr::reduce(c) %>% as.numeric())
      
      # Calular anomalías
      self$obs_data$add_anomalies(
        self$files_info$hcst_first_year, self$files_info$hcst_last_year)
      
      # Agregar categorías
      self$obs_data$add_categories(
        self$files_info$hcst_first_year, self$files_info$hcst_last_year)
      
    },
    pv_get_pred_det_fcst_data = function() {
      # Extraer configuración
      det_fcst_config <- private$pv_config$get_config(self$files_info$type)
      
      # Identificar archivos con los datos
      det_fcst_file <- paste0(
        det_fcst_config$input_folders$calibrated_data$forecasts, '/', 
        self$files_info$det_fcst_file)
      
      # Crear estrategia de corrección
      correction_strategy <- NULL
      if ( !is.null(det_fcst_config$correction_strategies$calibrated_data$det_values$forecasts[[self$variable]]) ) {
        correction_factory <- CorrectionStrategyFactory$new(
          strategy = det_fcst_config$correction_strategies$calibrated_data$det_values$forecasts[[self$variable]])
        correction_strategy <- correction_factory$create_obj(
          df_to_get_sd = self$obs_data$data, value_col = self$variable)
      }
      
      # Crear proveedor de factoría
      factory_provider <- FcstDataFactoryProvider$new(
        self$variable)
      # Obtener factoría
      data_factory <- factory_provider$get_factory()
      
      # Definir parámetros para creación del objeto 
      # que permite obtener los datos
      params <- list()
      params[['data_file']] <- det_fcst_file
      params[['data_variable']] <- self$variable
      params[['target_months']] <- 
        stringr::str_split(self$files_info$target_months, '-') %>% 
          purrr::reduce(c) %>% as.numeric()
      params[['time']] <- 'init_time'
      if (!is.null(correction_strategy))
        params[['correction_strategy']] <- correction_strategy
      # Crear objeto que permite acceder a los datos
      self$pred_det_fcst_data <- do.call(data_factory$create_det_obj, params)
      
      # Se agrega la columna anomaly o self$variable según sea necesario
      self$pred_det_fcst_data$add_anom_or_det(
        df_to_get_mean = self$obs_data$data,
        self$files_info$hcst_first_year, 
        self$files_info$hcst_last_year)
    },
    pv_get_pred_det_hcst_data = function() {
      # Extraer configuración
      det_hcst_config <- private$pv_config$get_config(self$files_info$type)
      
      # Identificar archivos con los datos
      det_hcst_file <- paste0(
        det_hcst_config$input_folders$calibrated_data$hindcasts, '/', 
        self$files_info$det_hcst_file)
      
      # Crear estrategia de corrección
      correction_strategy <- NULL
      if ( !is.null(det_hcst_config$correction_strategies$calibrated_data$det_values$hindcasts[[self$variable]]) ) {
        correction_factory <- CorrectionStrategyFactory$new(
          strategy = det_hcst_config$correction_strategies$calibrated_data$det_values$hindcasts[[self$variable]])
        correction_strategy <- correction_factory$create_obj(
          df_to_get_sd = self$obs_data$data, value_col = self$variable)
      }
      
      # Crear proveedor de factoría
      factory_provider <- FcstDataFactoryProvider$new(
        self$variable)
      # Obtener factoría
      data_factory <- factory_provider$get_factory()
      
      # Definir parámetros para creación del objeto 
      # que permite obtener los datos
      params <- list()
      params[['data_file']] <- det_hcst_file
      params[['data_variable']] <- self$variable
      params[['target_months']] <- 
        stringr::str_split(self$files_info$target_months, '-') %>% 
        purrr::reduce(c) %>% as.numeric()
      params[['time']] <- 'init_time'
      if (!is.null(correction_strategy))
        params[['correction_strategy']] <- correction_strategy
      # Crear objeto que permite acceder a los datos
      self$pred_det_hcst_data <- do.call(data_factory$create_det_obj, params)
      
      # Se agrega la columna anomaly o self$variable según sea necesario
      self$pred_det_hcst_data$add_anom_or_det(
        df_to_get_mean = self$obs_data$data,
        self$files_info$hcst_first_year, 
        self$files_info$hcst_last_year)
    },
    pv_get_pred_prob_fcst_data = function() {
      # Extraer configuración
      prob_fcst_config <- private$pv_config$get_config(self$files_info$type)
      
      # Identificar archivos con los datos
      prob_fcst_file <- paste0(
        prob_fcst_config$input_folders$calibrated_data$forecasts, '/', 
        self$files_info$prob_fcst_file)
      
      # Crear proveedor de factoría
      factory_provider <- FcstDataFactoryProvider$new(
        self$variable)
      # Obtener factoría
      data_factory <- factory_provider$get_factory()
      
      # Definir parámetros para creación del objeto 
      # que permite obtener los datos
      params <- list()
      params[['data_file']] <- prob_fcst_file
      params[['data_variable']] <- self$variable
      params[['target_months']] <- 
        stringr::str_split(self$files_info$target_months, '-') %>% 
        purrr::reduce(c) %>% as.numeric()
      params[['time']] <- 'init_time'
      # Crear objeto que permite acceder a los datos
      self$pred_prob_fcst_data <- do.call(data_factory$create_prob_obj, params)
      
      # Agregar columna con categorías
      self$pred_prob_fcst_data$add_categories()
    },
    pv_get_pred_prob_hcst_data = function() {
      # Extraer configuración
      prob_hcst_config <- private$pv_config$get_config(self$files_info$type)
      
      # Identificar archivos con los datos
      prob_hcst_file <- paste0(
        prob_hcst_config$input_folders$calibrated_data$hindcasts, '/', 
        self$files_info$prob_hcst_file)
      
      # Crear proveedor de factoría
      factory_provider <- FcstDataFactoryProvider$new(
        self$variable)
      # Obtener factoría
      data_factory <- factory_provider$get_factory()
      
      # Definir parámetros para creación del objeto 
      # que permite obtener los datos
      params <- list()
      params[['data_file']] <- prob_hcst_file
      params[['data_variable']] <- self$variable
      params[['target_months']] <- 
        stringr::str_split(self$files_info$target_months, '-') %>% 
        purrr::reduce(c) %>% as.numeric()
      params[['time']] <- 'init_time'
      # Crear objeto que permite acceder a los datos
      self$pred_prob_hcst_data <- do.call(data_factory$create_prob_obj, params)
      
      # Agregar columna con categorías
      self$pred_prob_hcst_data$add_categories()
    },
    pv_get_pred_prob_xtrm_data = function() {
      # La probabilidad de extremos solo se calcula para EREG
      if (self$files_info$type == 'ereg') {
        
        # Extraer configuración
        prob_xtrm_config <- private$pv_config$get_config(self$files_info$type)
        
        # Identificar archivos con los datos
        prob_xtrm_file <- paste0(
          prob_xtrm_config$input_folders$calibrated_data$forecasts, '/',
          self$files_info$prob_xtrm_file)
        
        # Crear proveedor de factoría
        factory_provider <- FcstDataFactoryProvider$new(
          self$variable)
        # Obtener factoría
        data_factory <- factory_provider$get_factory()

        # Definir parámetros para creación del objeto
        # que permite obtener los datos
        params <- list()
        params[['data_file']] <- prob_xtrm_file
        params[['data_variable']] <- self$variable
        params[['target_months']] <-
          stringr::str_split(self$files_info$target_months, '-') %>%
          purrr::reduce(c) %>% as.numeric()
        params[['time']] <- 'init_time'
        # Crear objeto que permite acceder a los datos
        self$pred_prob_xtrm_data <- do.call(data_factory$create_prob_obj, params)
        
        # Agregar columna con categorías
        self$pred_prob_xtrm_data$add_categories()
        
      }
    },
    pv_get_uncalibrated_fcst_data = function() {
      # No siempre está disponible el prono sin calibrar
      if ( !is.null(self$files_info$uncalibrated_fcst_file) &&
           !is.na(self$files_info$uncalibrated_fcst_file) ) {
        
        # Extraer configuración
        uncal_fcst_config <- private$pv_config$get_config(self$files_info$type)
        
        # Identificar archivos con los datos
        uncal_fcst_file <- paste0(
          uncal_fcst_config$input_folders$uncalibrated_data, '/',
          self$files_info$uncalibrated_fcst_file)
        
        # Crear estrategia de corrección
        correction_strategy <- NULL
        if ( !is.null(uncal_fcst_config$correction_strategies$uncalibrated_data) ) {
          correction_factory <- CorrectionStrategyFactory$new(
            strategy = uncal_fcst_config$correction_strategies$uncalibrated_data)
          correction_strategy <- correction_factory$create_obj(
            df_to_get_sd = self$obs_data$data, value_col = self$variable)
        }
        
        # Crear proveedor de factoría
        factory_provider <- FcstDataFactoryProvider$new(
          self$variable)
        # Obtener factoría
        data_factory <- factory_provider$get_factory()
        
        # Definir parámetros para creación del objeto 
        # que permite obtener los datos
        params <- list()
        params[['data_file']] <- uncal_fcst_file
        params[['data_variable']] <- self$variable
        params[['target_months']] <- 
          stringr::str_split(self$files_info$target_months, '-') %>% 
          purrr::reduce(c) %>% as.numeric()
        params[['time']] <- 'init_time'
        if (!is.null(correction_strategy))
          params[['correction_strategy']] <- correction_strategy
        # Crear objeto que permite acceder a los datos
        self$uncalibrated_fcst_data <- do.call(data_factory$create_det_obj, params)
        
      }
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
#DatosEntrada$debug("pv_get_obs_data")
#DatosEntrada$undebug("pv_get_obs_data")
#DatosEntrada$debug("pv_get_pred_det_data")
#DatosEntrada$undebug("pv_get_pred_det_data")
#DatosEntrada$debug("pv_get_pred_prob_data")
#DatosEntrada$undebug("pv_get_pred_prob_data")

#
#

