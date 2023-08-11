
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "yaml")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))


Config <- R6::R6Class(
  classname = "Config",
  public = list(
    initialize = function(configuration_file) {
      private$pv_config_file <- configuration_file
      private$pv_config_data <- private$pv_read_config()
    },
    
    get_config = function(tags, from = NULL) {
      for ( tag in tags )
        if ( is.null(from) )
          from <- private$pv_config_data[[tag]]
        else
          from <- from[[tag]]
      return ( from )
    },
    
    get_initial_conditions = function() {
      
      # Definir argumentos admitidos
      option_list = list(
        optparse::make_option(
          c("--year"), type="integer", default=lubridate::year(lubridate::now()), 
          help="Year that should be considered as initial condition (Default: %default)."),
        optparse::make_option(
          c("--month"), type="integer", default=lubridate::month(lubridate::now()), 
          help="Month that should be considered as initial condition (Default: %default).")
      )
      
      # Parsear argumentos del script
      opt = optparse::parse_args(
        optparse::OptionParser(option_list=option_list))
      
      # Obtener condiciones inciales desde el archivo de configuración
      global_ic <- self$get_config('initial_conditions')
      
      # Definir condifiones inciales según corresponda
      if ( is.null(global_ic) ) {
        global_ic <- list()
        global_ic$month <- as.integer(opt$month)
        global_ic$year <- as.integer(opt$year)
      }
      
      # Retornar condiciones iniciales
      return (global_ic)
      
    }
  ),
  private = list(
    pv_config_file = character(),
    pv_config_data = list(),
    
    pv_read_config = function() {
      private$pv_config_data <- yaml::read_yaml(private$pv_config_file)
    }
  ),
  active = list(
    config_file = function() {
      return ( private$pv_config_file )
    },
    config_data = function() {
      return ( private$pv_config_data )
    }
  ),
  lock_objects = FALSE,
  portable = TRUE,
  lock_class = FALSE
)
