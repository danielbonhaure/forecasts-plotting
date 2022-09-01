
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
