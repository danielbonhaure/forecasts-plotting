
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "lubridate", "rlang")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))


# Patrón factory para crear la instancia correcta de CorrectionStrategy


# ABSTRACT FACTORY
CorrectionStrategyAbsFactory <- R6::R6Class(
  classname = "CorrectionStrategyAbsFactory",
  public = list(
    create_obj = function(...) stop("I'm an abstract interface method")
  ),
  active = list(
    correction_strategy = function() {
      stop("I'm an abstract interface method")
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE FACTORY 
CorrectionStrategyFactory <- R6::R6Class(
  classname = "CorrectionStrategyFactory",
  public = list(
    initialize = function(strategy) {
      private$check_inputs(strategy)
      private$pv_strategy <- strategy
    },
    
    create_obj = function(...) {
      if (private$pv_strategy == "era5-land_obs") {
        return ( Era5LandCorrectionStrategy$new(...) )
      } else if (private$pv_strategy == "cpc-cmap_obs") {
        return ( CPCprateCorrectionStrategy$new(...) )
      } else if (private$pv_strategy == "cpt_invalid_prcp_values") {
        return ( CPTprcpPronoCorrectionStrategy$new(...) )
      }else {  stop("Invalid strategy") }
    }
  ),
  private = list(
    pv_strategy = character(), 
    
    check_inputs = function(strategy) {
      stopifnot(is.character(strategy), length(strategy) == 1)
      stopifnot(strategy %in% c("era5-land_obs", "cpc-cmap_obs", 
                                "cpt_invalid_prcp_values"))
    }
  ),
  active = list(
    correction_strategy = function() {
      return ( private$pv_strategy )
    }
  ),
  inherit = CorrectionStrategyAbsFactory,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)


# Implementación del patrón de diseño strategy para corregir datos observados


# Clase abstracta de la que heredan la estrategias concretas
CorrectionStrategy <- R6::R6Class(
  classname = "CorrectionStrategy",
  public = list(
    apply_correction = function(data_to_correct) {
      stop("I'm an abstract interface method")
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# Clase concreta para aplicar la corrección que requieren los datos de Era5-Land
Era5LandCorrectionStrategy <- R6::R6Class(
  classname = "Era5LandCorrectionStrategy",
  public = list(
    initialize = function(value_col = "value", time_col = "time", ...) {
      private$pv_value_col <- value_col
      private$pv_time_col <- time_col
    },
    apply_correction = function(data_to_correct) {
      corrected_data <- data_to_correct %>%
        # 1era corr: multiplar por 1000 para pasar de m a mm
        dplyr::mutate(
          !!private$pv_value_col := !!as.name(private$pv_value_col) * 1000
        ) %>%
        # 2da corr: multiplicar por la cantidad de días en el mes
        dplyr::mutate(
          !!private$pv_value_col := !!as.name(private$pv_value_col) * 
            lubridate::days_in_month(!!as.name(private$pv_time_col))
        )
      # Se retornan los datos corregidos
      return ( corrected_data )
    }
  ),
  private = list(
    pv_value_col = NULL,
    pv_time_col = NULL
  ),
  inherit = CorrectionStrategy,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
#Era5LandCorrectionStrategy$debug("apply_correction")
#Era5LandCorrectionStrategy$undebug("apply_correction")

# Clase concreta para aplicar la corrección que requieren los datos de CPC-CMAP
CPCprateCorrectionStrategy <- R6::R6Class(
  classname = "CPCprateCorrectionStrategy",
  public = list(
    initialize = function(value_col = "value", time_col = "time", ...) {
      private$pv_value_col <- value_col
      private$pv_time_col <- time_col
    },
    apply_correction = function(data_to_correct) {
      corrected_data <- data_to_correct %>%
        # 1era corr: multiplicar por la cantidad de días en el mes
        dplyr::mutate(
          !!private$pv_value_col := !!as.name(private$pv_value_col) * 
            lubridate::days_in_month(!!as.name(private$pv_time_col))
        )
      # Se retornan los datos corregidos
      return ( corrected_data )
    }
  ),
  private = list(
    pv_value_col = NULL,
    pv_time_col = NULL
  ),
  inherit = CorrectionStrategy,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
#CPCprateCorrectionStrategy$debug("apply_correction")
#CPCprateCorrectionStrategy$undebug("apply_correction")

# Clase concreta para aplicar la corrección que requieren los datos de salida
# determinísticos de precitación del CPT. Es una corrección indicada por 
# Fabricio para mejorar la correlación. La corrección consiste en eliminar
# valores a más de 7 desviaciones estándar de la media.
CPTprcpPronoCorrectionStrategy <- R6::R6Class(
  classname = "CPTprcpPronoCorrectionStrategy",
  public = list(
    initialize = function(lon_col = "longitude", lat_col = "latitude", 
                          value_col = "prcp", year_col = "year", 
                          month_col = "month", df_to_get_sd, ...) {
      private$pv_lon_col <- lon_col
      private$pv_lat_col <- lat_col
      private$pv_value_col <- value_col
      private$pv_year_col <- year_col
      private$pv_month_col <- month_col
      private$pv_df_to_get_sd <- df_to_get_sd
    },
    apply_correction = function(data_to_correct) {
      # Corregir valores fuera de rango (asignar random entre 0 y 0.1, sin 0 y 0.1)
      # el problema es que la correlación lanza warnings si reemplazo todos por 0!!
      corrected_data <- data_to_correct %>%
        # los valores NaN hace que las funciones no se comporten como
        # se espera, por ejemplo dplyr::between asume que NaN sí es un valor
        # entre 0 y 0 (una comparación frecuente)
        dplyr::mutate(
          !!private$pv_value_col := ifelse(
            is.nan(!!rlang::sym(private$pv_value_col)),
            yes=NA, no=!!rlang::sym(private$pv_value_col))
        ) %>%
        # ahora sí se puede seguir con la corrección de los datos
        dplyr::left_join(
          private$get_correction_ranges(), 
          by = c(private$pv_lon_col, private$pv_lat_col, 
                 private$pv_year_col, private$pv_month_col)) %>%
        dplyr::mutate(
          aux_value = runif(n = n(), min = 0, max = 0.1)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          in_range = dplyr::between(
            !!rlang::sym(private$pv_value_col), 
            min_valid_value, 
            max_valid_value)) %>%
        dplyr::mutate(
          !!private$pv_value_col := ifelse(
            !is.na(!!rlang::sym(private$pv_value_col)) & 
              !is.na(in_range) & !in_range & point_to_check, 
            yes=aux_value, 
            no=!!rlang::sym(private$pv_value_col))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-point_to_check, -min_valid_value, -max_valid_value, 
                      -aux_value, -in_range)
      # Se retornan los datos corregidos
      return ( corrected_data )
    }
  ),
  private = list(
    pv_lon_col = NULL,
    pv_lat_col = NULL,
    pv_value_col = NULL,
    pv_year_col = NULL,
    pv_month_col = NULL,
    pv_df_to_get_sd = NULL,
    
    get_correction_ranges = function() {
      agrupar_por <- c(private$pv_lon_col, private$pv_lat_col)
      # Calcular estadísticas sobre de los datos observados
      correction_ranges_df <- private$pv_df_to_get_sd %>%
        # como la variable analizada es siempre precipitación, no puede haber
        # valores menores a 0, si los hay, puede que se trate de un error o que 
        # ese valor haya sido utilizado para representar los NA (ej: -999)
        dplyr::mutate(
          !!private$pv_value_col := ifelse(
            !!rlang::sym(private$pv_value_col) < 0, 
            yes=NA, no=!!rlang::sym(private$pv_value_col))
        ) %>%
        # ahora sí se pueden agrupar los datos y calcular los rangos
        dplyr::group_by(
          dplyr::across(dplyr::all_of(agrupar_por))) %>%
        dplyr::mutate(
          mx = ifelse(
            any(!is.na(!!rlang::sym(private$pv_value_col))), 
            yes=max(!!rlang::sym(private$pv_value_col), na.rm = TRUE), 
            no=NA),
          point_to_check = sum(
            !!rlang::sym(private$pv_value_col), na.rm = T) <= 0.2,
          sd = sd(
            !!rlang::sym(private$pv_value_col), na.rm = TRUE) # desviación estándar, excel pt: DESVPAD
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          min_valid_value = mx - sd*7,
          max_valid_value = mx + sd*7
        ) %>%
        dplyr::mutate(
          min_valid_value = ifelse(min_valid_value < 0, 0, min_valid_value),
          max_valid_value = ifelse(max_valid_value < 0, 0, max_valid_value)
        ) %>%
        dplyr::select(private$pv_lon_col, private$pv_lat_col, 
                      private$pv_year_col, private$pv_month_col, 
                      point_to_check, min_valid_value, max_valid_value)
      # Se retornan los rangos
      return ( correction_ranges_df )
    }
  ),
  inherit = CorrectionStrategy,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
#CPTprcpPronoCorrectionStrategy$debug("apply_correction")
#CPTprcpPronoCorrectionStrategy$undebug("apply_correction")
