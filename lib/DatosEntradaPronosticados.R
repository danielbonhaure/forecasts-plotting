
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "glue", "metR", "lubridate", "stats", "stringr",
              "tibble", "purrr", "tidync", "tidyr", "here")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))

# Cargar helpers
source(here::here("lib", "Helpers.R"), echo = FALSE, chdir = TRUE)

# Cargar estrategias de corrección
source(here::here("lib", "EstrategiasDeCorreccion.R"), echo = FALSE, chdir = TRUE)


# The type for forecast data
forecast_type = "forecast_data"


# Implementation of Abstract Factory Pattern 
# Objective is to model input data!!


# ABSTRACT FACTORY
FcstDataFactoryProvider <- R6::R6Class(
  classname = "FcstDataFactoryProvider",
  public = list(
    initialize = function(variable) {
      private$check_inputs(variable)
      private$pv_variable <- variable
    },
    
    get_factory = function() {
      if (private$pv_variable == "t2m") {
        return ( FcstTempDataFactory$new() )
      } else if (private$pv_variable == "prcp") {
        return ( FcstPrcpDataFactory$new() )
      } else { stop("Invalid variable.") }
    }
  ),
  private = list(
    pv_variable = character(),  # Valid values are: t2m (temperature), prcp (precipitation)
    
    check_inputs = function(variable) {
      stopifnot(is.character(variable), length(variable) == 1)
      stopifnot(variable %in% c("t2m", "prcp"))
    }
  ),
  active = list(
    type = function() {
      return ( forecast_type )  
    },
    variable = function() {
      return ( private$pv_variable )  
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)


# ABSTRACT FACTORY
FcstDataAbsFactory <- R6::R6Class(
  classname = "FcstDataAbsFactory",
  public = list(
    create_det_obj = function(...) stop("I'm an abstract interface method"),
    create_prob_obj = function(...) stop("I'm an abstract interface method")
  ),
  active = list(
    type = function() {
      return ( forecast_type )  
    },
    variable = function() {
      stop("I'm an abstract interface method")
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE FACTORY FOR TEMPERATURE
FcstTempDataFactory <- R6::R6Class(
  classname = "FcstTempDataFactory",
  public = list(
    create_det_obj = function(...) {
      return ( 
        FcstTempDeterministicData$new(...) 
      )
    },
    create_prob_obj = function(...) {
      return ( 
        FcstTempProbabilisticData$new(...) 
      )
    }
  ),
  active = list(
    variable = function() {
      return ( "t2m" )
    }
  ),
  inherit = FcstDataAbsFactory,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE FACTORY FOR PRECIPITATION
FcstPrcpDataFactory <- R6::R6Class(
  classname = "FcstPrcpDataFactory",
  public = list(
    create_det_obj = function(...) {
      return ( 
        FcstPrcpDeterministicData$new(...) 
      )
    },
    create_prob_obj = function(...) {
      return ( 
        FcstPrcpProbabilisticData$new(...) 
      )
    }
  ),
  active = list(
    variable = function() {
      return ( "prcp" )
    }
  ),
  inherit = FcstDataAbsFactory,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)


# ABSTRACT DETERMINISTIC PRODUCTS
FcstDeterministicData <- R6::R6Class(
  classname = "FcstDeterministicData",
  public = list(
    data = NULL,
    initialize = function(data_file, data_variable, target_months,
                          longitude = "longitude", latitude = "latitude", 
                          time = "init_time", correction_strategy = NULL) {
      
      private$check_inputs(data_file, data_variable, target_months)
      private$pv_data_file <- data_file
      private$pv_value_var <- data_variable
      private$pv_tgt_month <- target_months
      
      private$pv_data_type <- ifelse(
        DataTypeHelper$data_expressed_in_anomalies(data_file, data_variable),
        "anomalies", "total_values")
      
      private$check_file_variables(longitude, latitude, time)
      private$pv_longitude_var <- longitude
      private$pv_latitude_var <- latitude
      private$pv_time_var <- time
      
      private$check_correction_strategy(correction_strategy)
      private$pv_correction_strategy <- correction_strategy
      
      private$pv_data_already_interpolated <- FALSE
      
      private$load_data()
    },
    
    reload_data = function() {
      private$pv_data_already_interpolated <- FALSE
      
      private$load_data()
    },
    
    add_anom_or_det = function(df_to_get_mean, 
                               lower_training_year = NULL,
                               higher_training_year = NULL) {
      
      # Definir las columnas que debe tener df_to_get_mean
      needed_cols <- c("longitude", "latitude", "year", "month", self$variable)
      df_to_get_mean <- df_to_get_mean %>% dplyr::select(dplyr::any_of(needed_cols))
      
      # Si df_to_get_mean no tiene las columnas apropiadas, no se puede seguir
      if ( ! all( needed_cols %in% colnames(df_to_get_mean) ) ) {
        warning("El df con datos obsevardos no tiene las columnas esperadas")
        return ( invisible(NULL) )
      }
      
      # Si las grillas no son iguales, entonces no se puede seguir
      if ( !are_points_joinables(self$data, df_to_get_mean) ) {
        warning("El df con datos obsevardos y el df con datos ",
                "pronosticados no tienen la misma grilla")
        return ( invisible(NULL) )
      }
      
      #
      #
      
      # Se recalcula el promedio, para estar seguros de usar el promedio correcto
      promedios <- df_to_get_mean %>%
        # Se excluyen los años que no deben ser usados para el cálculo 
        # de las estadísticas (si fueron definidos)
        { if ( !is.null(lower_training_year) ) 
          dplyr::filter(., year >= lower_training_year) 
          else . } %>%
        { if ( !is.null(higher_training_year) ) 
          dplyr::filter(., year <= higher_training_year) 
          else . } %>%
        dplyr::group_by(longitude, latitude) %>%
        dplyr::summarise(
          mean = mean(!!as.name(self$variable), na.rm = TRUE), 
          .groups = 'drop'
        ) %>%
        dplyr::ungroup()
      
      #
      #
      
      # Con el promedio re-calculado, se agregan las columnas faltantes
      if ( private$pv_data_type == "anomalies" ) {
        self$data <- self$data %>% 
          dplyr::left_join(
            promedios, by = c("longitude", "latitude")) %>%
          dplyr::mutate(anomaly = !!as.name(self$variable)) %>%
          dplyr::mutate(!!self$variable := mean + anomaly) %>%
          dplyr::select(-mean)
      } else if ( private$pv_data_type == "total_values" ) {
        self$data <- self$data %>% 
          dplyr::left_join(
            promedios, by = c("longitude", "latitude")) %>%
          dplyr::mutate(anomaly = !!as.name(self$variable) - mean) %>%
          dplyr::select(-mean)
      } else {
        stop(glue::glue("El tipo de datos declarado no es válido!"))
      }
      
      # Se seleccionan y re-ordenan las columnas de self$data
      self$data <- self$data %>%
        dplyr::select(longitude, latitude, init_time, year, month,
                      self$variable, anomaly, dplyr::everything())
      
    },
    
    interpolate_data = function(new_points, grid = FALSE) {
      # Verificar si es necesario interpolar. Ej: si los puntos en new_points
      # coinciden con los originales, entonces no es necesario interpolar
      data_must_be_interpolated <- self$data %>% 
        dplyr::select(longitude, latitude) %>% 
        dplyr::distinct() %>%
        dplyr::anti_join(new_points, by = c("longitude", "latitude")) %>%
        base::nrow() %>% base::as.logical()
      
      # Interpolate data only if it must be interpolated and 
      # if they were not previously interpolated 
      if ( data_must_be_interpolated && !private$pv_data_already_interpolated ) {
      
        # Define interpolation formula
        f_left_side <- glue::glue("{self$variable}")
        f_right_side <- "longitude + latitude"
        interp_formula <- as.formula(paste(f_left_side, "~", f_right_side))
        
        # Interpolate data
        self$data <- purrr::map_dfr(
          .x = unique(self$data$init_time),
          .f = function(c_time) {
            # Filtrar self$data, se usan solo las filas con init_time = c_time
            c_data <- self$data %>%
              dplyr::filter(init_time == c_time)
            # Interpolar datos de la fecha c_time
            c_interp <- metR::Interpolate(
              formula = interp_formula, 
              x.out = new_points$longitude, 
              y.out = new_points$latitude, 
              grid = grid, 
              data = c_data)
            # La interpolación retorna solo lon, lat y el valor interpolado, 
            # por la tanto, es necesario agregar al resultado las columnas
            # que representan el tiempo (init_time, year y month)
            c_interp <- tibble::as_tibble(c_interp) %>% 
              dplyr::mutate(init_time = c_time, 
                            year = unique(c_data$year),
                            month = unique(c_data$month)) %>%
              dplyr::select(longitude, latitude, init_time, year, month, 
                            dplyr::everything())
            # Retornar el dataframe interpolado
            return ( c_interp )
          })
      }
      
      # Marcar datos como interpolados
      private$pv_data_already_interpolated <- TRUE
      
      # Como R siempre retorna algo, por último se deja self$data
      self$data
    }
  ),
  private = list(
    pv_data_file = character(),
    pv_value_var = character(),
    pv_data_type = character(),
    pv_tgt_month = numeric(),
    
    pv_longitude_var = character(),
    pv_latitude_var = character(),
    pv_time_var = character(),
    
    pv_correction_strategy = CorrectionStrategy,
    
    pv_data_already_interpolated = logical(),
    
    check_inputs = function(data_file, data_variable, target_months) {
      if ( is.vector(data_file) | is.list(data_file) ) {
        for (f in data_file)
          stopifnot(is.character(f), length(f) == 1)
      } else {
        stopifnot(is.character(data_file), length(data_file) == 1)
      }
      stopifnot(is.character(data_variable), length(data_variable) == 1)
      if ( is.vector(target_months) | is.list(target_months) ) {
        for (m in target_months)
          stopifnot(is.numeric(m), length(f) == 1)
      } else {
        stopifnot(is.numeric(target_months), length(target_months) == 1)
      }
    },
    
    check_file_variables = function(longitude, latitude, time) {
      stopifnot(is.character(longitude), length(longitude) == 1)
      stopifnot(is.character(latitude), length(latitude) == 1)
      stopifnot(is.character(time), length(time) == 1)
    },
    
    check_correction_strategy = function(correction_strategy) {
      if ( !is.null(correction_strategy) )
        stopifnot("CorrectionStrategy" %in% class(correction_strategy))
    },
    
    load_data = function() {
      
      self$data <- purrr::map_dfr(
        .x = private$pv_data_file,
        .f = function(current_file) {
          
          # Extraer datos del netcdf
          current_file_data <- tidync::tidync(current_file) %>% 
            tidync::hyper_tibble(na.rm = FALSE)
          
          # Definir nuevos nombres para las columnas
          lookup <- c()
          lookup[["longitude"]] <- private$pv_longitude_var
          lookup[["latitude"]] <- private$pv_latitude_var
          lookup[["init_time"]] <- private$pv_time_var
          lookup[[self$variable]] <- private$pv_value_var
          lookup <- unlist(
            lookup, recursive = TRUE, use.names = TRUE)
          # Renombrar y Seleccionar columnas
          current_file_data <- current_file_data %>%
            dplyr::rename(dplyr::any_of(lookup)) %>%
            dplyr::select(dplyr::any_of(names(lookup)))
          
          # Corregir longitud
          current_file_data <- current_file_data %>%
            dplyr::mutate(
              longitude = ifelse(longitude > 180, longitude - 360, longitude))
          
          # Extraer correctores de fecha
          tc <- TimeHelper$extract_netcdf_time_correctors(
            file_name = current_file, time_variable = private$pv_time_var)
          
          # Corregir la fecha
          current_file_data <- current_file_data %>%
            dplyr::mutate(
              init_time = tc$start_time + tc$compute_delta(init_time)) %>%
            dplyr::mutate(
              init_time = as.Date(init_time))
          
          # Agregar el año y mes objetivo del pronóstico
          # OBS: el año no siempre es el de la fecha de inicio, puede ocurrir
          #      que la fecha de inicio sea diciembre, y el primer mes objetivo
          #      sea enero, en ese caso, el año del primer mes objetivo es el
          #      año siguiente al año de la fecha de inicio.
          current_file_data <- current_file_data %>%
            dplyr::mutate(
              year = ifelse(lubridate::month(init_time) > private$pv_tgt_month[1],
                            lubridate::year(init_time) + 1, 
                            lubridate::year(init_time)),
              month = paste(private$pv_tgt_month, collapse = ','))
          
          # Reordenar columnas
          current_file_data <- current_file_data %>%
            dplyr::select(longitude, latitude, init_time, year, month,
                          dplyr::everything())
          
          # Aplicar estrategia de corrección, si existe una
          if ( !is.null(private$pv_correction_strategy) )
            current_file_data <- private$pv_correction_strategy$apply_correction(
              data_to_correct = current_file_data)
          
          # Retornar datos del archivo  
          return ( current_file_data )
        }
      )
          
    }
  ),
  active = list(
    type = function() {
      return ( forecast_type )  
    },
    variable = function() {
      stop("I'm an abstract interface method")
    },
    data_file = function() {
      return ( private$pv_data_file )  
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE DETERMINISTIC PRODUCT FOR TEMPERATURE
FcstTempDeterministicData <- R6::R6Class(
  classname = "FcstTempDeterministicData",
  active = list(
    variable = function() {
      return ( "t2m" )
    }
  ),
  inherit = FcstDeterministicData,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE DETERMINISTIC PRODUCT FOR PRECIPITATION
FcstPrcpDeterministicData <- R6::R6Class(
  classname = "FcstPrcpDeterministicData",
  active = list(
    variable = function() {
      return ( "prcp" )
    }
  ),
  inherit = FcstDeterministicData,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)


# ABSTRACT PROBABILISTC PRODUCTS
FcstProbabilisticData <- R6::R6Class(
  classname = "FcstProbabilisticData",
  public = list(
    data = NULL,
    initialize = function(data_file, data_variable, target_months,
                          longitude = "longitude", latitude = "latitude", 
                          time = "init_time", categories_variable = "category", 
                          cat_below = "below", cat_normal = "normal", 
                          cat_above = "above", correction_strategy = NULL) {
      
      private$check_inputs(data_file, data_variable, target_months)
      private$pv_data_file <- data_file
      private$pv_value_var <- data_variable
      private$pv_tgt_month <- target_months
      
      private$check_file_variables(longitude, latitude, time, categories_variable, 
                                   cat_below, cat_normal, cat_above)
      private$pv_longitude_var <- longitude
      private$pv_latitude_var <- latitude
      private$pv_time_var <- time
      private$pv_categories_var <- categories_variable
      private$pv_cat_below_var <- cat_below
      private$pv_cat_normal_var <- cat_normal
      private$pv_cat_above_var <- cat_above
      
      private$pv_below_col <- glue::glue("prob_{self$variable}_below")
      private$pv_normal_col <- glue::glue("prob_{self$variable}_normal")
      private$pv_above_col <- glue::glue("prob_{self$variable}_above")
      
      private$check_correction_strategy(correction_strategy)
      private$pv_correction_strategy <- correction_strategy
      
      private$pv_data_already_interpolated <- FALSE
      private$load_data()
    },
    
    reload_data = function() {
      private$pv_data_already_interpolated <- FALSE
      private$load_data()
    },
    
    compute_prob_from_det_and_obs_data = function(det_data_df, obs_data_df, 
                                                  lower_training_year = NULL,
                                                  higher_training_year = NULL,
                                                  suffix = "") {
      
      # Definir y seleccionar las columnas que debe tener det_data_df
      det_data_cols <- c("longitude", "latitude", "init_time", "year", "month", self$variable)
      det_data_df <- det_data_df %>% dplyr::select(dplyr::any_of(det_data_cols)) 
      
      # Si det_data_df no tiene las columnas apropiadas, no se puede seguir
      if ( ! all( det_data_cols %in% colnames(det_data_df) ) ) {
        warning("El df con datos pronosticados no tiene las columnas esperadas")
        return ( invisible(NULL) )
      }
      
      # Definir seleccionar las columnas que debe tener obs_data_df
      obs_data_cols <- c("longitude", "latitude", "year", "month", self$variable)
      obs_data_df <- obs_data_df %>% dplyr::select(dplyr::any_of(obs_data_cols)) 
      
      # Si obs_data_df no tiene las columnas apropiadas, no se puede seguir
      if ( ! all( obs_data_cols %in% colnames(obs_data_df) ) ) {
        warning("El df con datos obsevardos no tiene las columnas esperadas")
        return ( invisible(NULL) )
      }
      
      # Si las grillas no son iguales, entonces no se puede seguir
      if ( !are_points_joinables(det_data_df, obs_data_df) ) {
        warning("El df con datos obsevardos y el df con datos ",
                "pronosticados no tienen la misma grilla")
        return ( invisible(NULL) )
      }
      
      #
      #
      
      # Definir nombre de las columnas luego del join
      det_col <- glue:::glue("{self$variable}.det")
      obs_col <- glue:::glue("{self$variable}.obs")
      
      # Calcular media, varianza, correlación. OJO: la media, la varianza y la 
      # correlación se calculan utilizando solo los datos observados y generados 
      # en el periodo de entrenamiento!!!
      data_obs_statistics <- det_data_df %>%
        dplyr::left_join(
          obs_data_df, by = c("longitude", "latitude", "year", "month"), 
          suffix = c('.det', '.obs')
        ) %>%
        # Se excluyen los años que no deben ser usados para el cálculo 
        # de las estadísticas (si fueron definidos)
        { if ( !is.null(lower_training_year) ) 
            dplyr::filter(., year >= lower_training_year) 
          else . } %>%
        { if ( !is.null(higher_training_year) ) 
            dplyr::filter(., year <= higher_training_year) 
          else . } %>%
        dplyr::mutate(
          !!obs_col := round(!!as.name(obs_col), 1)  
        ) %>%
        dplyr::group_by(longitude, latitude) %>%
        dplyr::summarise(
          mean = mean(!!as.name(obs_col), na.rm = TRUE), 
          var = stats::var(!!as.name(obs_col), na.rm = TRUE), 
          corr = stats::cor(!!as.name(det_col), !!as.name(obs_col),
                            use = "na.or.complete"), 
          .groups = 'drop'
        ) %>%
        dplyr::ungroup()
      
      # Definir columnas para probabilidades
      below_col <- paste0(private$pv_below_col, suffix)
      normal_col <- paste0(private$pv_normal_col, suffix)
      above_col <- paste0(private$pv_above_col, suffix)
      
      # Se agregan, a los datos generados, la media, la varianza y la correlación 
      # calculados en el paso previo. Luego se calculan las anomalias (anom), las 
      # previsiones normalizadas (prev_norm), la distancia de la previsión en
      # relación a la distribución observada (dp) y las probabilidades de que la
      # precipitación sea menor, igual o mayor a la media.
      resultado <- det_data_df %>%
        dplyr::left_join(data_obs_statistics, by = c('longitude', 'latitude')) %>%
        dplyr::group_by(longitude, latitude) %>%
        dplyr::mutate(
          prev_norm = (!!as.name(self$variable) - mean) / sqrt(var)
        ) %>% 
        dplyr::ungroup() %>%
        dplyr::mutate(
          dp = sqrt(1 - corr^2)  # distancia de la previsión en relación a la distribución observada
        ) %>%
        dplyr::mutate(
          !!below_col := round(stats::pnorm(-0.4303, prev_norm, dp), 2)
        ) %>% 
        dplyr::mutate(
          !!normal_col := round(stats::pnorm(0.4303, prev_norm, dp) - !!as.name(below_col), 2)
        ) %>% 
        dplyr::mutate(
          !!above_col := 1 - !!as.name(below_col) - !!as.name(normal_col)
        ) %>%
        dplyr::select(-mean, -var, -corr, -prev_norm, -dp, -self$variable)
      
      # Retornar el resultado
      return ( resultado )
      
    },
    
    interpolate_data = function(new_points, grid = FALSE) {
      # Verificar si es necesario interpolar. Ej: si los puntos en new_points
      # coinciden con los originales, entonces no es necesario interpolar
      data_must_be_interpolated <- self$data %>% 
        dplyr::select(longitude, latitude) %>% 
        dplyr::distinct() %>%
        dplyr::anti_join(new_points, by = c("longitude", "latitude")) %>%
        base::nrow() %>% base::as.logical()
      
      # Interpolate data only if it must be interpolated and 
      # if they were not previously interpolated 
      if ( data_must_be_interpolated && !private$pv_data_already_interpolated ) {
        
        # Define interpolation formula
        f_left_side <- glue::glue("{private$pv_below_col} | ",
                                  "{private$pv_normal_col} | ",
                                  "{private$pv_above_col}")
        f_right_side <- "longitude + latitude"
        interp_formula <- as.formula(paste(f_left_side, "~", f_right_side))
        
        # Interpolate data
        self$data <- purrr::map_dfr(
          .x = unique(self$data$init_time),
          .f = function(c_time) {
            # Filtrar self$data, se usan solo las filas con init_time = c_time
            c_data <- self$data %>% 
              dplyr::filter(init_time == c_time)
            # Interpolar datos de la fecha c_time
            c_interp <- metR::Interpolate(
              formula = interp_formula, 
              x.out = new_points$longitude, 
              y.out = new_points$latitude, 
              grid = grid, 
              data = c_data)
            # La interpolación retorna solo lon, lat y los valores interpolados, 
            # por la tanto, es necesario agregar al resultado las columnas que
            # representan el tiempo (init_time, year y month)
            c_interp <- tibble::as_tibble(c_interp) %>% 
              dplyr::mutate(init_time = c_time, 
                            year = unique(c_data$year),
                            month = unique(c_data$month)) %>%
              dplyr::select(longitude, latitude, init_time, year, month, 
                            dplyr::everything())
            # Para estar seguros que las probabilidades interpoladas sumen 1,
            # la probabilidad normal se recalcula como 1 - above - bellow
            c_interp <- c_interp %>%
              dplyr::mutate(
                !!as.name(private$pv_normal_col) := 
                  1 - !!as.name(private$pv_below_col) - !!as.name(private$pv_above_col)
              )
            # Retornar el dataframe interpolado
            return ( c_interp )
          })
      }
      
      # Marcar datos como interpolados
      private$pv_data_already_interpolated <- TRUE
      
      # Como R siempre retorna algo, por último se deja self$data
      self$data
    },
    
    add_categories = function(prob_data_df = NULL, 
                              below_col = NULL,
                              normal_col = NULL, 
                              above_col = NULL) {
      # Algunas veces se ejcuta esté método como método de clase
      if ( is.null(prob_data_df) ) {
        prob_data_df <- self$data
        below_col <- private$pv_below_col
        normal_col <- private$pv_normal_col
        above_col <- private$pv_above_col
      }
      # Definir la categoría de cada fila en prob_data_df
      prob_data_df <- prob_data_df %>%
        dplyr::mutate(category = dplyr::case_when(
          (!!as.name(above_col) > 
             !!as.name(normal_col) & 
             !!as.name(above_col) > 
             !!as.name(below_col)) ~ "above",
          (!!as.name(normal_col) > 
             !!as.name(above_col) & 
             !!as.name(normal_col) > 
             !!as.name(below_col)) ~ "normal",
          (!!as.name(below_col) > 
             !!as.name(normal_col) & 
             !!as.name(below_col) > 
             !!as.name(above_col)) ~ "below",
          ((!!as.name(normal_col) > 
             !!as.name(above_col) & 
             !!as.name(normal_col) == 
             !!as.name(below_col)) | 
           (!!as.name(normal_col) == 
             !!as.name(above_col) & 
             !!as.name(normal_col) > 
             !!as.name(below_col)) |
           (!!as.name(normal_col) == 
             !!as.name(above_col) & 
             !!as.name(normal_col) ==
             !!as.name(below_col))) ~ "normal",
          (!!as.name(below_col) > 
             !!as.name(normal_col) & 
             !!as.name(below_col) == 
             !!as.name(above_col)) ~ NA_character_,
          TRUE ~ NA_character_
        )) %>%
        dplyr::mutate(
          category = factor(category, levels = c("below", "normal", "above"))
        )
      # Reportar casos en los que no se pudo determinar la categoría
      casos_con_below_y_above_superiores <- prob_data_df %>% 
        dplyr::mutate(category = dplyr::case_when(
          (!!as.name(below_col) > 
             !!as.name(normal_col) & 
             !!as.name(below_col) == 
             !!as.name(above_col)) ~ 1,
          TRUE ~ 0
        )) %>% dplyr::pull(category) %>% sum()
      if ( casos_con_below_y_above_superiores > 0 )
        warning("Hay casos en los que below y above son las categorías ",
                "con mayor probabilidad de ocurrencia y, por lo tanto, ",
                "no fue posible determinar la categoría para el evento.")
      
      # Asignar valor a self$data solo si prob_data_df es NULL
      if ( is.null(prob_data_df) ) {
        self$data <- prob_data_df
      }
      
      # Retornar resultado
      return ( prob_data_df )
    }
  ),
  private = list(
    pv_data_file = character(),
    pv_value_var = character(),
    pv_tgt_month = numeric(),
    
    pv_longitude_var = character(),
    pv_latitude_var = character(),
    pv_time_var = character(),
    pv_categories_var = character(),
    pv_cat_below_var = character(),
    pv_cat_normal_var = character(),
    pv_cat_above_var = character(),
    
    pv_below_col = character(),
    pv_normal_col = character(),
    pv_above_col = character(),
    
    pv_correction_strategy = CorrectionStrategy,
    
    pv_data_already_interpolated = logical(),
    
    check_inputs = function(data_file, data_variable, target_months) {
      if ( is.vector(data_file) | is.list(data_file) ) {
        for (f in data_file)
          stopifnot(is.character(f), length(f) == 1)
      } else {
        stopifnot(is.character(data_file), length(data_file) == 1)
      }
      stopifnot(is.character(data_variable), length(data_variable) == 1)
      if ( is.vector(target_months) | is.list(target_months) ) {
        for (m in target_months)
          stopifnot(is.numeric(m), length(f) == 1)
      } else {
        stopifnot(is.numeric(target_months), length(target_months) == 1)
      }
    },
    
    check_file_variables = function(longitude, latitude, time, categories_variable, 
                                    cat_below, cat_normal, cat_above) {
      stopifnot(is.character(longitude), length(longitude) == 1)
      stopifnot(is.character(latitude), length(latitude) == 1)
      stopifnot(is.character(time), length(time) == 1)
      stopifnot(is.character(categories_variable), length(categories_variable) == 1)
      stopifnot(is.character(cat_below), length(cat_below) == 1)
      stopifnot(is.character(cat_normal), length(cat_normal) == 1)
      stopifnot(is.character(cat_above), length(cat_above) == 1)
    },
    
    check_correction_strategy = function(correction_strategy) {
      if ( !is.null(correction_strategy) )
        stopifnot("CorrectionStrategy" %in% class(correction_strategy))
    },
    
    load_data = function() {
      
      self$data <- purrr::map_dfr(
        .x = private$pv_data_file,
        .f = function(current_file) {
          
          # Extraer datos del netcdf
          current_file_data <- tidync::tidync(current_file) %>% 
            tidync::hyper_tibble(na.rm = FALSE)
          
          # Definir nuevos nombres para las columnas
          lookup <- c()
          lookup[["longitude"]] <- private$pv_longitude_var
          lookup[["latitude"]] <- private$pv_latitude_var
          lookup[["init_time"]] <- private$pv_time_var
          lookup[["category"]] <- private$pv_categories_var
          lookup[["value"]] <- private$pv_value_var
          lookup <- unlist(
            lookup, recursive = TRUE, use.names = TRUE)
          # Renombrar y Seleccionar columnas
          current_file_data <- current_file_data %>%
            dplyr::rename(dplyr::any_of(lookup)) %>%
            dplyr::select(dplyr::any_of(names(lookup)))
          
          # Corregir longitud
          current_file_data <- current_file_data %>%
            dplyr::mutate(
              longitude = ifelse(longitude > 180, longitude - 360, longitude))
          
          # Extraer correctores de fecha
          tc <- TimeHelper$extract_netcdf_time_correctors(
              file_name = current_file, time_variable = private$pv_time_var)
          
          # Corregir la fecha
          current_file_data <- current_file_data %>%
            dplyr::mutate(
              init_time = tc$start_time + tc$compute_delta(init_time)) %>%
            dplyr::mutate(
              init_time = as.Date(init_time))
          
          # Agregar el año y mes objetivo del pronóstico
          # OBS: el año no siempre es el de la fecha de inicio, puede ocurrir
          #      que la fecha de inicio sea diciembre, y el primer mes objetivo
          #      sea enero, en ese caso, el año del primer mes objetivo es el
          #      año siguiente al año de la fecha de inicio.
          current_file_data <- current_file_data %>%
            dplyr::mutate(
              year = ifelse(lubridate::month(init_time) > private$pv_tgt_month[1],
                            lubridate::year(init_time) + 1, 
                            lubridate::year(init_time)),
              month = paste(private$pv_tgt_month, collapse = ','))
          
          # Reordenar columnas
          current_file_data <- current_file_data %>%
            dplyr::select(longitude, latitude, init_time, year, month,
                          dplyr::everything())
          
          # Pivotar el dataframe para que la categorías estén en columnas
          current_file_data <- current_file_data %>%
            tidyr::pivot_wider(names_from = category, values_from = value) 
          
          # Renombrar las nuevas columnas con las categorías
          current_file_data <- current_file_data %>%
            dplyr::rename(
              !!private$pv_below_col := !!as.name(private$pv_cat_below_var), 
              !!private$pv_normal_col := !!as.name(private$pv_cat_normal_var), 
              !!private$pv_above_col := !!as.name(private$pv_cat_above_var))
          
          # Aplicar estrategia de corrección, si existe una
          if ( !is.null(private$pv_correction_strategy) )
            current_file_data <- private$pv_correction_strategy$apply_correction(
              data_to_correct = current_file_data)
          
          # Retornar datos del archivo  
          return ( current_file_data )
        }
      )

    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
# Declare class methods
FcstProbabilisticData$add_categories = function(...) {
  FcstProbabilisticData$public_methods$add_categories(...)
}

# CONCRETE PROBABILISTC PRODUCT FOR TEMPERATURE
FcstTempProbabilisticData <- R6::R6Class(
  classname = "FcstTempProbabilisticData",
  active = list(
    variable = function() {
      return ( "t2m" )
    }
  ),
  inherit = FcstProbabilisticData,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE PROBABILISTC PRODUCT FOR PRECIPITATION
FcstPrcpProbabilisticData <- R6::R6Class(
  classname = "FcstPrcpProbabilisticData",
  active = list(
    variable = function() {
      return ( "prcp" )
    }
  ),
  inherit = FcstProbabilisticData,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)


