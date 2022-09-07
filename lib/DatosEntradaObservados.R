
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "glue", "metR", "lubridate", "stats", "stringr",
              "tibble", "purrr", "tidync", "here")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))

# Cargar helpers
source(here::here("lib", "Helpers.R"), echo = FALSE, chdir = TRUE)

# Cargar estrategias de corrección
source(here::here("lib", "EstrategiasDeCorreccion.R"), echo = FALSE, chdir = TRUE)


# The type for forecast data
observed_type = "observed_data"


# Implementation of Abstract Factory Pattern 
# Objective is to model input data!!


# ABSTRACT FACTORY
ObsDataAbsFactory <- R6::R6Class(
  classname = "ObsDataAbsFactory",
  public = list(
    create_det_obj = function(...) stop("I'm an abstract interface method")
  ),
  active = list(
    type = function() {
      return ( observed_type )  
    },
    variable = function() {
      stop("I'm an abstract interface method")
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE FACTORY 
ObsDataFactory <- R6::R6Class(
  classname = "ObsDataFactory",
  public = list(
    initialize = function(variable) {
      private$check_inputs(variable)
      private$pv_variable <- variable
    },
    
    create_det_obj = function(...) {
      if (private$pv_variable == "t2m") {
        return ( ObsTempDeterministicData$new(...) )
      } else if (private$pv_variable == "prcp") {
        return ( ObsPrcpDeterministicData$new(...) )
      } else {  stop("Invalid variable.") }
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
    variable = function() {
      return ( private$pv_variable )
    }
  ),
  inherit = ObsDataAbsFactory,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)



# ABSTRACT DETERMINISTIC PRODUCTS
ObsDeterministicData <- R6::R6Class(
  classname = "ObsDeterministicData",
  public = list(
    data = NULL,
    initialize = function(data_file, data_variable, bbox = NULL,
                          longitude = "longitude", latitude = "latitude", 
                          time = "time", correction_strategy = NULL) {
      
      private$check_inputs(data_file, data_variable)
      private$pv_data_file <- data_file
      private$pv_value_var <- data_variable
      
      private$check_bounding_box(bbox)
      private$pv_bbox <- bbox
      
      private$check_file_variables(longitude, latitude, time)
      private$pv_longitude_var <- longitude
      private$pv_latitude_var <- latitude
      private$pv_time_var <- time
      
      private$check_correction_strategy(correction_strategy)
      private$pv_correction_strategy <- correction_strategy
      
      private$pv_data_already_interpolated <- FALSE
      private$pv_data_already_grouped <- FALSE
      
      private$load_data()
    },
    
    reload_data = function() {
      private$pv_data_already_interpolated <- FALSE
      private$pv_data_already_grouped <- FALSE

      private$load_data()
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
        
        # Si se interpola antes de agrupar, entonces no hay una columna
        # "time" en self$data, sino que una columna "year". Sin embargo, 
        # si se interpola despues de agrupar ocurre lo contrario.
        time_col <- ifelse("time" %in% colnames(self$data), "time", "year")
        # Una vez identificada la columna con el tiempo, se interpola
        self$data <- purrr::map_dfr(
          .x = unique(self$data[[time_col]]),
          .f = function(c_time) {
            # Filtrar self$data, se usan solo las filas con fecha c_time
            c_data <- self$data %>%
              dplyr::filter(!!as.name(time_col) == c_time)
            # Interpolar datos de la fecha c_time
            c_interp <- metR::Interpolate(
              formula = interp_formula, 
              x.out = new_points$longitude, 
              y.out = new_points$latitude, 
              grid = grid, 
              data = c_data)
            # La interpolación retorna solo lon, lat y el valor interpolado,
            # por la tanto, es necesario agregar al resultado las columnas
            # que representan el tiempo (time o year y month)
            c_interp <- tibble::as_tibble(c_interp) %>% 
              # aquí se agrega la columna time o la columna year, según 
              # cual haya sido utilizada para filtrar los datos
              dplyr::mutate(!!time_col := c_time) %>%
              # aquí se agrega la coluna month, pero solo si la columna 
              # month está entre los datos filtrados
              { if ( "month" %in% colnames(c_data) )
                  dplyr::mutate(., month = unique(c_data$month))
                else . } %>%
              # aquí se seleccionan las columnas a retornar
              dplyr::select(longitude, latitude, 
                            dplyr::any_of(c("time", "year", "month")), 
                            self$variable, dplyr::any_of(c("mean")), 
                            dplyr::everything())
            # Retornar el dataframe interpolado
            return ( c_interp )
          })
      }
      
      # Marcar datos como interpolados
      private$pv_data_already_interpolated <- TRUE
      
      # Como R siempre retorna algo, por último se deja self$data
      self$data
    },

    group_data_by_months = function(months) {
      stop("I'm an abstract interface method")
    },
    
    add_anomalies = function(lower_year_to_use = NULL, 
                             higher_year_to_use = NULL) {
      # Calcular media (promedio) para cada coordenada
      promedios <- self$data %>%
        # Si los datos aún no fueron agrupados no hay una columna year
        # y hay que agregarla usando la columna time para ello!
        { if ( !private$pv_data_already_grouped ) 
          dplyr::mutate(., year = lubridate::year(time)) 
          else . } %>%
        # Se excluyen los años que no deben ser usados para el cálculo 
        # de los promedios (si fueron definidos)
        { if ( !is.null(lower_year_to_use) ) 
          dplyr::filter(., year >= lower_year_to_use) 
          else . } %>%
        { if ( !is.null(higher_year_to_use) ) 
          dplyr::filter(., year <= higher_year_to_use) 
          else . } %>%
        # Se agrupan los datos por longitud y latitud
        dplyr::group_by(longitude, latitude) %>%
        # Se calculan los promedios
        dplyr::summarise(
          mean = mean(!!as.name(self$variable), na.rm = T), 
          .groups = 'drop') %>%
        # Des-agrupar datos
        dplyr::ungroup()
      
      # Si self$data ya tiene la columna anomaly, entonces ya se calculó el
      # promedio antes, por lo tanto, para agregar la nueva columna anomaly
      # es necesario eliminar la vieja del df. Si esto no se hace, como las
      # columnas existen en ambos df, el join agrega sufijos.
      if ( "anomaly" %in% colnames(self$data) )
        self$data <- self$data %>% 
          dplyr::select(-anomaly)
      
      # Agregar los promedios al self$data
      self$data <- self$data %>% 
        dplyr::left_join(promedios, by = c("longitude", "latitude")) %>%
        dplyr::mutate(anomaly = !!as.name(self$variable) - mean) %>%
        dplyr::select(-mean)
      
      # Seleccionar y reordenar las columnas
      self$data <- self$data %>% 
        dplyr::select(longitude, latitude, 
                      dplyr::any_of(c("time", "year", "month")), 
                      self$variable, anomaly, dplyr::everything())
    },
    
    add_categories = function(lower_year_to_use = NULL, 
                              higher_year_to_use = NULL) {
      # Calcular el percentil p33 y p66 para par longitud latitude
      percentiles <- self$data %>%
        # Si los datos aún no fueron agrupados no hay una columna year
        # y hay que agregarla usando la columna time para ello!
        { if ( !private$pv_data_already_grouped ) 
            dplyr::mutate(., year = lubridate::year(time)) 
          else . } %>%
        # Se excluyen los años que no deben ser usados para el cálculo 
        # de los percentiles (si fueron definidos)
        { if ( !is.null(lower_year_to_use) ) 
          dplyr::filter(., year >= lower_year_to_use) 
          else . } %>%
        { if ( !is.null(higher_year_to_use) ) 
          dplyr::filter(., year <= higher_year_to_use) 
          else . } %>%
        # Se agrupan los datos por longitud y latitud
        dplyr::group_by(longitude, latitude) %>%
        # Se calculan los percentiles
        dplyr::summarise(
          p33.3 = stats::quantile(!!as.name(self$variable), 1/3, na.rm = T),
          p66.6 = stats::quantile(!!as.name(self$variable), 2/3, na.rm = T), 
          .groups = 'drop'
        ) %>%
        # Des-agrupar datos
        dplyr::ungroup()
      
      # Si self$data ya tiene las columnas p33.3 y p66.6, entonces ya se 
      # agregaron las categorías antes, por lo tanto, para agregar las nuevas
      # categorías es necesario eliminar la viejas del df. Si esto no se hace, 
      # como las columnas existen en amboas df, el join agrega sufijos.
      if ( all(c("p33.3", "p66.6") %in% colnames(self$data)) )
        self$data <- self$data %>% 
          dplyr::select(-p33.3, -p66.6, -category, -event_below, 
                        -event_normal, -event_above)
      
      # Definir la categoría de cada fila en self$data
      self$data <- self$data %>% 
        dplyr::left_join(percentiles, by = c("longitude", "latitude")) %>%
        dplyr::mutate(
          category = dplyr::case_when(
            !!as.name(self$variable) < p33.3 ~ "below",
            !!as.name(self$variable) >= p33.3 & 
              !!as.name(self$variable) <= p66.6 ~ "normal",
            !!as.name(self$variable) > p66.6 ~ "above",
            TRUE ~ NA_character_ )) %>%
        dplyr::mutate(
          category = factor(category, levels = c("below", "normal", "above"))
        )
    }
  ),
  private = list(
    pv_data_file = character(),
    pv_value_var = character(),
    pv_bbox = list(nla = numeric(), sla = numeric(), 
                   wlo = numeric(), elo = numeric()),
    
    pv_longitude_var = character(),
    pv_latitude_var = character(),
    pv_time_var = character(),
    
    pv_correction_strategy = CorrectionStrategy,
    
    pv_data_already_interpolated = logical(),
    pv_data_already_grouped = logical(),
    
    check_inputs = function(data_file, data_variable) {
      stopifnot(is.character(data_file), length(data_file) == 1)
      stopifnot(is.character(data_variable), length(data_variable) == 1)
    },
    
    check_bounding_box = function(bbox) {
      if ( !is.null(bbox) ) {
        stopifnot(is.list(bbox) | is.vector(bbox), length(bbox) == 4)
        stopifnot(all(c('nla', 'sla', 'wlo', 'elo') %in% names(bbox)))
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
          lookup[["time"]] <- private$pv_time_var
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
          
          # Excluir coordenadas fuera del bounding box
          if ( !is.null(private$pv_bbox) )
            current_file_data <- current_file_data %>%
              dplyr::filter(
                latitude <= private$pv_bbox$nla,
                latitude >= private$pv_bbox$sla,
                longitude >= private$pv_bbox$wlo,
                longitude <= private$pv_bbox$elo)
          
          # Extraer correctores de fecha
          tc <- TimeHelper$extract_netcdf_time_correctors(
            file_name = current_file, time_variable = private$pv_time_var)
          
          # Corregir la fecha
          current_file_data <- current_file_data %>%
            dplyr::mutate(time = tc$start_time + tc$compute_delta(time)) %>%
            dplyr::mutate(time = as.Date(time))
          
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
      return ( observed_type )  
    },
    variable = function() {
      stop("I'm an abstract interface method")
    },
    data_file = function() {
      return ( private$pv_data_file )  
    },
    bbox = function() {
      return ( private$pv_bbox )
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE DETERMINISTIC PRODUCT FOR TEMPERATURE
ObsTempDeterministicData <- R6::R6Class(
  classname = "ObsTempDeterministicData",
  public = list(
    group_data_by_months = function(months) {
      # Verificar que el/los mes/es es/sean correcto/s
      MonthsHelper$check_months(months)
      # Detectar cambio de año en los meses
      cambio_de_anho <- MonthsHelper$detect_year_change(months)
      # Agrupar solo si los datos no fueron agrupados antes
      if ( !private$pv_data_already_grouped )
        self$data <- self$data %>%
          # agregar mes y año como columnas
          dplyr::mutate(
            year = lubridate::year(time), 
            month = lubridate::month(time)) %>%
          # dejar solo los meses a agrupar
          dplyr::filter(month %in% months) %>%
          # Agregar marca para agrupar, si se usa solo el año los trimetres que 
          # cambien de año (ej: 12,1,2) van a ser agrupados de manera errónea 
          # (ej: 12-2021, 1-2021, 2-2021 en lugar de 12-2021, 1-2022, 2-2022).
          # Como ID de cada grupo se usa el año del primero de los meses de la
          # lista de meses a agrupar (ej: para el trimestre 10,11,1 el año 
          # utilizado com ID es el asociado al mes 10, que además es el menor
          # de los años cubiertos por el trimestre). Entonces, si se detecta 
          # un cambio año, todos los meses entre 1 (enero) y 12 (diciembre) 
          # menos la cantidad de meses a grupar, es decir, 12 - length(months)), 
          # deben usar como ID el año del registro menos 1 (porque están en el 
          # segundo de los dos años cubiertos por los meses a agrupar). Ej: si 
          # la cantidad de meses a agrupar es 3, el primero de los grupos de 3
          # meses, con cambio de año, es 11,12,1; por o tanto, todos los meses 
          # entre 1 (enero) y 10 (octubre) llevan como ID el primero de los 2 
          # años cubiertos por el listado de meses a agrupar, es decir, el año
          # indicado en el registro (fila) menos 1. 
          # PERO OJO: algunos archivos con datos observados para un trimestre,
          # como los archvos observados generados por CPT y EREG, tienen en el
          # archivo una sola entrada para el trimestre y no tres. En estos casos
          # si se filtran los datos forzando que los grupos tengan 3 entradas,
          # se van a excluír registros válidos, por lo tanto, cuando en el 
          # archivo haya solo un mes (aunque el archivo contenga datos de un
          # trimestre) se van a tomar los grupos con un solo elemento.
          dplyr::mutate(
            group_id = ifelse(
              test = cambio_de_anho & month >= 1 & month <= 12-length(months)+1, 
              yes = year-1, no = year)
          ) %>% 
          # eliminar años con meses incompletos 
          dplyr::group_by(longitude, latitude, group_id) %>%
          dplyr::mutate(n_months_per_year = n()) %>%
          # Antes se exigía que n_months_per_year == length(months), pero esto no 
          # siempre es correcto. Los archivos con datos observados trimestrales
          # generados por EREG y CPT tienen una sola entrada para el trimestre 
          # (en lugar de una por mes) y el mes de esas entradas es el primer mes 
          # del trimestre. Por lo tanto, un mejor filtro es obligar a que
          # n_months_per_year == month %>% unique() %>% length(), o sea, obligar
          # a que n_months_per_year sea igual a la cantidad de mese distintos
          # en el archivo mismo.
          dplyr::filter(n_months_per_year == month %>% unique() %>% length()) %>%
          # agrupar los meses indicados (por año) y promediarlos
          dplyr::summarise(
            !!self$variable := mean(!!as.name(self$variable), na.rm = T), 
            .groups = 'drop') %>%
          # desagrupar datos
          dplyr::ungroup() %>%
          # agregar meses
          dplyr::mutate(
            month = paste(months, collapse =  ','),
            year = group_id) %>%
          # seleccionar columnas a retornar
          dplyr::select(longitude, latitude, year, month, self$variable)
      
      # Marcar datos como agrupados
      private$pv_data_already_grouped <- TRUE
      
      # Como R siempre retorna algo, por último se deja self$data
      self$data
    }
  ),
  active = list(
    variable = function() {
      return ( "t2m" )
    }
  ),
  inherit = ObsDeterministicData,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

# CONCRETE DETERMINISTIC PRODUCT FOR PRECIPITATION
ObsPrcpDeterministicData <- R6::R6Class(
  classname = "ObsPrcpDeterministicData",
  public = list(
    group_data_by_months = function(months) {
      # Verificar que el/los mes/es es/sean correcto/s
      MonthsHelper$check_months(months)
      # Detectar cambio de año en los meses
      cambio_de_anho <- MonthsHelper$detect_year_change(months)
      # Agrupar solo si los datos no fueron agrupados antes
      if ( !private$pv_data_already_grouped )
        self$data <- self$data %>%
        # agregar mes y año como columnas
        dplyr::mutate(
          year = lubridate::year(time), 
          month = lubridate::month(time)) %>%
        # dejar solo los meses a agrupar
        dplyr::filter(month %in% months) %>%
        # Agregar marca para agrupar, si se usa solo el año los trimetres que 
        # cambien de año (ej: 12,1,2) van a ser agrupados de manera errónea 
        # (ej: 12-2021, 1-2021, 2-2021 en lugar de 12-2021, 1-2022, 2-2022).
        # Como ID de cada grupo se usa el año del primero de los meses de la
        # lista de meses a agrupar (ej: para el trimestre 10,11,1 el año 
        # utilizado com ID es el asociado al mes 10, que además es el menor
        # de los años cubiertos por el trimestre). Entonces, si se detecta 
        # un cambio año, todos los meses entre 1 (enero) y 12 (diciembre) 
        # menos la cantidad de meses a grupar, es decir, 12 - length(months)), 
        # deben usar como ID el año del registro menos 1 (porque están en el 
        # segundo de los dos años cubiertos por los meses a agrupar). Ej: si 
        # la cantidad de meses a agrupar es 3, el primero de los grupos de 3
        # meses, con cambio de año, es 11,12,1; por o tanto, todos los meses 
        # entre 1 (enero) y 10 (octubre) llevan como ID el primero de los 2 
        # años cubiertos por el listado de meses a agrupar, es decir, el año
        # indicado en el registro (fila) menos 1. 
        # PERO OJO: algunos archivos con datos observados para un trimestre,
        # como los archvos observados generados por CPT y EREG, tienen en el
        # archivo una sola entrada para el trimestre y no tres. En estos casos
        # si se filtran los datos forzando que los grupos tengan 3 entradas,
        # se van a excluír registros válidos, por lo tanto, cuando en el 
        # archivo haya solo un mes (aunque el archivo contenga datos de un
        # trimestre) se van a tomar los grupos con un solo elemento.
        dplyr::mutate(
          group_id = ifelse(
            test = cambio_de_anho & month >= 1 & month <= 12-length(months)+1, 
            yes = year-1, no = year)
        ) %>% 
        # eliminar años con meses incompletos 
        dplyr::group_by(longitude, latitude, group_id) %>%
        dplyr::mutate(n_months_per_year = n()) %>%
        # Antes se exigía que n_months_per_year == length(months), pero esto no 
        # siempre es correcto. Los archivos con datos observados trimestrales
        # generados por EREG y CPT tienen una sola entrada para el trimestre 
        # (en lugar de una por mes) y el mes de esas entradas es el primer mes 
        # del trimestre. Por lo tanto, un mejor filtro es obligar a que
        # n_months_per_year == month %>% unique() %>% length(), o sea, obligar
        # a que n_months_per_year sea igual a la cantidad de mese distintos
        # en el archivo mismo.
        dplyr::filter(n_months_per_year == month %>% unique() %>% length()) %>%
        # agrupar los meses indicados (por año) y sumarlos
        dplyr::summarise(
          !!self$variable := sum(!!as.name(self$variable), na.rm = T), 
          .groups = 'drop') %>%
        # desagrupar datos
        dplyr::ungroup() %>%
        # agregar meses
        dplyr::mutate(
          month = paste(months, collapse =  ','),
          year = group_id) %>%
        # seleccionar columnas a retornar
        dplyr::select(longitude, latitude, year, month, self$variable)
      
      # Marcar datos como agrupados
      private$pv_data_already_grouped <- TRUE
      
      # Como R siempre retorna algo, por último se deja self$data
      self$data
    }
  ),
  active = list(
    variable = function() {
      return ( "prcp" )
    }
  ),
  inherit = ObsDeterministicData,
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)

