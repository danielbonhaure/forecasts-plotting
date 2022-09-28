
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "ncdf4", "lubridate", "stringr", "purrr")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))


#
#
#


# Crear una secuencia/rango circular
crange <- function(start, stop, modulo) {
  if (start > stop) {
    resp <- c(seq(start, modulo), seq(1, stop))
  } else {
    resp <- seq(start, stop)
  }
  resp <- purrr::map_dbl(resp, ~ ifelse(.x > modulo, .x %% modulo, .x))
  return (resp)
}

# Sumar valores de manera circular
csum <- function(x, y, modulo) {
  resp <- x + y
  if ( resp > modulo )
    resp <- resp %% modulo
  return(resp)
}


# Método que permite calcular las distancias entre elementos 
# adyacentes en un vector de longitudes o latitudes.
spatial_dist = function(x) {
  # Primero se ordena el vector de mayor a menor, esto es correcto
  # porque el vector solo puede tener longitudes o latitudes, y por 
  # porque el objetivo de la función es calcular distancias entre 
  # elementos espacialmente adyacentes.
  x <- sort(unique(x), decreasing = T)
  
  # Despues, a partir del vector ordenado, se crean dos vectores.
  # El 1ero toma desde el 1er elemento del vector hasta el anteúltimo.
  # El 2do toma desde el 2do elemento del vactor hasta el último.
  # Con esto se logra que el 1er vector tengo siempre un valor mayor
  # que el segundo vector. Finalmente, restanto estos dos vectores es
  # posible obtener la distancia entre elementos consecutivos del
  # vector recibido como parámetro originalmente.
  x_dist <- x[seq(1,length(x)-1)] - x[seq(2,length(x)-0)]
  
  # Se retornan las diferencias/distancias
  return ( x_dist )
}


are_points_gridded = function(points_df) {
  # Definir la diferencias entre las longitudes y latitudes
  x_diff <- spatial_dist(points_df$longitude)
  y_diff <- spatial_dist(points_df$latitude)
  
  # Determinar si los puntos forman o no una grilla regular
  return ( min(x_diff) == mean(x_diff) && mean(x_diff) == max(x_diff) && 
             min(y_diff) == mean(y_diff) && mean(y_diff) == max(y_diff) )
  
}


are_points_joinables = function(main_df, aux_df) {
  # Obtener puntos de ambos df
  coords_main_df <- 
    main_df %>% 
      dplyr::select(longitude, latitude) %>%
      dplyr::distinct()
  coords_aux_df <- 
    aux_df %>% 
      dplyr::select(longitude, latitude) %>%
      dplyr::distinct()
  
  # Unir los puntos cuando lon y lat coincidan
  common_coords <- 
    dplyr::inner_join(coords_main_df, coords_aux_df, 
                      by = c("longitude", "latitude"))
  
  # Si el número de coordenadas comunes difiere del número
  # de coordenadas en el df principal "main_df" entonces se 
  # concidera que los dfs no son compatibles.
  if ( nrow(coords_main_df) != nrow(common_coords) ) {
    return ( FALSE )
  }
  
  # Si en este punto no se detectó ninguna incopatiblidad
  # entonces se concsidera que los dfs son compatibles.
  return ( TRUE )
}


#
#
#


TimeHelper <- R6::R6Class(
  classname = "TimeHelper",
  public = list(
    extract_netcdf_time_correctors = function(file_name, time_variable) {
      # En los archivos netcdf, el tiempo se suele especificar de la
      # siguiente manera: "<unit> since <time>". Para obtener la fecha 
      # correcta a partir de esto, se debe agregar <unit> a <time>, es 
      # decir, fecha = <time> + <unit>. La variable <time> es la fecha de
      # origen, unit puede ser una cantidad de segundos, minutos, horas, 
      # dias, meses. Ej: "days since 1982-04-01 00:00:00"
      
      # Abrir archivo para extrar la unidad de medida del tiempo
      nc <- ncdf4::nc_open(file_name)
      
      # Extraer la unidad de medidad del tiempo
      time_units <- ncdf4::ncatt_get(nc, time_variable, "units")[["value"]]
      
      # Definir el delta que deber ser agregado al tiempo de origen
      if ( stringr::str_detect(time_units, "^seconds since") ){
        compute_delta <- lubridate::seconds
      } else if ( stringr::str_detect(time_units, "^minutes since") ){
        compute_delta <- lubridate::minutes
      } else if ( stringr::str_detect(time_units, "^hours since") ){
        compute_delta <- lubridate::hours
      } else if ( stringr::str_detect(time_units, "^days since") ) {
        compute_delta <- lubridate::days
      } else if ( stringr::str_detect(time_units, "^months since") ) {
        compute_delta <- lubridate::dmonths
      } else { stop("Invalid time units") }
      
      # Definir el tiempo de origen
      start_time <- lubridate::date(gsub("[^0-9.-]", "", time_units))
      
      # Cerrar archivo netcdf
      ncdf4::nc_close(nc)
      
      # Retornar correctores
      return ( list(start_time = start_time, compute_delta = compute_delta) )
      
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
# Declare class methods
TimeHelper$extract_netcdf_time_correctors = function(...) {
  TimeHelper$public_methods$extract_netcdf_time_correctors(...)
}


#
#
#


MonthsHelper <- R6::R6Class(
  classname = "MonthsHelper",
  public = list(
    check_months = function(months) {
      # Si alguno de los meses es mayor a 12 o menor a 1, no son meses válidos
      for ( m in months )
        if ( m < 1 | m > 12 )
          stop( "Some months are less than 1 or greater than 12!" )
      # Si los meses no son consecutivos, no son meses válidos
      if ( length(months) > 1 )
        for ( i in seq(length(months)-1) ) 
          if ( ! (months[i+1] - months[i] == 1 | (months[i] == 12 & months[i+1] == 1)) )
            stop( "Months are not consecutive!" )
    },
    detect_year_change = function(months) {
      # Se detecta un cambio de año cuando la diferencia entre un 
      # mes y el siguiente da como resultado un valor negativo
      if ( length(months) > 1 )
        for ( i in seq(length(months)-1) )
          if ( sign(months[i+1] - months[i]) == -1 )
            return ( TRUE )
      return ( FALSE )
    },
    get_leadtime = function(first_month, last_month) {
      if ( last_month >= first_month ) {
        return ( last_month - first_month )
      } else {
        return ( last_month - first_month + 12 )
      }
    },
    parse_trimesters = function(trimester) {
      if (trimester == 'JFM') {
        return ('1-3')
      } else if (trimester == 'FMA') {
        return ('2-4')
      } else if (trimester == 'MAM') {
        return ('3-5')
      } else if (trimester == 'AMJ') {
        return ('4-6')
      } else if (trimester == 'MJJ') {
        return ('5-7')
      } else if (trimester == 'JJA') {
        return ('6-8')
      } else if (trimester == 'JAS') {
        return ('7-9')
      } else if (trimester == 'ASO') {
        return ('8-10')
      } else if (trimester == 'SON') {
        return ('9-11')
      } else if (trimester == 'OND') {
        return ('10-12')
      } else if (trimester == 'NDJ') {
        return ('11-1')
      } else if (trimester == 'DJF') {
        return ('12-2')
      }
    },
    trimester_to_seq = function(trimester) {
      if (trimester == 'JFM') {
        return (crange(1, 3, 12))
      } else if (trimester == 'FMA') {
        return (crange(2, 4, 12))
      } else if (trimester == 'MAM') {
        return (crange(3, 5, 12))
      } else if (trimester == 'AMJ') {
        return (crange(4, 6, 12))
      } else if (trimester == 'MJJ') {
        return (crange(5, 7, 12))
      } else if (trimester == 'JJA') {
        return (crange(6, 8, 12))
      } else if (trimester == 'JAS') {
        return (crange(7, 9, 12))
      } else if (trimester == 'ASO') {
        return (crange(8, 10, 12))
      } else if (trimester == 'SON') {
        return (crange(9, 11, 12))
      } else if (trimester == 'OND') {
        return (crange(10, 12, 12))
      } else if (trimester == 'NDJ') {
        return (crange(11, 1, 12))
      } else if (trimester == 'DJF') {
        return (crange(12, 2, 12))
      }
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
# Declare class methods
MonthsHelper$trimesters = c('JFM', 'FMA', 'MAM', 'AMJ', 'MJJ', 'JJA', 
                            'JAS', 'ASO', 'SON', 'OND', 'NDJ', 'DJF')
MonthsHelper$check_months = function(...) {
  MonthsHelper$public_methods$check_months(...) 
}
MonthsHelper$detect_year_change = function(...) {
  MonthsHelper$public_methods$detect_year_change(...)
}
MonthsHelper$get_leadtime = function(...) {
  MonthsHelper$public_methods$get_leadtime(...)
}
MonthsHelper$parse_trimesters = function(...) {
  MonthsHelper$public_methods$parse_trimesters(...)
}
MonthsHelper$trimester_to_seq = function(...) {
  MonthsHelper$public_methods$trimester_to_seq(...)
}


#
#
#


DataTypeHelper <- R6::R6Class(
  classname = "DataTypeHelper",
  public = list(
    data_expressed_in_anomalies = function(file_name, data_variable) {
      # En los archivos netcdf, se guarda la unidad de medida de la
      # variable con datos. Si esta descripción contiene el string 
      # "anomaly" entonces el dato está expresado en anomalías.
      
      # Abrir archivo para extrar la unidad de medida del tiempo
      nc <- ncdf4::nc_open(file_name)
      
      # Extraer la unidad de medidad de la variable
      data_units <- ncdf4::ncatt_get(nc, data_variable, "units")[["value"]]
      
      # Cerrar archivo netcdf
      ncdf4::nc_close(nc)
      
      # Retornar el resultado de la sig expresión regular
      return ( stringr::str_detect(data_units, "anomaly") )
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
# Declare class methods
DataTypeHelper$data_expressed_in_anomalies = function(...) {
  DataTypeHelper$public_methods$data_expressed_in_anomalies(...) 
}


#
#
#


InterpolationHelper <- R6::R6Class(
  classname = "InterpolationHelper",
  public = list(
    interp_kriging = function(data_df, cols_to_interp, new_grid_sf) {
      ##### Kriging  
      
      # El método Kriging es un poco más complicado que el IDW, ya que requiere 
      # la construcción de un modelo de semivariograma para describir el patrón 
      # de autocorrelación espacial de los datos. En este caso no le remuevo la 
      # tendencia a los datos. En este enlace hacen algo similar pero removiéndole 
      # la tendencia: https://mgimond.github.io/Spatial/interpolation-in-r.html
      
      # Me aseguro que data_df sea un tibble
      data_df <- tibble::as_tibble(data_df)
      
      # Se deben eliminar los NA para poder crear los variogramas
      datos_df <- data_df %>%
        dplyr::filter(dplyr::across(dplyr::any_of(cols_to_interp), ~ !is.na(.)))
      
      # Los variogramas se crean usando objetos sp, para crearlos primero
      # es necesario crear un objeto sf
      datos_sf <- sf::st_as_sf(
        datos_df, coords = c("longitude", "latitude"), 
        remove = FALSE, crs = 4326) 
      datos_sp <- as(datos_sf, "Spatial")
      
      
      # Interpolar las variables, una a la vez
      interp_variables <- purrr::map(
        .x = if (length(cols_to_interp) > 1) cols_to_interp[!grepl('normal', cols_to_interp)] else cols_to_interp,
        .f = function(interp_col, datos_sp, new_grid_sf) {
          
          # Define interpolation formula
          f_left_side <- interp_col
          f_right_side <- 1  # "longitude + latitude"  # usar "longitude + latitude" para universal kriging
          interp_formula <- as.formula(paste(f_left_side, "~", f_right_side))
          
          # Veo la nube del variograma para los datos. 
          variogcloud <- gstat::variogram(
            object = interp_formula, 
            data = datos_sp, 
            locations = datos_sp, 
            cloud = TRUE)
          # plot(variogcloud)
          
          # Los valores en la nube se pueden agrupar y trazar con una función muy similar:
          semivariog <- gstat::variogram(
            object = interp_formula, 
            data = datos_sp, 
            locations = datos_sp)
          # plot(semivariog)
          
          # Calculo el modelo del variograma con la función "autofirVariogram":
          semivariog_fit <- automap::autofitVariogram(
            formula = interp_formula, 
            input_data = datos_sp, 
            model = "Sph")
          # Defino el modelo "Sph" (de "spherical") porque así está en el ejemplo. 
          # Si dejo este parámetro libre el gráfico me queda feo.
          # plot(semivariog, semivariog_fit$var_model, main = "Fitted variogram")
          
          # Se hace la interpolación de Kriging. 
          # Se usa el modelo de variograma creado en el anterior paso. 
          # Se usa la función "krige" del paquete "gstat" junto con el modelo de 
          # semivariograma creado para generar una superficie simple de Kriged.
          # Se aplica ordinary kriging, ver:
          # https://swilke-geoscience.net/post/2020-09-10-kriging_with_r/kriging/
          # Para aplicar simple kriging se debe agregar el parámetro beta
          # Para aplicar unversal kriging de debe usar "longitude + latitude" en 
          # lugar de 1 en el lado derecho de la formula.
          col_datos_interp <- gstat::krige(
            formula = interp_formula, 
            #beta = mean(datos_sf %>% dplyr::pull(interp_col)),
            locations = datos_sp, 
            newdata = new_grid_sf, 
            model = semivariog_fit$var_model)
          
          # Transformar los datos interpolados (un objeto sf) a un tibble
          col_datos_interp <- col_datos_interp %>%
            dplyr::mutate(
              longitude = sf::st_coordinates(.)[,1],
              latitude = sf::st_coordinates(.)[,2]) %>%
            sf::st_drop_geometry() %>%
            dplyr::rename(!!interp_col := var1.pred) %>% 
            dplyr::select(longitude, latitude, !!interp_col) %>%
            tibble::as_tibble()
          
          # Retornan la interpolación de la columna
          return (col_datos_interp)
          
        }, datos_sp = datos_sp, new_grid_sf = new_grid_sf)
      
      # Unir todas las interpolaciones en un solo df
      datos_interp <- 
        purrr::reduce(
          .x = interp_variables, 
          .f = dplyr::inner_join,
          by = c("longitude", "latitude")
        ) 
      
      # Calcular la probabilidad de above
      if ( length(cols_to_interp) > 1 )
        datos_interp <- datos_interp %>%
        dplyr::mutate(
          !!cols_to_interp[grepl('normal', cols_to_interp)] := 
            1 - rowSums(dplyr::across(
              cols_to_interp[!grepl('normal', cols_to_interp)])))
      
      # Retornar resultado
      return ( datos_interp )
    },
    interp_bilinear = function(data_df, cols_to_interp, new_points, grid = FALSE) {
      
      # Define interpolation formula
      f_left_side <- paste(cols_to_interp, collapse = " | ")
      f_right_side <- "longitude + latitude"
      interp_formula <- as.formula(paste(f_left_side, "~", f_right_side))
      
      # Interpolate data
      interp_data <- metR::Interpolate(
        formula = interp_formula, 
        x.out = new_points$longitude, 
        y.out = new_points$latitude, 
        grid = grid, 
        data = data_df)
      
      if ( length(cols_to_interp) > 1 ) {
        # Para estar seguros que las probabilidades interpoladas sumen 1,
        # la probabilidad normal se recalcula como 1 - above - bellow
        interp_data <- tibble::as_tibble(interp_data)  %>%
          dplyr::mutate(
            !!as.name(cols_to_interp[1]) := 
              1 - sum(cols_to_interp[seq(2, length(cols_to_interp))])
          )}
      
      # Retornar datos interpolados
      return ( interp_data )
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
# Declare class methods
InterpolationHelper$interp_kriging = function(...) {
  InterpolationHelper$public_methods$interp_kriging(...) 
}
InterpolationHelper$interp_bilinear = function(...) {
  InterpolationHelper$public_methods$interp_bilinear(...) 
}


#
#
#


DryMaskHelper <- R6::R6Class(
  classname = "DryMaskHelper",
  public = list(
    read_data = function(dry_mask_file, trgt_months) {
      
      # Leer archivo con mascara seca
      dry_mask_df = tidync::tidync(dry_mask_file) %>% 
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
            .x = trgt_months,
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
      
      # Retornar los datos agrupados
      return ( dry_mask_trgt_months )
    },
    interpolate = function(dry_mask_df, dest_points_df, spatial_domain ) {
      
      # Reducir cantidad de puntos, excluir puntos fuera del dominio espacial
      dry_mask_df <- dry_mask_df %>%
        dplyr::filter(longitude >= spatial_domain$wlo - 2, 
                      longitude <= spatial_domain$elo + 2, 
                      latitude <= spatial_domain$nla + 2, 
                      latitude >= spatial_domain$sla - 2)
      
      # Definir valores que permiten definir la nueva grilla
      trgt_lons <- dest_points_df$longitude
      trgt_lats <- dest_points_df$latitude
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
      new_dry_mask_df <- 
        InterpolationHelper$interp_bilinear(
          data_df = dry_mask_df,
          cols_to_interp = "must_be_masked",
          new_points = new_grid_sf) %>%
        dplyr::mutate(
          must_be_masked = dplyr::case_when(
            must_be_masked < .5 ~ as.logical(0),
            must_be_masked >= .5 ~ as.logical(1),
            TRUE ~ NA
          ))
      
      # Retornar máscara interpolada
      return ( new_dry_mask_df )
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
# Declare class methods
DryMaskHelper$read_data = function(...) {
  DryMaskHelper$public_methods$read_data(...) 
}
DryMaskHelper$interpolate = function(...) {
  DryMaskHelper$public_methods$interpolate(...) 
}


