
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "ncdf4", "lubridate", "stringr")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))


#
#
#


# Crear una secuencia/rango circular
crange <- function(start, stop, modulo) {
  if (start > stop)
    return ( c(seq(start, modulo), seq(1, stop)) )
  else
    return ( seq(start, stop) )
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


