
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("here")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))

# Cargar librerias
source(here::here("lib", "Helpers.R"), echo = FALSE, chdir = TRUE)
source(here::here("lib", "Configuracion.R"), echo = FALSE, chdir = TRUE)


#
#
#


# Leer configuración
global_config <- Config$new(here::here('config.yaml'))

# Obtener condiciones inciales
global_ic <- global_config$get_initial_conditions()


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


#
#
#


config_cpt <- global_config$get_config('pycpt')

cpt_regex_modelos <- paste0('(?:', paste(config_cpt$modelos, collapse='|'), ')')

cpt_regex_pre_variables <- '(?:precip|tmp2m)'
cpt_regex_variables <- paste0('(?:', paste(config_cpt$variables, collapse='|'), ')')

cpt_regex_fuente_datos <- paste0('(?:', paste(config_cpt$fuentes, collapse='|'), ')')

cpt_regex_init_month <- paste0(month.abb[global_ic$month], 'ic')

cpt_valid_months <- c()
cpt_valid_trimesters <- c()
for ( lt in global_config$get_config('output_leadtimes') ) {
  cpt_valid_months <- c(cpt_valid_months, csum(global_ic$month, lt, 12))
  trimester <- crange(global_ic$month+lt, global_ic$month+lt+2, 12)
  cpt_valid_trimesters <- c(cpt_valid_trimesters, 
                            paste0(csum(global_ic$month, lt, 12), '-', trimester[3]))
}
cpt_regex_months <- paste0('(?:', paste(cpt_valid_months, collapse = "|"), "|", 
                           paste(cpt_valid_trimesters, collapse = "|"), ')')

cpt_regex_hcst_years <- '(?:1981-2010|1981-2011|1982-2011|1991-2020|1991-2021|1992-2021)'

cpt_valid_years <- c(global_ic$year,  # para meses en el mismo año
                     paste0(global_ic$year, '-', global_ic$year),  # para trimestres en el mismo año  
                     paste0(global_ic$year, '-', global_ic$year+1),  # para trimestres con meses en dos años
                     global_ic$year+1,  # para meses en el siguiente año
                     paste0(global_ic$year+1, '-', global_ic$year+1))  # para trimestres con todos los meses en el siguiente año
cpt_regex_fcst_years <- paste0('(?:', paste(cpt_valid_years, collapse = '|'), ')')

cpt_files_regex <- paste0(
  '^', 
  cpt_regex_modelos, '_', cpt_regex_pre_variables, '-', cpt_regex_variables, '_', 
  cpt_regex_fuente_datos, '_', cpt_regex_init_month, '_', cpt_regex_months, '_',  
  cpt_regex_hcst_years, '_', cpt_regex_fcst_years, '_1_forecast\\.nc',
  '$')


#
#
#


config_ereg <- global_config$get_config('ereg')

ereg_regex_variables <- paste0('(?:', paste(config_ereg$variables, collapse='|'), ')')

ereg_regex_modelos <- paste0('(?:', paste(config_ereg$modelos, collapse='|'), ')')

ereg_regex_init_month <- month.abb[global_ic$month]
ereg_regex_init_year <- global_ic$year
ereg_regex_init_year_str <- as.character(global_ic$year)

valid_trimesters <- c('JFM', 'FMA', 'MAM', 'AMJ', 'MJJ', 'JJA', 
                      'JAS', 'ASO', 'SON', 'OND', 'NDJ', 'DJF')
ereg_regex_months <- paste0('(?:', paste(valid_trimesters, collapse='|'), ')')

ereg_files_regex <- paste0(
  '^', 
  ereg_regex_variables, '_', ereg_regex_modelos, '_', ereg_regex_init_month, 
  ereg_regex_init_year, '_', ereg_regex_months, '_gp_01_mean_cor_wsereg\\.nc',
  '$')

