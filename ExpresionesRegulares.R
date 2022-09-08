
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
global_ic <- global_config$get_config('initial_conditions')
if ( is.null(global_ic) ) {
  global_ic <- list()
  global_ic$month <- lubridate::month(lubridate::now())
  global_ic$year <- lubridate::year(lubridate::now())
}


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
  resp <- purrr::map_dbl(resp, ~ ifelse(.x > 12, .x %% 12, .x))
  return (resp)
}


#
#
#


config_cpt <- global_config$get_config('acc-cpt')

cpt_regex_modelos <- paste0('(?:', paste(config_cpt$modelos, collapse='|'), ')')

cpt_regex_pre_variables <- '(?:precip|tmp2m)'
cpt_regex_variables <- paste0('(?:', paste(config_cpt$variables, collapse='|'), ')')

cpt_regex_fuente_datos <- paste0('(?:', paste(config_cpt$fuentes, collapse='|'), ')')

cpt_regex_init_month <- paste0(month.abb[global_ic$month], 'ic')

cpt_valid_months <- crange(global_ic$month+1, global_ic$month+3, 12)
cpt_regex_months <- paste0('(?:', cpt_valid_months[1], '-', cpt_valid_months[3], 
                           '|', cpt_valid_months[1], '|', cpt_valid_months[2], 
                           '|', cpt_valid_months[3], ')')

cpt_regex_hcst_years <- '(?:1981-2010|1982-2011|1991-2020|1992-2021)'

fcst_year <- ifelse(
  all(global_ic$month < cpt_valid_months),
  yes=global_ic$year,
  no=global_ic$year+1)
cpt_regex_fcst_years <- paste0('(?:', fcst_year, '-', fcst_year, '|', fcst_year, ')')

cpt_files_regex <- paste0(
  '^', 
  cpt_regex_modelos, '_', cpt_regex_pre_variables, '-', cpt_regex_variables, '_', 
  cpt_regex_fuente_datos, '_', cpt_regex_init_month, '_', cpt_regex_months, '_', 
  cpt_regex_hcst_years, '_', cpt_regex_fcst_years, '_1\\.txt',
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
  ereg_regex_init_year, '_', ereg_regex_months, '_gp_01_mean_cor_wsereg.npz',
  '$')

