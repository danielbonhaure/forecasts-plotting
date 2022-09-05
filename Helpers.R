
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "stringr", "htmltools", "htmlwidgets", "here")
for ( pkg in packages )
  if ( ! pkg %in% rownames(installed.packages()) )
    stop(paste("Package", pkg,"is not installed"))

# Cargar librerias
source(here::here("lib", "Helpers.R"), echo = FALSE, chdir = TRUE)
source(here::here("lib", "Configuracion.R"), echo = FALSE, chdir = TRUE)

# Cargar expresiones regulares (ya definidas de acuerdo a la configuración)
source(here::here("ExpresionesRegulares.R"), echo = FALSE, chdir = TRUE)


#
#
#


# Leer configuración
global_config <- Config$new(here::here('config.yaml'))

# Obtener condiciones inciales
global_ic <- global_config$get_config('initial_conditions')

# Obtener shapefiles e imágenes
global_shapefiles <- global_config$get_config('shapefiles')
global_images <- global_config$get_config('images')

# Leer shapes a ser utlizados en los gráficos
crcsas_sf <- sf::st_read(global_shapefiles$`crc-sas`, quiet = TRUE)

# Se crea un polígono con buffer para el CRC-SAS.
# Se crea aquí porque es una operación lenta.
# La distancia (parámetro dist) del buffer está en metros.
# 300km son más o menos 2 grados de lat y lon.
logger::log_info('Inicia operación sf::st_buffer sobre el shp del CRC-SAS')
buffered_crcsas_sf <- crcsas_sf %>%
  sf::st_transform(crs = 3857) %>%
  sf::st_buffer(dist = 300000) %>%
  sf::st_transform(crs = 4326)
logger::log_info('Finaliza operación sf::st_buffer sobre el shp del CRC-SAS')


#
#
#


# Color para NA en mapas
na_color <- "#5C5C5C"
na_color_prob <- "#8eb148"
mask_color <- "#f2c0fe"

# Determinar el mejor valor para el parámetro digits de la función round
round_digits <- function(valor) {
  dplyr::case_when(
    abs(valor) == 0 ~ 0,
    abs(valor) <= 0.00009 ~ 7,
    abs(valor) <= 0.0009 ~ 6,
    abs(valor) <= 0.009 ~ 5,
    abs(valor) <= 0.09 ~ 4,
    abs(valor) <= 0.9 ~ 3,
    abs(valor) <= 9 ~ 2,
    TRUE ~ 0)
}

# Redonder valores usando la función round_digits
auto_round <- function(valores) {
  lapply(
    X = valores, 
    FUN = function(valor) {
      round(
        x = valor, 
        digits = round_digits(valor)) 
    })
}

# Excluir puntos que están fuera del polígono
exclude_points_outside_polygon <- function(a_tibble_df, polygon_sf, 
                                           dist_buffer=NULL) {
  buffered_polygon <- polygon_sf %>%
    sf::st_transform(crs = 3857) %>%
    { if ( !is.null(dist_buffer) )
      sf::st_buffer(., dist = dist_buffer)
      else . }
  
  filtered_df <- a_tibble_df %>%
    dplyr::filter(latitude <= 0) %>%
    sf::st_as_sf(
      coords = c("longitude", "latitude"), 
      remove = FALSE, crs = 4326) %>%
    sf::st_transform(crs = 3857) %>%
    sf::st_filter(buffered_polygon) %>%
    sf::st_drop_geometry()
  
  return ( filtered_df )
}

# Excluir puntos que no están dentro del polígono del CRC-SAS
exclude_points_outside_crcsas <- function(a_tibble_df, dist_buffer=NULL) {
  return ( exclude_points_outside_polygon(a_tibble_df, crcsas_sf, dist_buffer) )
}

# Excluir puntos que no están dentro del polígono del CRC-SAS extendido
exclude_points_outside_buffered_crcsas <- function(a_tibble_df, dist_buffer=NULL) {
  return ( exclude_points_outside_polygon(a_tibble_df, buffered_crcsas_sf, dist_buffer) )
}

# Determinar el color asociado a un valor
get_value_color <- function(valor, breaks, colors, na_value_color) {
  # Si el valor es NA o NULL o no es numérico, se retorna gris
  if ( is.na(valor) || is.null(valor) || !is.numeric(valor) )
    return ( na_value_color )
  # Siempre debe haber un color más que breaks
  stopifnot(length(breaks) == length(colors) - 1)
  # Definir el color para el valor
  if ( valor <= breaks[1] ) {
    # Si el valor es <= al 1er/menor break, el color es 1ero de la lista
    return ( colors[1] )
  } else if ( valor > breaks[1] && valor < breaks[length(breaks)] ) {
    # Si el valor es > al 1er/menor break y < (estrictamente menor) al 
    # último/mayor break, se determina el color de la siguiente manera
    for ( i in 2:length(breaks) )
      # Se compara el valor con cada uno de lo breaks intermedios. Tener
      # en cuenta que en la condición <= para el último break del for, en 
      # realidad la igualdad nunca se da por el if que habilita el for!!
      if ( valor > breaks[i-1] && valor <= breaks[i]) 
        return (colors[i])
  } else if ( valor >= breaks[length(breaks)] ) {
    # Si el valor es >= al último/mayor break, el color es último de la lista
    return ( colors[length(colors)] )
  } else {
    stop ( "Error en la función custom_palette." )
  }
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


GenerarHTMLLogo <- function(logo.file) {
  logo.ascii <- base::readBin(con = logo.file,
                              what = "raw",
                              n = base::file.info(logo.file)[1, "size"])
  logo.b64   <- RCurl::base64Encode(txt = logo.ascii,
                                    mode = "character")
  html       <- paste0("<img src='data:image/png;base64,", logo.b64,
                       "' border=\"0\" alt=\"CRC-SAS\"/>")
  return (html)
}


PlotsHelper <- R6::R6Class(
  classname = "PlotsHelper",
  public = list(
    graficar_mapa = function(data_df, gridded_data, spatial_domain, 
                             main_title, legend_title, 
                             output_file_abspath, 
                             breaks = NULL, colors = NULL, 
                             dry_mask_df, save_map = T) {
      
      # Establecer breaks y color por defecto (siempre debe haber un color más que breaks)
      if ( is.null(breaks) && is.null(colors) ) {
        colors <- viridis::viridis(10)
        probs <- seq(0, 1, length.out = length(colors) + 2)
        quant <- stats::quantile(data_df$value, probs, na.rm = TRUE)
        breaks <- quant[seq(2, length(quant)-2)]
      } else if ( is.null(breaks) ) {
        probs <- seq(0, 1, length.out = length(colors) + 2)
        quant <- stats::quantile(data_df$value, probs, na.rm = TRUE)
        breaks <- quant[seq(2, length(quant)-2)]
      } else if ( is.null(colors) ) {
        colors <- viridis::viridis( length(breaks) + 1 )
      }
      
      # Determinar el color de cada celda
      data_df <- data_df %>%
        dplyr::mutate(
          c_color = purrr::map_chr(
            value, ~ get_value_color(.x, breaks, colors, na_color)) )
      
      # Enmascarar con máscara seca si corresponde
      if ( !is.null(dry_mask_df) ) {
        data_df <- data_df %>%
          dplyr::left_join(
            dry_mask_df, by = c("longitude", "latitude")) %>%
          dplyr::mutate(
            c_color = ifelse(must_be_masked, mask_color, c_color)) %>%
          dplyr::select(-must_be_masked)
      }
      
      # Identificar distancia entre puntos a graficar
      dist_between_lon <- min(spatial_dist(data_df$longitude)) / 2
      dist_between_lat <- min(spatial_dist(data_df$latitude)) / 2
      if (!gridded_data) {
        min_distance <- min(dist_between_lon, dist_between_lat)
        circle_radius <- ifelse(min_distance < 2, 3, 5)
      }
      
      # Generar el gráfico
      css_fix_1 <- 
        ".info.legend.principal {background: rgba(255, 255, 255, 0.5) !important;}"
      css_fix_2 <- 
        ".leaflet-top .leaflet-control {margin-top: 20px;}"
      css_title <- 
        ".leaflet-control.map-title {text-align: center; padding-left: 10px; padding-right: 10px; font-weight: bold; font-size: 14px;}"
      # Convert CSS to HTML
      html_fix <- htmltools::tags$style(
        type = "text/css", paste(css_fix_1, css_fix_2, css_title))
      # Crear mapa con leaflet
      m <- leaflet::leaflet() %>%
        leaflet::fitBounds(
          lng1 = spatial_domain$wlo, lng2 = spatial_domain$elo,
          lat1 = spatial_domain$sla, lat2 = spatial_domain$nla) %>%
        leaflet::addTiles(
          urlTemplate = paste0("//server.arcgisonline.com/ArcGIS/rest",
                               "/services/World_Street_Map/MapServer",
                               "/tile/{z}/{y}/{x}")) %>%
        leafem::addMouseCoordinates() %>%
        leaflet::addSimpleGraticule() %>%
        leaflet::addPolygons(
          data = crcsas_sf,
          stroke = TRUE,
          opacity = 1.0,
          weight = 1,
          fillOpacity = 0.0,
          smoothFactor = 0.5,
          color = "#000000") %>%
        { if ( gridded_data ) 
          leaflet::addRectangles(
            map = .,
            data = data_df,
            lng1 = ~ longitude - dist_between_lon,
            lat1 = ~ latitude - dist_between_lat,
            lng2 = ~ longitude + dist_between_lon,
            lat2 = ~ latitude + dist_between_lat,
            stroke = FALSE,
            fillColor = ~ c_color,
            fillOpacity = 0.7,
            popup = ~ glue::glue("Lon: {longitude}, Lat: {latitude} <br> ",
                                 "<center>Val: {auto_round(value)}</center>"),
            label = ~ as.character(auto_round(value)))
          else . } %>%
        { if ( !gridded_data ) 
          leaflet::addCircleMarkers(
            map = .,
            lng = ~ longitude,
            lat = ~ latitude,
            data = data_df,
            radius = circle_radius,
            stroke = FALSE,
            fillColor = ~ c_color,
            fillOpacity = 0.7,
            popup = ~ glue::glue("Lon: {longitude}, Lat: {latitude} <br> ",
                                 "<center>Val: {auto_round(value)}</center>"),
            label = ~ as.character(auto_round(value)))
          else . } %>%
        leaflet::addLegend(
          map = .,
          position = "bottomright", 
          colors = c(
            glue::glue('conic-gradient(at 50% 0%, ',
                       'transparent 135deg, {colors[1]} 0, ',
                       '{colors[1]} 225deg, transparent 0);'),
            colors[seq(2, length(colors) - 1)],
            glue::glue('conic-gradient(at 50% 100%, ',
                       '{colors[length(colors)]} 45deg, transparent 0, ',
                       'transparent 315deg, {colors[length(colors)]} 0)'),
            purrr::map_chr(
              .x = if (!is.null(dry_mask_df)) c(na_color, mask_color) else na_color, 
              .f = ~ glue::glue('{.x};border-radius: 25px;margin-top:3px;'))),
          labels = c(
            "<i style='opacity: .9;'></i>", 
            purrr::map_chr(
              .x = breaks, 
              .f = ~ paste0("<i style='opacity: .9; margin-top: -9px;'>", .x, "</i>")),
            purrr::map_chr(
              .x = if (!is.null(dry_mask_df)) c("NaN", "D.M.") else "NaN", 
              .f = ~ paste0("<i style='opacity: .9;'>", .x, "</i>"))),
          title = htmltools::HTML(legend_title),
          className = "info legend principal",
          opacity = 0.9) %>%
        leaflet::addControl(
          html = htmltools::tags$div(
            htmltools::HTML(stringr::str_replace_all(main_title, '\n', '<br>'))),
          position = "topright",
          className="map-title info") %>%
        leaflet::addControl(
          html = GenerarHTMLLogo(
            global_images$`crc-sas`), 
          position = "bottomleft") %>%
        leaflet.extras2::addEasyprint(
          options = leaflet.extras2::easyprintOptions(
            title = "Descargar mapa a PNG",
            sizeModes = list("A4Portrait", "A4Landscape"),
            exportOnly = TRUE,
            hideControlContainer = FALSE,
            filename = basename(output_file_abspath) %>% 
              tools::file_path_sans_ext() %>% paste0('.png'))) %>%
        htmlwidgets::prependContent(html_fix)
      # Guardar mapa
      if ( save_map )
        htmlwidgets::saveWidget(
          widget = m, 
          file = output_file_abspath, 
          selfcontained = TRUE)
      # Retornar mapa (para pruebas)
      return ( m )
    },
    graficar_mapa_prob = function(data_df, gridded_data, spatial_domain,
                                  main_title, output_file_abspath, 
                                  breaks = NULL, colors_below = NULL, 
                                  colors_normal = NULL, colors_above = NULL,
                                  dry_mask_df, save_map = T) {
        
      # Determinar el color de cada celda
      data_df <- data_df %>%
        dplyr::mutate(
          c_color = dplyr::case_when(
            category == 'below' ~ purrr::map_chr(
              .x = prob_below, 
              .f = ~ get_value_color(.x, breaks[seq(2, length(breaks)-1)], colors_below, na_color_prob)),
            category == 'normal' ~ purrr::map_chr(
              .x = prob_normal, 
              .f = ~ get_value_color(.x, breaks[seq(2, length(breaks)-1)], colors_normal, na_color_prob)),
            category == 'above' ~ purrr::map_chr(
              .x = prob_above, 
              .f = ~ get_value_color(.x, breaks[seq(2, length(breaks)-1)], colors_above, na_color_prob)),
            is.na(category) ~ purrr::map_chr(
              .x = NA, 
              .f = ~ get_value_color(.x, breaks[seq(2, length(breaks)-1)], colors_normal, na_color_prob)),
            TRUE ~ NA_character_))
      
      # Enmascarar con máscara seca si corresponde
      if ( !is.null(dry_mask_df) ) {
        data_df <- data_df %>%
          dplyr::left_join(
            dry_mask_df, by = c("longitude", "latitude")) %>%
          dplyr::mutate(
            c_color = ifelse(must_be_masked, mask_color, c_color)) %>%
          dplyr::select(-must_be_masked)
      }
      
      # Identificar distancia entre puntos a graficar
      dist_between_lon <- min(spatial_dist(data_df$longitude)) / 2
      dist_between_lat <- min(spatial_dist(data_df$latitude)) / 2
      if (!gridded_data) {
        dist_between_lon <- ifelse(dist_between_lon > 2, 2, dist_between_lon)
        dist_between_lat <- ifelse(dist_between_lat > 2, 2, dist_between_lat)
        circle_radius <- min(c(dist_between_lon, dist_between_lat))
      }
      
      # Generar el gráfico
      css_fix_1 <- 
        ".info.legend.principal {background: rgba(255, 255, 255, 0.5) !important;}"
      css_fix_2 <- 
        ".leaflet-top .leaflet-control {margin-top: 20px;}"
      css_title <- 
        ".leaflet-control.map-title {text-align: center; padding-left: 10px; padding-right: 10px; font-weight: bold; font-size: 14px;}"
      # Convert CSS to HTML
      html_fix <- htmltools::tags$style(
        type = "text/css", paste(css_fix_1, css_fix_2, css_title))
      # Crear mapa con leaflet
      m <- leaflet::leaflet() %>%
        leaflet::fitBounds(
          lng1 = spatial_domain$wlo, lng2 = spatial_domain$elo,
          lat1 = spatial_domain$sla, lat2 = spatial_domain$nla) %>%
        leaflet::addTiles(
          urlTemplate = paste0("//server.arcgisonline.com/ArcGIS/rest",
                               "/services/World_Street_Map/MapServer",
                               "/tile/{z}/{y}/{x}")) %>%
        leafem::addMouseCoordinates() %>%
        leaflet::addSimpleGraticule() %>%
        leaflet::addPolygons(
          data = crcsas_sf,
          stroke = TRUE,
          opacity = 1.0,
          weight = 1,
          fillOpacity = 0.0,
          smoothFactor = 0.5,
          color = "#000000") %>%
        { if ( gridded_data ) 
          leaflet::addRectangles(
            map = .,
            data = data_df,
            lng1 = ~ longitude - dist_between_lon,
            lat1 = ~ latitude - dist_between_lat,
            lng2 = ~ longitude + dist_between_lon,
            lat2 = ~ latitude + dist_between_lat,
            stroke = FALSE,
            fillColor = ~ c_color,
            fillOpacity = 0.7,
            popup = ~ glue::glue("Lon: {longitude}, Lat: {latitude} <br> ",
                                 "<center>Prob. Below: {auto_round(prob_below)}</center>",
                                 "<center>Prob. Near: {auto_round(prob_normal)}</center>",
                                 "<center>Prob. Above: {auto_round(prob_above)}</center>"),
            label = ~ dplyr::case_when(
              category == 'below' ~ glue::glue("Prob. Below: {auto_round(prob_below)}"),
              category == 'normal' ~ glue::glue("Prob. Normal: {auto_round(prob_normal)}"),
              category == 'above' ~ glue::glue("Prob. Above: {auto_round(prob_above)}"),
              is.na(category) ~ 'Sin datos',
              TRUE ~ 'Categoría desconocida!'))
          else . } %>%
        { if ( !gridded_data ) 
          leaflet::addCircleMarkers(
            map = .,
            data = data_df,
            radius = circle_radius,
            stroke = FALSE,
            fillColor = ~ c_color,
            fillOpacity = 0.7,
            popup = ~ glue::glue("Lon: {longitude}, Lat: {latitude} <br> ",
                                 "<center>Prob. Below: {auto_round(prob_below)}</center>",
                                 "<center>Prob. Near: {auto_round(prob_normal)}</center>",
                                 "<center>Prob. Above: {auto_round(prob_above)}</center>"),
            label = ~ dplyr::case_when(
              category == 'below' ~ glue::glue("Prob. Below: {auto_round(prob_below)}"),
              category == 'normal' ~ glue::glue("Prob. Normal: {auto_round(prob_normal)}"),
              category == 'above' ~ glue::glue("Prob. Above: {auto_round(prob_above)}"),
              is.na(category) ~ 'Sin datos',
              TRUE ~ 'Categoría desconocida!'))
          else . } %>%
        leaflet::addLegend(
          map = .,
          position = "bottomright",
          colors = c('#FFFFFF;visibility: hidden;',
                     '#FFFFFF;visibility: hidden;',
                     colors_below, 
                     '#FFFFFF;visibility: hidden;',
                     '#FFFFFF;visibility: hidden;',
                     '#FFFFFF;visibility: hidden;',
                     colors_normal,
                     '#FFFFFF;visibility: hidden;',
                     '#FFFFFF;visibility: hidden;',
                     '#FFFFFF;visibility: hidden;',
                     colors_above,
                     '#FFFFFF;visibility: hidden;',
                     purrr::map_chr(
                       .x = if (!is.null(dry_mask_df)) c(na_color, mask_color) else na_color, 
                       .f = ~ glue::glue('{.x};border-radius: 25px;margin-top:3px;'))),
          labels = c("<b style='margin-left:-20px'> Below Normal </b>", "",
                     purrr::map_chr(
                       .x = breaks, 
                       .f = ~ paste0("<i style='opacity: .9; margin-top: -9px;'>", .x, "</i>")),
                     "<b style='margin-left:-20px'> Near Normal </b>", "",
                     purrr::map_chr(
                       .x = breaks, 
                       .f = ~ paste0("<i style='opacity: .9; margin-top: -9px;'>", .x, "</i>")),
                     "<b style='margin-left:-20px'> Above Normal </b>", "",
                     purrr::map_chr(
                       .x = breaks, 
                       .f = ~ paste0("<i style='opacity: .9; margin-top: -9px;'>", .x, "</i>")),
                     purrr::map_chr(
                       .x = if (!is.null(dry_mask_df)) c("NaN", "DryM.") else "NaN", 
                       .f = ~ paste0("<i style='opacity: .9;'>", .x, "</i>"))),
          className = "info legend principal",
          opacity = 0.9) %>%
        leaflet::addControl(
          html = htmltools::tags$div(
            htmltools::HTML(stringr::str_replace_all(main_title, '\n', '<br>'))),
          position = "topright",
          className="map-title info") %>%
        leaflet::addControl(
          html = GenerarHTMLLogo(
            global_images$`crc-sas`), 
          position = "bottomleft") %>%
        leaflet.extras2::addEasyprint(
          options = leaflet.extras2::easyprintOptions(
            title = "Descargar mapa a PNG",
            sizeModes = list("A4Portrait", "A4Landscape"),
            exportOnly = TRUE,
            hideControlContainer = FALSE,
            filename = basename(output_file_abspath) %>% 
              tools::file_path_sans_ext() %>% paste0('.png'))) %>%
        htmlwidgets::prependContent(html_fix)
      # Guardar mapa
      if ( save_map )
        htmlwidgets::saveWidget(
          widget = m, 
          file = output_file_abspath, 
          selfcontained = TRUE)
      # Retornar mapa (para pruebas)
      return ( m )
    },
    aplicar_suavizado = function(data_df, cols_to_interp, delta, inplace) {
      ##### Kriging  
      
      # El método Kriging es un poco más complicado que el IDW, ya que requiere 
      # la construcción de un modelo de semivariograma para describir el patrón 
      # de autocorrelación espacial de los datos. En este caso no le remuevo la 
      # tendencia a los datos. En este enlace hacen algo similar pero removiéndole 
      # la tendencia: https://mgimond.github.io/Spatial/interpolation-in-r.html
      
      # Me aseguro que data_df sea un tibble
      data_df <- tibble::as_tibble(data_df)
      
      # Se mueven los puntos antes de crear la grilla, así cuando se
      # crea la grilla los nuevos puntos están donde estaban en un 
      # principio los originales, dando la impresión de que luego de 
      # la interpolación los puntos no se movieron.
      if (inplace) {
        data_df <- data_df %>%
          dplyr::mutate(
            longitude = longitude - delta,
            latitude = latitude - delta)
      }
      
      # Crear grilla nueva. Los puntos de la grilla nueva no pueden coincidir
      # con los puntos de la grilla original, porque si coinciden la 
      # interpolación no producen ningún cambio en los gráficos.
      if ( global_config$get_config("unify_grid") ) {
        spatial_domain <- global_config$get_config("spatial_domain")
        grid_resolution <- global_config$get_config("unify_grid_resolution")
        new_grid_sf <- tibble::as_tibble(
          expand.grid(longitude = seq(from = spatial_domain$wlo, 
                                      to = spatial_domain$elo, 
                                      by = grid_resolution), 
                      latitude = seq(from = spatial_domain$sla, 
                                     to = spatial_domain$nla, 
                                     by = grid_resolution))) %>%
          sf::st_as_sf(coords = c("longitude", "latitude"), 
                       remove = FALSE, crs = 4326)
      } else {
        new_grid_sf <- tibble::as_tibble(
          expand.grid(longitude = seq(from = min(data_df$longitude) - delta, 
                                      to = max(data_df$longitude) + delta, 
                                      by = delta*2), 
                      latitude = seq(from = min(data_df$latitude) - delta, 
                                     to = max(data_df$latitude) + delta, 
                                     by = delta*2))) %>%
          sf::st_as_sf(coords = c("longitude", "latitude"), 
                       remove = FALSE, crs = 4326)
      }
      
      # Se deben eliminar los NA para poder crear los variogramas
      datos_df <- data_df %>%
        dplyr::filter(dplyr::across(cols_to_interp, ~ !is.na(.)))
      
      # Los variogramas se crean usando objetos sp, para crearlos primero
      # es necesario crear un objeto sf
      datos_sf <- sf::st_as_sf(
        datos_df, coords = c("longitude", "latitude"), 
        remove = FALSE, crs = 4326) 
      datos_sp <- as(datos_sf, "Spatial")
      
      
      # Interpolar las variables, una a la vez
      datos_interp <- 
        purrr::reduce(
          .x = purrr::map(
            .x = if (length(cols_to_interp) > 1) cols_to_interp[!grepl('normal', cols_to_interp)] else cols_to_interp,
            .f = function(interp_col) {
              
              # Define interpolation formula
              f_left_side <- interp_col
              f_right_side <- 1  # "longitude + latitude"  # usar "longitude + latitude" para universal kriging
              interp_formula <- as.formula(paste(f_left_side, "~", f_right_side))
              
              # Veo la nube del variograma para los datos. 
              variogcloud <- gstat::variogram(
                object = interp_formula, 
                data = datos_sp, 
                locations = data_sp, 
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
            }),  # fin del purrr:map
          .f = dplyr::inner_join,
          by = c("longitude", "latitude")
        )  # fin del purrr::reduce
      
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
    definir_titulo = function(data_type, base_file, data_year = NULL) {
      
      variable <- base_file$variable
      variable_str <- ifelse(variable == 'prcp', 'Precipitation', 'Average Air Temperature - 2m')
      variable_unit <- ifelse(variable == 'prcp', 'mm', '°C')
      
      modelo <- stringr::str_extract(base_file$basename, cpt_regex_modelos)
      if ( is.na(modelo) )
        modelo <- stringr::str_extract(base_file$basename, ereg_regex_modelos)
      
      initial_month <- month.abb[global_ic$month]
      initial_year <- global_ic$year
      
      valid_months <- stringr::str_split(base_file$target_months, '-') %>% 
        purrr::reduce(c) %>% as.numeric()
      
      forecast_months_str <- ifelse(
        length(valid_months) == 1, month.name[valid_months[[1]]], 
        paste0(month.abb[valid_months[[1]]], '-', month.abb[valid_months[[3]]]))
      
      if ( !is.null(data_year) ) {
        month_year <- glue::glue("{month.abb[valid_months[[1]]]} {data_year}")
        if (length(valid_months) > 1) {
          if (valid_months[[1]] > valid_months[[3]]) {
            month_year <- glue::glue("{month_year} - {month.abb[valid_months[[3]]]} {data_year+1}")
          } else {
            month_year <- glue::glue("{month_year} - {month.abb[valid_months[[3]]]} {data_year}")
          }
        } 
      }
      
      # Definir título por defecto
      main_title <- "Título no definido"
      
      # Definir título real
      if (data_type == "anom") {
        main_title <- glue::glue("{variable_str} Anomaly Forecast ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {month_year} ",
                                 "\nIssued: {initial_month} {initial_year}")
      } else if (data_type == "corr") {
        main_title <- glue::glue("Correlation between Forecast and Observation ",
                                 "({base_file$hcst_first_year}-{base_file$hcst_last_year})",
                                 "\n{toupper(modelo)} valid for {forecast_months_str} ",
                                 "\nIssued: {initial_month}")
      } else if (data_type == "det.fcst") {
        main_title <- glue::glue("{variable_str} ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {month_year} ",
                                 "\nIssued: {initial_month} {initial_year} ",
                                 "\n(calibrated forecast)")
      } else if (data_type == "prob.fcst") {
        main_title <- glue::glue("{variable_str} ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {month_year} ",
                                 "\nIssued: {initial_month} {initial_year} ",
                                 "\n(calibrated forecast)")
      } else if (data_type == "uncal.fcst") {
        main_title <- glue::glue("{variable_str} ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {month_year} ",
                                 "\nIssued: {initial_month} {initial_year} ",
                                 "\n(uncalibrated forecast)")
      } 
      
      # Retornar título real
      return ( main_title )
      
    }
  ),
  private = list(
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
# Declare class methods
PlotsHelper$graficar_mapa = function(...) {
  PlotsHelper$public_methods$graficar_mapa(...)
}
PlotsHelper$graficar_mapa_prob = function(...) {
  PlotsHelper$public_methods$graficar_mapa_prob(...)
}
PlotsHelper$aplicar_suavizado = function(...) {
  PlotsHelper$public_methods$aplicar_suavizado(...)
}
PlotsHelper$definir_titulo = function(...) {
  PlotsHelper$public_methods$definir_titulo(...)
}

CorrelationHelper <- R6::R6Class(
  classname = "CorrelationHelper",
  public = list(
    compute_correlation = function(det_data_df, obs_data_df, variable,
                                   lower_training_year = NULL,
                                   higher_training_year = NULL) {
      
      # Definir y seleccionar las columnas que debe tener det_data_df
      det_data_cols <- c("longitude", "latitude", "init_time", "year", "month", variable)
      det_data_df <- det_data_df %>% dplyr::select(dplyr::any_of(det_data_cols)) 
      
      # Si det_data_df no tiene las columnas apropiadas, no se puede seguir
      if ( ! all( det_data_cols %in% colnames(det_data_df) ) ) {
        warning("El df con datos pronosticados no tiene las columnas esperadas")
        return ( invisible(NULL) )
      }
      
      # Definir seleccionar las columnas que debe tener obs_data_df
      obs_data_cols <- c("longitude", "latitude", "year", "month", variable)
      obs_data_df <- obs_data_df %>% dplyr::select(dplyr::any_of(obs_data_cols)) 
      
      # Si obs_data_df no tiene las columnas apropiadas, no se puede seguir
      if ( ! all( obs_data_cols %in% colnames(obs_data_df) ) ) {
        warning("El df con datos obsevardos no tiene las columnas esperadas")
        return ( invisible(NULL) )
      }
      
      # Si las grillas no son iguales, entonces no se puede seguir
      grilla_det_data <- det_data_df %>% 
        dplyr::select(longitude, latitude) %>%
        dplyr::distinct()
      grilla_obs_data <- obs_data_df %>% 
        dplyr::select(longitude, latitude) %>%
        dplyr::distinct()
      grilla_comun <- grilla_det_data %>% 
        dplyr::inner_join(grilla_obs_data, 
                          by = c("longitude", "latitude"))
      if ( nrow(grilla_det_data) != nrow(grilla_obs_data) |
           nrow(grilla_det_data) != nrow(grilla_comun) ) {
        warning("El df con los datos obsevardos y el df con datos ",
                "pronosticados no tienen la misma grilla")
        return ( invisible(NULL) )
      }
      
      #
      #
      
      # Definir nombre de las columnas luego del join
      det_col <- glue:::glue("{variable}.det")
      obs_col <- glue:::glue("{variable}.obs")
      
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
          corr = stats::cor(!!as.name(det_col), !!as.name(obs_col),
                            use = "na.or.complete"), 
          .groups = 'drop'
        ) %>%
        dplyr::ungroup()
      
      # Retornar resultado
      return ( data_obs_statistics )
    }
  ),
  lock_objects = TRUE,
  portable = TRUE,
  lock_class = TRUE
)
# Declare class methods
CorrelationHelper$compute_correlation = function(...) {
  CorrelationHelper$public_methods$compute_correlation(...) 
}

