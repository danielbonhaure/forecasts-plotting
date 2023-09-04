
# Cargar paquetes
library(dplyr, quietly=TRUE)

# Verificar que estén instalados los paquetes necesarios
packages <- c("R6", "stringr", "htmltools", "htmlwidgets", "here", 
              "automap", "leaflet", "leaflet.extras2", "sf", "logger",
              "tibble", "purrr", "gstat", "metR", "RCurl", "viridis",
              "stats", "glue", "tools", "leafem")
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
global_ic <- global_config$get_initial_conditions()

# Obtener shapefiles e imágenes
global_shapefiles <- global_config$get_config('shapefiles')
global_images <- global_config$get_config('images')

# Leer shapes a ser utlizados en los gráficos
crcsas_sf <- sf::st_read(global_shapefiles$crcsas, quiet = TRUE)

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
na_color_prob <-"#745b6b" #"#65745b"
mask_color <- "#dfd8bf"

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


GenerarHTMLLogo <- function(logo.file, alt.text) {
  logo.ascii <- base::readBin(con = logo.file,
                              what = "raw",
                              n = base::file.info(logo.file)[1, "size"])
  logo.b64   <- RCurl::base64Encode(txt = logo.ascii,
                                    mode = "character")
  html       <- paste0("<img src='data:image/png;base64,", logo.b64,
                       "' border=\"0\" alt=\"", alt.text, "\"/>")
  return (html)
}


PlotsHelper <- R6::R6Class(
  classname = "PlotsHelper",
  public = list(
    graficar_mapa = function(data_df, gridded_data, spatial_domain, 
                             main_title, legend_title, data_type, lang,
                             output_file_abspath, download_file_basename,
                             dry_mask_df,
                             breaks = NULL, colors = NULL, rev_legend = FALSE,
                             include_first = FALSE, include_last = FALSE, 
                             save_map = TRUE) {
      
      # Definir texto a ser utilizados
      no_data_txt <- switch(lang, "en" = "No data", 
                            "es" = "Sin datos", 
                            "pt" = "Sem dados")
      no_data_txt_short <- switch(lang, "en" = "No data", 
                                  "es" = "Sin datos", 
                                  "pt" = "Sem dados")
      masked_txt <- switch(lang, "en" = "Masked, Dry Season", 
                           "es" = "Enmascarada, Estación Seca", 
                           "pt" = "Mascarada, Estação Seca")
      masked_txt_short <- switch(lang, "en" = "Dry Season", 
                                 "es" = "Estación Seca", 
                                 "pt" = "Estação Seca")
      
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
      breaks_aux <- breaks
      if (include_first)
        breaks_aux <- breaks_aux[-(if (rev_legend) length(breaks_aux) else 1)]
      if (include_last)
        breaks_aux <- breaks_aux[-(if (rev_legend) 1 else length(breaks_aux))]
      data_df <- data_df %>%
        dplyr::mutate(
          c_color = purrr::map_chr(
            value, ~ get_value_color(.x, breaks_aux, colors, na_color)),
          label_msg = dplyr::case_when(
            is.na(value) ~ no_data_txt,
            TRUE ~ as.character(auto_round(value)) ) )
      
      # Enmascarar con máscara seca si corresponde
      if ( !is.null(dry_mask_df) ) {
        data_df <- data_df %>%
          dplyr::left_join(
            dry_mask_df, by = c("longitude", "latitude")) %>%
          dplyr::mutate(
            c_color = ifelse(must_be_masked, mask_color, c_color),
            label_msg = dplyr::case_when(
              must_be_masked ~ masked_txt,
              TRUE ~ as.character(auto_round(value)) ) ) %>%
          dplyr::select(-must_be_masked)
      }
      
      # Identificar distancia entre puntos a graficar
      dist_between_lon <- min(spatial_dist(data_df$longitude)) / 2
      dist_between_lat <- min(spatial_dist(data_df$latitude)) / 2
      if (!gridded_data) {
        min_distance <- min(dist_between_lon, dist_between_lat)
        circle_radius <- ifelse(min_distance < 2, 3, 5)
      }
      
      # Invertir colores en escala, cuando sea necesario
      if ( rev_legend ) {
        colors <- rev(colors)
        breaks <- rev(breaks)
      }
      
      # Modificar colores según sea necesario
      if (include_first)
        colors <- c("rgba(255, 255, 255, 0)", colors)
      if (include_last)
        colors <- c(colors, "rgba(255, 255, 255, 0)")

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
            popup = ~ glue::glue("<center>Lon: {longitude}, Lat: {latitude}</center>",
                                 "<center>Val: {label_msg}</center>"),
            label = ~ label_msg)
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
            popup = ~ glue::glue("<center>Lon: {longitude}, Lat: {latitude}</center>",
                                 "<center>Val: {label_msg}</center>"),
            label = ~ label_msg)
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
              .x = if (!is.null(dry_mask_df)) c(no_data_txt_short, masked_txt_short) else no_data_txt_short, 
              .f = ~ paste0("<i style='opacity: .9; white-space: nowrap; width: fit-content;'>", .x, "</i>"))),
          title = htmltools::HTML(legend_title),
          className = if (include_first) "info legend principal include-first" else "info legend principal",
          opacity = 0.9) %>%
        leaflet::addControl(
          html = htmltools::tags$div(
            htmltools::HTML(stringr::str_replace_all(main_title, '\n', '<br>'))),
          position = "topright",
          className="map-title info") %>%
        PlotsHelper$agregar_logos(
          mapa_leaflet = ., data_type = data_type) %>%
        leaflet.extras2::addEasyprint(
          options = leaflet.extras2::easyprintOptions(
            title = switch(lang, "en" = "Download map as PNG", 
                           "es" = "Descargar mapa como PNG", 
                           "pt" = "Baixar mapa como PNG"),
            sizeModes = list("A4Portrait", "A4Landscape"),
            exportOnly = TRUE,
            hideControlContainer = FALSE,
            filename = download_file_basename)) %>%
        htmlwidgets::prependContent(
          PlotsHelper$definir_estilos_css()) %>%
        htmlwidgets::onRender(
          PlotsHelper$definir_javascript())
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
                                  main_title, legend_title, data_type, lang,
                                  output_file_abspath, download_file_basename,
                                  dry_mask_df,
                                  breaks = NULL, colors_below = NULL,
                                  colors_normal = NULL, colors_above = NULL,
                                  save_map = TRUE) {
      
      # Definir texto a ser utilizados
      txt_prob_below = switch(lang, "en" = "Prob. Below", 
                              "es" = "Prob. Inferior",
                              "pt" = "Prob. Abaixo")
      txt_prob_normal = switch(lang, "en" = "Prob. Normal", 
                               "es" = "Prob. Normal",
                               "pt" = "Prob. Normal")
      txt_prob_above = switch(lang, "en" = "Prob. Above", 
                              "es" = "Prob. Superior",
                              "pt" = "Prob. Acima")
      no_cat_txt <- switch(lang, "en" = "No category", 
                           "es" = "Sin categoría", 
                           "pt" = "Sem categoria")
      unknown_cat_txt <- switch(lang, "en" = "Unknown category!", 
                                "es" = "Categoría desconocida!", 
                                "pt" = "Categoria desconhecida!")
      no_data_txt <- switch(lang, "en" = "No data", 
                            "es" = "Sin datos", 
                            "pt" = "Sem dados")
      no_data_txt_short <- switch(lang, "en" = "No data", 
                                  "es" = "Sin datos", 
                                  "pt" = "Sem dados")
      masked_txt <- switch(lang, "en" = "Masked, Dry Season", 
                           "es" = "Enmascarada, Estación Seca", 
                           "pt" = "Mascarada, Estação Seca")
      masked_txt_short <- switch(lang, "en" = "Dry Season", 
                                 "es" = "Estación Seca", 
                                 "pt" = "Estação Seca")
      txt_leg_lbl_below = switch(lang, "en" = "Prob. \"Below Normal\" (%)", 
                                 "es" = "Prob. \"Inferior a lo Normal\" (%)",
                                 "pt" = "Prob. \"Abaixo do Normal\" (%)")
      txt_leg_lbl_normal = switch(lang, "en" = "Prob. \"Normal\" (%)", 
                                  "es" = "Prob. \"Normal\" (%)",
                                  "pt" = "Prob. \"Normal\" (%)")
      txt_leg_lbl_above = switch(lang, "en" = "Prob. \"Above Normal\" (%)", 
                                 "es" = "Prob. \"Superior a lo Normal\" (%)",
                                 "pt" = "Prob. \"Acima do Normal\" (%)")
        
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
            TRUE ~ NA_character_),
          label_msg = dplyr::case_when(
            category == 'below' ~ paste0(txt_prob_below, ": ", as.character(unlist(auto_round(prob_below))*100), " %"),
            category == 'normal' ~ paste0(txt_prob_normal, ": ", as.character(unlist(auto_round(prob_normal))*100), " %"),
            category == 'above' ~ paste0(txt_prob_above, ": ", as.character(unlist(auto_round(prob_above))*100), " %"),
            is.na(category) & (!is.na(prob_below) | !is.na(prob_normal) | !is.na(prob_above)) ~ no_cat_txt,
            is.na(category) & is.na(prob_below) & is.na(prob_normal) & is.na(prob_above) ~ no_data_txt,
            TRUE ~ unknown_cat_txt),
          popup_msg = paste0(txt_prob_below, ": ",  ifelse(is.na(prob_below), no_data_txt, as.character(unlist(auto_round(prob_below))*100)), " % <br>",
                             txt_prob_normal, ": ", ifelse(is.na(prob_normal), no_data_txt, as.character(unlist(auto_round(prob_normal))*100)), " % <br>",
                             txt_prob_above, ": ",  ifelse(is.na(prob_above), no_data_txt, as.character(unlist(auto_round(prob_above))*100)), " % <br>") )
      
      # Enmascarar con máscara seca si corresponde
      if ( !is.null(dry_mask_df) ) {
        data_df <- data_df %>%
          dplyr::left_join(
            dry_mask_df, by = c("longitude", "latitude")) %>%
          dplyr::mutate(
            c_color = ifelse(must_be_masked, mask_color, c_color),
            label_msg = dplyr::case_when(
              must_be_masked ~ glue::glue("Prob. {masked_txt}"),
              TRUE ~ label_msg),
            popup_msg = dplyr::case_when(
              must_be_masked ~ glue::glue("{txt_prob_below}: {masked_txt} <br>",
                                          "{txt_prob_normal}: {masked_txt} <br>",
                                          "{txt_prob_above}: {masked_txt} <br>"),
              TRUE ~ popup_msg)) %>%
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
            popup = ~ glue::glue("<center>Lon: {longitude}, Lat: {latitude}</center> 
                                 {popup_msg}"),
            label = ~ label_msg)
          else . } %>%
        { if ( !gridded_data ) 
          leaflet::addCircleMarkers(
            map = .,
            data = data_df,
            radius = circle_radius,
            stroke = FALSE,
            fillColor = ~ c_color,
            fillOpacity = 0.7,
            popup = ~ glue::glue("<center>Lon: {longitude}, Lat: {latitude}</center> 
                                 {popup_msg}"),
            label = ~ label_msg)
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
                       .x = if (!is.null(dry_mask_df)) c(na_color_prob, mask_color) else na_color_prob, 
                       .f = ~ glue::glue('{.x};border-radius: 25px;margin-top:3px;'))),
          labels = c(glue::glue("<b style='margin-left:-20px'> {txt_leg_lbl_below} </b>"), "",
                     purrr::map_chr(
                       .x = breaks, 
                       .f = ~ paste0("<i style='opacity: .9; margin-top: -9px;'>", .x*100, "</i>")),
                     glue::glue("<b style='margin-left:-20px'> {txt_leg_lbl_normal} </b>"), "",
                     purrr::map_chr(
                       .x = breaks, 
                       .f = ~ paste0("<i style='opacity: .9; margin-top: -9px;'>", .x*100, "</i>")),
                     glue::glue("<b style='margin-left:-20px'> {txt_leg_lbl_above} </b>"), "",
                     purrr::map_chr(
                       .x = breaks, 
                       .f = ~ paste0("<i style='opacity: .9; margin-top: -9px;'>", .x*100, "</i>")),
                     purrr::map_chr(
                       .x = if (!is.null(dry_mask_df)) c(no_data_txt_short, masked_txt_short) else no_data_txt_short, 
                       .f = ~ paste0("<i style='opacity: .9; white-space: nowrap; width: fit-content;'>", .x, "</i>"))),
          title = htmltools::HTML(legend_title),
          className = "info legend principal",
          opacity = 0.9) %>%
        { if ( !is.na(main_title) && !is.null(main_title) && main_title != "" ) 
          leaflet::addControl(.,
            html = htmltools::tags$div(
              htmltools::HTML(stringr::str_replace_all(main_title, '\n', '<br>'))),
            position = "topright",
            className="map-title info")
          else . } %>%
        PlotsHelper$agregar_logos(
          mapa_leaflet = ., data_type = data_type) %>%
        leaflet.extras2::addEasyprint(
          options = leaflet.extras2::easyprintOptions(
            title = switch(lang, "en" = "Download map as PNG", 
                           "es" = "Descargar mapa como PNG", 
                           "pt" = "Baixar mapa como PNG"),
            sizeModes = list("A4Portrait", "A4Landscape"),
            exportOnly = TRUE,
            hideControlContainer = FALSE,
            filename = download_file_basename)) %>%
        htmlwidgets::prependContent(
          PlotsHelper$definir_estilos_css()) %>%
        htmlwidgets::onRender(
          PlotsHelper$definir_javascript())
      # Guardar mapa
      if ( save_map )
        htmlwidgets::saveWidget(
          widget = m, 
          file = output_file_abspath, 
          selfcontained = TRUE)
      # Retornar mapa (para pruebas)
      return ( m )
    },
    agregar_logos = function(mapa_leaflet, data_type) {
      mapa_leaflet <- mapa_leaflet %>%
        { if ( file.exists(global_images$crcsas) ) 
          leaflet::addControl(.,
                              html = GenerarHTMLLogo(
                                global_images$crcsas, "CRC-SAS"), 
                              position = "bottomleft",
                              className = "info legend logos crcsas")
          else . } %>%
          { if ( data_type == 'ereg' && file.exists(global_images$smn) ) 
            leaflet::addControl(.,
                                html = GenerarHTMLLogo(
                                  global_images$smn, "SMN"), 
                                position = "bottomleft",
                                className = "info legend logos smn")
            else . } %>%
          { if ( data_type == 'ereg' && file.exists(global_images$cima) ) 
              leaflet::addControl(.,
                                  html = GenerarHTMLLogo(
                                    global_images$cima, "CIMA CONICET"), 
                                  position = "bottomleft",
                                  className = "info legend logos cima")
            else . } %>%
          { if ( data_type == 'ereg' && file.exists(global_images$climar) )
              leaflet::addControl(.,
                                  html = GenerarHTMLLogo(
                                    global_images$climar, "CLIMAR"),
                                  position = "bottomleft",
                                  className = "info legend logos climar")
            else . }
    },
    definir_estilos_css = function() {
      # Generar el gráfico
      # Para usar insertado el svg descargado de fontawesome, use esta web: https://yoksel.github.io/url-encoder/
      css_fix_1 <- 
        ".info.legend.principal {background: rgba(255, 255, 255, 0.5) !important; width: fit-content;}"
      css_fix_2 <- 
        ".leaflet-top .leaflet-control {margin-top: 20px;}"
      css_fix_3 <- 
        ".info.legend.principal.include-first div {margin-bottom: -7px !important;}"
      css_title <- 
        ".leaflet-control.map-title {text-align: center; padding-left: 10px; padding-right: 10px; font-weight: bold; font-size: 14px; line-height: 15px; margin-top: 17px !important;}"
      css_logos <- 
        ".logos {background: rgba(255, 255, 255, 0.5) !important;}"
      css_logo_climar <-
        ".logos.climar img {width: 70px; height: 66px;}"
      css_logo_cima <- 
        ".logos.cima img {width: 70px; height: 63px;}"
      css_logo_smn <- 
        ".logos.smn img {width: 70px; height: 70px;}"
      css_logo_crcsas <- 
        ".logos.crcsas img {width: 70px; height: 78px;}"
      css_hide_button <-
        "#hide-leyend-button {position: absolute; bottom: 0px; right: 50px; z-index: 400; background-repeat: no-repeat;}"
      css_fa_eye <-
        ".fa-eye {margin-right: 1px; width: 17px; height: 17px; background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 576 512'%3E%3Cpath d='M288 32c-80.8 0-145.5 36.8-192.6 80.6C48.6 156 17.3 208 2.5 243.7c-3.3 7.9-3.3 16.7 0 24.6C17.3 304 48.6 356 95.4 399.4C142.5 443.2 207.2 480 288 480s145.5-36.8 192.6-80.6c46.8-43.5 78.1-95.4 93-131.1c3.3-7.9 3.3-16.7 0-24.6c-14.9-35.7-46.2-87.7-93-131.1C433.5 68.8 368.8 32 288 32zM432 256c0 79.5-64.5 144-144 144s-144-64.5-144-144s64.5-144 144-144s144 64.5 144 144zM288 192c0 35.3-28.7 64-64 64c-11.5 0-22.3-3-31.6-8.4c-.2 2.8-.4 5.5-.4 8.4c0 53 43 96 96 96s96-43 96-96s-43-96-96-96c-2.8 0-5.6 .1-8.4 .4c5.3 9.3 8.4 20.1 8.4 31.6z'/%3E%3C/svg%3E\");}"
      css_fa_eye_slash <-
        ".fa-eye-slash {margin-bottom: -2px; width: 19px; height: 19px; background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 640 512'%3E%3Cpath d='M38.8 5.1C28.4-3.1 13.3-1.2 5.1 9.2S-1.2 34.7 9.2 42.9l592 464c10.4 8.2 25.5 6.3 33.7-4.1s6.3-25.5-4.1-33.7L525.6 386.7c39.6-40.6 66.4-86.1 79.9-118.4c3.3-7.9 3.3-16.7 0-24.6c-14.9-35.7-46.2-87.7-93-131.1C465.5 68.8 400.8 32 320 32c-68.2 0-125 26.3-169.3 60.8L38.8 5.1zM223.1 149.5C248.6 126.2 282.7 112 320 112c79.5 0 144 64.5 144 144c0 24.9-6.3 48.3-17.4 68.7L408 294.5c5.2-11.8 8-24.8 8-38.5c0-53-43-96-96-96c-2.8 0-5.6 .1-8.4 .4c5.3 9.3 8.4 20.1 8.4 31.6c0 10.2-2.4 19.8-6.6 28.3l-90.3-70.8zm223.1 298L373 389.9c-16.4 6.5-34.3 10.1-53 10.1c-79.5 0-144-64.5-144-144c0-6.9 .5-13.6 1.4-20.2L83.1 161.5C60.3 191.2 44 220.8 34.5 243.7c-3.3 7.9-3.3 16.7 0 24.6c14.9 35.7 46.2 87.7 93 131.1C174.5 443.2 239.2 480 320 480c47.8 0 89.9-12.9 126.2-32.5z'/%3E%3C/svg%3E\");}"
      # Convert CSS to HTML
      html_fix <- htmltools::tags$style(
        type = "text/css", paste(css_fix_1, css_fix_2, css_fix_3, css_title, css_logos,
                                 css_logo_climar, css_logo_cima, css_logo_smn, css_logo_crcsas,
                                 css_hide_button, css_fa_eye, css_fa_eye_slash))
    },
    definir_javascript = function() {
      js <- "
          function() { 
            var map = this;
            
            // create the button object 
            var button = document.createElement('div');
            
            // set button properties
            button.id = 'hide-leyend-button';
            button.style.visibility = 'visible';
            button.classList.add('fa-eye-slash');
            
            // mark elements as visible
            for (let x of document.getElementsByClassName('info legend principal')) {
              x.style.visibility = 'visible';
            }
            for (let x of document.getElementsByClassName('map-title info leaflet-control')) {
              x.style.visibility = 'visible';
            }
            
            // add onclik function to button
            button.onclick = function () {
              // change eye button
              if (this.classList.contains('fa-eye-slash')) {
                this.classList.remove('fa-eye-slash');
                this.classList.add('fa-eye');
              } else {
                this.classList.remove('fa-eye');
                this.classList.add('fa-eye-slash');
              }
              // hidde or show legend and title
              for (let x of document.getElementsByClassName('info legend principal')) {
                if (x.style.visibility == 'visible') {
                  x.style.visibility = 'hidden';
                } else {
                  x.style.visibility = 'visible';
                }
              }
              for (let x of document.getElementsByClassName('map-title info leaflet-control')) {
                if (x.style.visibility == 'visible') {
                  x.style.visibility = 'hidden';
                } else {
                  x.style.visibility = 'visible';
                }
              }
            }
            
            // add the button to the body
            document.body.appendChild(button);
         }"
    },
    definir_titulo = function(data_type, base_file, lang, data_year = NULL) {
      
      meses_abb <- switch(lang, "en" = month.abb, 
                          "es" = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                   "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"), 
                          "pt" = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                   "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
      meses_name <- switch(lang, "en" = month.name, 
                           "es" = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"), 
                           "pt" = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                                    "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"))
      
      variable <- base_file$variable
      variable_str <- ifelse(variable == 'prcp', 
                             switch(lang, "en" = "Precipitation", 
                                    "es" = "Precipitación", 
                                    "pt" = "Precipitação"), 
                             switch(lang, "en" = "Temperature", 
                                    "es" = "Temperatura",
                                    "pt" = "Temperatura"))
      variable_unit <- ifelse(variable == 'prcp', 'mm', '°C')
      
      # Identificar modelo en pronósticos CRC-SAS-CPT
      modelo <- stringr::str_extract(base_file$basename, cpt_regex_modelos)
      # Si no se obtiene nada, el prono debe ser un prono Climar
      if ( is.na(modelo) )
        modelo <- stringr::str_extract(base_file$basename, ereg_regex_modelos)
      # Los nombres de los modelos siempre va en mayúsculas
      modelo <- toupper(modelo)
      # El modelo MME de los pronos Climar, debe ser renombrado
      if ( toupper(modelo) == "MME" )
        modelo <- switch(lang, "en" = "NMME individual models",
                         "es" = "modelos individuales del NMME",
                         "pt" = "modelos individuais do NMME")
      
      initial_month <- meses_abb[global_ic$month]
      initial_year <- global_ic$year
      
      valid_months <- stringr::str_split(base_file$target_months, '-') %>% 
        purrr::reduce(c) %>% as.numeric()
      
      forecast_months_str <- ifelse(
        length(valid_months) == 1, meses_name[valid_months[[1]]], 
        paste0(meses_abb[valid_months[[1]]], '-', meses_abb[valid_months[[3]]]))
      
      if ( !is.null(data_year) ) {
        month_year <- glue::glue("{meses_abb[valid_months[[1]]]} {data_year}")
        if (length(valid_months) > 1) {
          if (valid_months[[1]] > valid_months[[3]]) {
            month_year <- glue::glue("{month_year} - {meses_abb[valid_months[[3]]]} {data_year+1}")
          } else {
            month_year <- glue::glue("{month_year} - {meses_abb[valid_months[[3]]]} {data_year}")
          }
        } 
      }
      
      
      # Definir título por defecto
      main_title <- switch(lang, "en" = "Undefined title", 
                           "es" = "Título no definido",
                           "pt" = "Título indefinido")
      
      # Definir elementos según idioma
      valid_for <- switch(lang, "en" = "Valid for", 
                          "es" = "Válido para",
                          "pt" = "Valido para")
      issued <- switch(lang, "en" = "Issued on", 
                       "es" = "Emitido en",
                       "pt" = "Emitido o")
      issued_prob_xtrm <- switch(lang, "en" = "Forecast issued in", 
                                 "es" = "Pronóstico emitido en",
                                 "pt" = "Previsão divulgada em")
      calibrated <- switch(lang, "en" = "Calibrated model:", 
                           "es" = "Modelo calibrado:",
                           "pt" = "Modelo calibrado:")
      calibrated_prob_xtrm <- switch(lang, "en" = "Calibration of NMME model predictions", 
                                     "es" = "Calibración de predicciones del NMME",
                                     "pt" = "Calibração das previsões do NMME")
      uncalibrated <- switch(lang, "en" = "Uncalibrated model", 
                             "es" = "Modelo sin calibrar",
                             "pt" = "Modelo sem calibrar")
      
      # Definir título real
      if (data_type == "anom") {
        anomaly_desc <- switch(lang, "en" = "Anomaly forecast for", 
                               "es" = "Pronóstico de Anomalías para",
                               "pt" = "Previsão de Anomalia para")
        main_title <- glue::glue("{anomaly_desc} {tolower(variable_str)} ({variable_unit}). ",
                                 "\n{valid_for} {month_year}. ",
                                 "{issued} {initial_month} {initial_year}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "corr") {
        correlation_desc <- switch(lang, "en" = "Correlation between Forecast and Observation", 
                                   "es" = "Correlación entre Pronóstico y Observación",
                                   "pt" = "Correlação entre Previsão e Observação")
        main_title <- glue::glue("{correlation_desc} ({base_file$hcst_first_year}-{base_file$hcst_last_year}). ",
                                 "\n{valid_for} {forecast_months_str}. ",
                                 "{issued} {initial_month}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "det.fcst") {
        det_fcst_desc <- switch(lang, "en" = "Deterministic forecast for", 
                                "es" = "Pronóstico determinístico para",
                                "pt" = "Previsão determinística para")
        main_title <- glue::glue("{det_fcst_desc} {tolower(variable_str)} ({variable_unit}). ",
                                 "\n{valid_for} {month_year}. ",
                                 "{issued} {initial_month} {initial_year}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "prob.fcst") {
        prob_fcst_desc <- switch(lang, "en" = "Probabilistic forecast for the most probable category of", 
                                 "es" = "Pronóstico probabilístico para la categoría más probable de",
                                 "pt" = "Previsão probabilística para a categoria mais provável de")
        main_title <- glue::glue("{prob_fcst_desc} {tolower(variable_str)}. ",
                                 "\n{valid_for} {month_year}. ",
                                 "{issued} {initial_month} {initial_year}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "prob.below.33") {
        prob_fcst_desc <- switch(lang, "en" = "Probabilistic forecast for", 
                                 "es" = "Pronóstico probabilístico para",
                                 "pt" = "Previsão probabilística para")
        prob_below_33_desc <- switch(lang, "en" = "below 33rd precentil of the historical distribution",
                                     "es" = "inferior al percentil 33 de la distribución histórica",
                                     "pt" = "abaixo do percentil 33 da distribuição histórica")
        main_title <- glue::glue("{prob_fcst_desc} {tolower(variable_str)} {prob_below_33_desc}. ",
                                 "\n{valid_for} {month_year}. ",
                                 "{issued} {initial_month} {initial_year}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "prob.above.66") {
        prob_fcst_desc <- switch(lang, "en" = "Probabilistic forecast for", 
                                 "es" = "Pronóstico probabilístico para",
                                 "pt" = "Previsão probabilística para")
        prob_above_66_desc <- switch(lang, "en" = "above 66th precentil of the historical distribution",
                                     "es" = "superior al percentil 66 de la distribución histórica",
                                     "pt" = "acima do percentil 66 da distribuição histórica")
        main_title <- glue::glue("{prob_fcst_desc} {tolower(variable_str)} {prob_above_66_desc}. ",
                                 "\n{valid_for} {month_year}. ",
                                 "{issued} {initial_month} {initial_year}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "uncal.fcst") {
        uncal_fcst_desc <- switch(lang, "en" = "Original Forecast for", 
                                  "es" = "Pronóstico original para",
                                  "pt" = "Previsão original para")
        main_title <- glue::glue("{uncal_fcst_desc} {tolower(variable_str)} ({variable_unit}). ",
                                 "\n{valid_for} {month_year}. ",
                                 "{issued}: {initial_month} {initial_year}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "prob.below.20") {
        prob_fcst_desc <- switch(lang, "en" = "Probabilistic forecast for", 
                                 "es" = "Pronóstico probabilístico para",
                                 "pt" = "Previsão probabilística para")
        prob_below_20_desc <- switch(lang, "en" = "below 20th precentil of the historical distribution", 
                                     "es" = "inferior al percentil 20 de la distribución histórica",
                                     "pt" = "abaixo do percentil 20 da distribuição histórica")
        main_title <- glue::glue("{prob_fcst_desc} {tolower(variable_str)} {prob_below_20_desc}. ",
                                 "\n{valid_for} {month_year}. ",
                                 "{issued} {initial_month} {initial_year}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "prob.above.80") {
        prob_fcst_desc <- switch(lang, "en" = "Probabilistic forecast for", 
                                 "es" = "Pronóstico probabilístico para",
                                 "pt" = "Previsão probabilística para")
        prob_above_80_desc <- switch(lang, "en" = "above 80th precentil of the historical distribution", 
                                     "es" = "superior al percentil 80 de la distribución histórica",
                                     "pt" = "acima do percentil 80 da distribuição histórica")
        main_title <- glue::glue("{prob_fcst_desc} {tolower(variable_str)} {prob_above_80_desc}. ",
                                 "\n{valid_for} {month_year}. ",
                                 "{issued} {initial_month} {initial_year}. ",
                                 "\n{calibrated} {modelo}.")
      } else if (data_type == "prob.xtrm.dry") {
        prob_fcst_desc <- switch(lang, "en" = "Probability of extremely dry conditions", 
                                 "es" = "Probabilidad de condiciones extremadamente secas",
                                 "pt" = "Possibilidade de condições extremamente secas")
        main_title <- glue::glue("{prob_fcst_desc}. ",
                                 "\n{issued_prob_xtrm} {initial_month} {initial_year}. ",
                                 "\n{calibrated_prob_xtrm}.")
      } else if (data_type == "prob.xtrm.wet") {
        prob_fcst_desc <- switch(lang, "en" = "Probability of extremely wet conditions", 
                                 "es" = "Probabilidad de condiciones extremadamente húmedas",
                                 "pt" = "Possibilidade de condições extremamente úmidas")
        main_title <- glue::glue("{prob_fcst_desc}. ",
                                 "\n{issued_prob_xtrm} {initial_month} {initial_year}. ",
                                 "\n{calibrated_prob_xtrm}.")
      } else if (data_type == "prob.xtrm.hot") {
        prob_fcst_desc <- switch(lang, "en" = "Probability of extremely hot conditions", 
                                 "es" = "Probabilidad de condiciones extremadamente cálidas",
                                 "pt" = "Possibilidade de condições extremamente quentes")
        main_title <- glue::glue("{prob_fcst_desc}. ",
                                 "\n{issued_prob_xtrm} {initial_month} {initial_year}. ",
                                 "\n{calibrated_prob_xtrm}.")
      } else if (data_type == "prob.xtrm.cold") {
        prob_fcst_desc <- switch(lang, "en" = "Probability of extremely cold conditions", 
                                 "es" = "Probabilidad de condiciones extremadamente frías",
                                 "pt" = "Possibilidade de condições extremamente frias")
        main_title <- glue::glue("{prob_fcst_desc}. ",
                                 "\n{issued_prob_xtrm} {initial_month} {initial_year}. ",
                                 "\n{calibrated_prob_xtrm}.")
      } 
      
      # Retornar título real
      return ( main_title )
      
    },
    definir_titulo_leyenda = function(data_type, base_file, lang) {
      
      variable <- base_file$variable
      variable_str <- ifelse(variable == 'prcp', 
                             switch(lang, "en" = "Precipitation", 
                                    "es" = "Precipitación", 
                                    "pt" = "Precipitação"), 
                             switch(lang, "en" = "Temperature 2m", 
                                    "es" = "Temperatura 2m",
                                    "pt" = "Temperatura 2m"))
      variable_unit <- ifelse(variable == 'prcp', 'mm', '°C')
      
      probabilidad <- switch(lang, "en" = "Probability", 
                             "es" = "Probabilidad", 
                             "pt" = "Probabilidade")
      
      # Definir título por defecto
      legend_title <- switch(lang, "en" = "", "es" = "", "pt" = "")
      
      # Definir título real
      if (data_type == "anom") {
        anomaly_desc <- switch(lang, "en" = "Anomaly", 
                               "es" = "Anomalía",
                               "pt" = "Anomalia")
        legend_title <- glue::glue("{anomaly_desc} ({variable_unit})")
      } else if (data_type == "corr") {
        correlation_desc <- switch(lang, "en" = "Correlation", 
                                   "es" = "Correlación",
                                   "pt" = "Correlação")
        legend_title <- glue::glue("{correlation_desc}")
      } else if (data_type == "det.fcst") {
        legend_title <- glue::glue("{variable_str} ({variable_unit})")
      } else if (data_type == "prob.fcst") {
        legend_title <- glue::glue("")
      } else if (data_type == "prob.below.33") {
        legend_title <- glue::glue("{probabilidad} (%)")
      } else if (data_type == "prob.above.66") {
        legend_title <- glue::glue("{probabilidad} (%)")
      } else if (data_type == "uncal.fcst") {
        legend_title <- glue::glue("{variable_str} ({variable_unit})")
      } else if (data_type == "prob.below.20") {
        legend_title <- glue::glue("{probabilidad} (%)")
      } else if (data_type == "prob.above.80") {
        legend_title <- glue::glue("{probabilidad} (%)")
      } else if (data_type == "prob.xtrm.dry") {
        legend_title <- glue::glue("{probabilidad} (%)")
      } else if (data_type == "prob.xtrm.wet") {
        legend_title <- glue::glue("{probabilidad} (%)")
      } else if (data_type == "prob.xtrm.hot") {
        legend_title <- glue::glue("{probabilidad} (%)")
      } else if (data_type == "prob.xtrm.cold") {
        legend_title <- glue::glue("{probabilidad} (%)")
      }
      
      # Retornar título real
      return ( legend_title )
      
    },
    definir_nombre_base_aux = function(data_type, base_file, lang, data_year = NULL) {
      
      # Definir nombre base por defecto
      nombre_base <- paste0(
        base_file$basename, "_", gsub("\\.", "_", data_type), "_", lang)
      
      # Definir nombre base final
      if (data_type == "anom") {
        invisible()
      } else if (data_type == "corr") {
        invisible()
      } else if (data_type == "det.fcst") {
        invisible()
      } else if (data_type == "prob.fcst") {
        invisible()
      } else if (data_type == "prob.below.33") {
        invisible()
      } else if (data_type == "prob.above.66") {
        invisible()
      } else if (data_type == "uncal.fcst") {
        invisible()
      } else if (data_type == "prob.below.20") {
        invisible()
      } else if (data_type == "prob.above.80") {
        invisible()
      } else if (data_type == "prob.xtrm.dry") {
        invisible()
      } else if (data_type == "prob.xtrm.wet") {
        invisible()
      } else if (data_type == "prob.xtrm.hot") {
        invisible()
      } else if (data_type == "prob.xtrm.cold") {
        invisible()
      }
      
      # Retornar nombre base final
      return ( nombre_base )
      
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
PlotsHelper$agregar_logos = function(...) {
  PlotsHelper$public_methods$agregar_logos(...)
}
PlotsHelper$definir_estilos_css = function(...) {
  PlotsHelper$public_methods$definir_estilos_css(...)
}
PlotsHelper$definir_javascript = function(...) {
  PlotsHelper$public_methods$definir_javascript(...)
}
PlotsHelper$definir_titulo = function(...) {
  PlotsHelper$public_methods$definir_titulo(...)
}
PlotsHelper$definir_titulo_leyenda = function(...) {
  PlotsHelper$public_methods$definir_titulo_leyenda(...)
}
PlotsHelper$definir_nombre_base_aux = function(...) {
  PlotsHelper$public_methods$definir_nombre_base_aux(...)
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
      if ( !are_points_joinables(det_data_df, obs_data_df) ) {
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

