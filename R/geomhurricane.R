library(dplyr)

#' Creates a tribble with a row for for each wind radii per wind direction
#' @importFrom  iterators iter
#' @importFrom  iterators netElem
#' @importFrom  base nrow
#' @importFrom  base as.double
#' @importFrom  base data.frame
#' @importFrom  base rbind
#' @importFrom  base paste
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @param df The tribble containing a row of hurricane data
#' @return A trible consisting of storm_id, datetime, longitude, latitude, wind_speed, ne, nw, se, sw
#' @examples \dontrun{
# > ike_data
# # A tibble: 3 x 9
# storm_id            datetime longitude latitude wind_speed    ne    nw    se    sw
# <chr>               <chr>     <dbl>    <dbl>      <chr> <dbl> <dbl> <dbl> <dbl>
#   1 IKE-2008 2008-09-13 06:00:00     -94.6     29.1         34   225   125   200   125
#   2 IKE-2008 2008-09-13 06:00:00     -94.6     29.1         50   150    75   160    80
#   3 IKE-2008 2008-09-13 06:00:00     -94.6     29.1         64   110    45    90    55
#' }
format_hurricane_data <- function(df) {
  ne <- NULL
  nw <- NULL
  se <- NULL
  sw <- NULL
  storm_id <- NULL
  datetime <- NULL
  wind_speed <- NULL
  longitude <- NULL
  latitude <- NULL
  idf <- iterators::iter(df, by="row")
  limit <- base::nrow(df)
  res <- base::data.frame(row.names = NULL)
  while (limit > 0) {
    limit <- limit - 1
    row <- iterators::nextElem(idf)
    for (r in c(34, 50, 64)) {
      base_row <- row %>%
        dplyr::mutate(wind_speed = as.character(r),
                      ne = base::as.double(row[base::paste("radius", r, "ne", sep="_")]),
                      nw = base::as.double(row[base::paste("radius", r, "nw", sep="_")]),
                      se = base::as.double(row[base::paste("radius", r, "se", sep="_")]),
                      sw = base::as.double(row[base::paste("radius", r, "sw", sep="_")]))
      res <- base::rbind(res, base_row)
    }
  }
  res %>%
    dplyr::select(storm_id, datetime, longitude, latitude, wind_speed, ne, nw, se, sw)
}


#' Creates a tribble with a row for for each wind radii per wind direction
#' based on the given file for the storm name and the specific time.
#' Longitude is negated to -longitude. Year, month, day and hour are combined into a single
#' datetime column of format yyyy-mm-dd hh:00:00
#' @importFrom  base c
#' @importFrom  base paste
#' @importFrom  base rbind
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom readr read_fwf
#' @importFrom readr fwf_widths
#' @param path File path containing the storm data.
#' @param storm_fullname This should be of the format <stormid>-<year> Eg: IKE-2008
#' @param exacttime Complete date / time value to restrict the storm to a single row. Eg: 2008-09-13 06:00:00
#' @return A tribble consisting of storm_id, datetime, longitude, latitude, wind_speed, ne, nw, se, sw
#' @examples \dontrun{
#' ike_data <- read_hurricane_data(paste0(getwd(), "/ebtrk_atlc_1988_2015.txt"), "IKE-2008",
#'  "2008-09-13 06:00:00")
# > ike_data
# # A tibble: 3 x 9
# storm_id            datetime longitude latitude wind_speed    ne    nw    se    sw
# <chr>               <chr>     <dbl>    <dbl>      <chr> <dbl> <dbl> <dbl> <dbl>
#   1 IKE-2008 2008-09-13 06:00:00     -94.6     29.1         34   225   125   200   125
#   2 IKE-2008 2008-09-13 06:00:00     -94.6     29.1         50   150    75   160    80
#   3 IKE-2008 2008-09-13 06:00:00     -94.6     29.1         64   110    45    90    55
#' }
#' @export
read_hurricane_data <- function(path, storm_fullname, exacttime) {
  year <- NULL
  month <- NULL
  day <- NULL
  hour <- NULL
  storm_id <- NULL
  storm_name <- NULL
  datetime <- NULL
  longitute <- NULL
  ext_tracks_widths <- base::c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
  ext_tracks_colnames <- base::c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           base::paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           base::paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           base::paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final")

  ext_tracks <- readr::read_fwf(path,
                         readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99")
  df <- ext_tracks %>%
    dplyr::mutate(storm_id = paste(storm_name, year, sep='-'),
           datetime = base::paste(
             base::paste(year, month, day, sep="-"),
             base::paste(hour, "00", "00", sep=":")
           ),
           longitude = -longitude)
  res <- df %>%
    dplyr::filter(storm_id == storm_fullname, datetime == exacttime)
  format_hurricane_data(res)
}

#' Creates a data.frame with points having (long, lat) as center, and the given wind radius.
#' Points are created between the range of start and end degrees, scaled by the scale factor.
#' This calculation uses geosphere::destPoint. Distance is scaled using 1852 and the scale factor.
#' @importFrom  base rbind
#' @importFrom  base data.frame
#' @importFrom  base colnames
#' @importFrom  base c
#' @importFrom geosphere destPoint
#' @param long Longitude
#' @param lat Latitude
#' @param start_deg Starting degree
#' @param end_deg Ending degree
#' @param radius Maximum wind radius along that quadrant
#' @param clr The color value to be used.
#' @param scale_factor The scaling factor for recalibrating the distance
#' @return A data.frame consisting of x (longitude), y (latitude), colour, fill
#' @examples \dontrun{
# Browse[2]> head(df)
# x        y colour fill
# 1 -94.60000 32.85848    red  red
# 2 -94.52236 32.85788    red  red
# 3 -94.44475 32.85610    red  red
# 4 -94.36719 32.85313    red  red
# 5 -94.28971 32.84897    red  red
# 6 -94.21234 32.84362    red  red
#' }
create_points <- function(long, lat, start_deg, end_deg, radius, clr, fl, scale_factor) {
  points <- base::data.frame(row.names = NULL)
  for (bearing in start_deg:end_deg) {
    loc <- base::c(long, lat)
    point <- geosphere::destPoint(p=loc, b=bearing, d=radius * 1852 * scale_factor)
    points <- base::rbind(points, as.data.frame(point))
  }
  df <- base::data.frame(points, colour = clr, fill = fl)
  base::colnames(df) <- c("x", "y", "colour", "fill")
  df
}

#' Subclassing the Geom class to create the mapping for the hurricane.
#' @importFrom base c
#' @importFrom base rbind
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom ggplot2 proto
#' @importFrom ggplot2 Geom
#' @importFrom dplyr mutate
#' @importFrom grid polygonGrob
#' @importFrom magrittr %>%
#' @importFrom grid gpar
#' @param required_aes Required aesthetics for the plot.
#' @param default_aes Default aesthetics
#' @param draw_key Function to draw the legend.
#' @param draw_group custom callback that processes one row of data per function call.
#' @return An instamce of GeomHurricane for drawing radius based plots.
GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::Geom,
                                  required_aes = base::c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
                                  default_aes = ggplot2::aes(alpha = 1, scale_radii = 1, fill = "red", colour = "red"),
                                  draw_key = ggplot2::draw_key_polygon,
                                  draw_group = function(data, panel_scales, coord) {
                                    df_ne <- create_points(data$x, data$y, 0, 90,
                                                           data$r_ne, data$colour, data$fill, data$scale_radii)
                                    df_se <- create_points(data$x, data$y, 90, 180,
                                                           data$r_se, data$colour, data$fill, data$scale_radii)
                                    df_sw <- create_points(data$x, data$y, 180, 270,
                                                           data$r_sw, data$colour, data$fill, data$scale_radii)
                                    df_nw <- create_points(data$x, data$y, 270, 360,
                                                           data$r_nw, data$colour, data$fill, data$scale_radii)
                                    # change types from factor to character
                                    df <- base::rbind(df_nw, df_ne, df_se, df_sw) %>%
                                      dplyr::mutate(colour = as.character(colour), fill = as.character(fill))
                                    res <- coord$transform(df, panel_scales)
                                    grid::polygonGrob(
                                      x = res$x,
                                      y = res$y,
                                      gp = grid::gpar(col = res$colour, fill = res$fill, alpha = res$alpha)
                                    )
                                  }
)

#' Create an instance of GeomHurricane
#' source from geom_polygon
#' @importFrom ggplot2 layer
#' @param mapping
#' @param data The hurricane data
#' @param stat Statistics object currently not doing any transformations
#' @param position Origin position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @return A layer for plotting the hurricane data.
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data, stat = stat, position = position,
    geom = GeomHurricane, mapping = mapping,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# library(dplyr)
# library(iterators)
# library(readr)
# library(geosphere)
# library(ggplot2)
# library(ggthemes)
# library(ggmap)
#
# setwd("/home/xyz/05_building-new-graphical-elements")
# ike_data <- read_hurricane_data(paste0(getwd(), "/ebtrk_atlc_1988_2015.txt"), "IKE-2008", "2008-09-13 06:00:00")
#
# custom_map5 <- get_map("Louisiana", zoom = 5, maptype = "toner-background") %>% ggmap(extent = "device")
# hurricane_ike <- custom_map5 +
#   geom_hurricane(data = ike_data,
#                  aes(x = longitude, y = latitude,
#                      r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#                      fill = wind_speed, color = wind_speed)) +
#   scale_color_manual(name = "Wind speed (kts)",
#                      values = c("red", "orange", "yellow")) +
#   scale_fill_manual(name = "Wind speed (kts)",
#                     values = c("red", "orange", "yellow"))
#
# jpeg("hurricane_data.jpeg")
# plot(hurricane_ike)
# dev.off()
