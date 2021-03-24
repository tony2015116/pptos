#' location_monitor
#'
#' @param data all csv data of locations
#' @param station_type nedap or fire stations
#' @param path_out the path to receive results
#'
#' @return pictures of each locations
#' @export
#'
#' @examples
#' #station_monitor(data = test, station_type = "nedap", path_out = "")
station_monitor <- function(data, station_type, path_out) {
  visit_time <- responder <- . <- location <- animal_number <- duration <- feed_intake <- Entry <-
    Exit <- Consumed <- weight <- ndt <- . <- items <- plot1 <- NULL
  if (station_type == "nedap") {
    temp1 <- data.table::copy(setDT(data))
    temp2 <-
      unique(temp1)[, c("date", "time") := data.table::tstrsplit(visit_time, " ", fixed =
                                                                   TRUE)][, c("date") := lubridate::ymd(date)][, !c("visit_time", "time")]
    temp3 <-
      unique(temp2, by = c("location", "responder", "date"))[!is.na(responder)][, keyby = .(location, date), .(animal_number = .N)]
    temp4 <- temp2[!is.na(animal_number)][, keyby = .(location, date), .(
      `total_intake_duration(min)` = round(sum(duration) / 60, digits = 4),
      total_intake = round(sum(feed_intake) /
                             1000, digits = 4),
      visit_number = .N
    )]
  }
  if (station_type == "fire") {
    temp1 <-
      data.table::copy(setDT(data))[, Entry := do.call(paste, c(.SD, sep = " ")), .SDcol =
                                      c("Date", "Entry")][, Exit := do.call(paste, c(.SD, sep = " ")), .SDcol =
                                                            c("Date", "Exit")][, c("Entry", "Exit") := lapply(.SD, lubridate::ymd_hms), .SDcol =
                                                                                 c("Entry", "Exit")][, duration := data.table::fifelse(Exit - Entry < 0 &
                                                                                                                                         hour(Exit) == 0,
                                                                                                                                       Exit - Entry + lubridate::ddays(1),
                                                                                                                                       Exit - Entry)]
    data.table::setnames(temp1,
                         c(1:3, 9),
                         c("location", "responder", "date", "weight"))
    temp2 <- unique(temp1)[, date := lubridate::ymd(date)]
    temp3 <-
      unique(temp2, by = c("location", "responder", "date"))[!is.na(responder)][, keyby = .(location, date), .(animal_number = .N)]
    temp4 <- temp2[!is.na(responder)][, duration := as.numeric(duration)][, keyby = .(location, date), .(
      `total_intake_duration(min)` = round(sum(duration) / 60, digits = 4),
      total_intake = round(sum(Consumed), digits = 4),
      visit_number = .N
    )]
  }
  temp5 <- merge(temp3, temp4, all.x = TRUE)
  temp6 = data.table::melt(
    temp5,
    id.vars = c("location", "date"),
    variable.name = "items",
    value.name = "values"
  )
  temp6_1 <- temp2[, .(location, date, weight)]
  temp6_2 <- temp6_1 %>%
    tidyfst::nest_dt(location) %>%
    tidyfst::mutate_dt(ndt = purrr::map(ndt, function(data) {
      data[CJ(date = tidyr::full_seq(date, 1)), on = .(date)][CJ(date = date, unique =
                                                                   TRUE), on = .(date)]
    })) %>%
    tidyfst::mutate_dt(
      plot1 = purrr::map2(
        ndt,
        location,
        ~ ggplot2::ggplot(data = .x, ggplot2::aes(
          x = date, y = weight, group = date
        )) +
          ggplot2::geom_boxplot(outlier.color = "red") +
          cowplot::background_grid(minor = "none") +
          ggplot2::scale_x_date(date_breaks = "1 day", date_labels =  "%d") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            legend.position = "none",
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = -90),
            axis.text = ggplot2::element_text(size = 8)
          )
      )
    ) %>%
    tidyfst::select_dt(-ndt)

  temp7 <- temp6 %>%
    tidyfst::nest_dt(location) %>%
    tidyfst::mutate_dt(ndt = purrr::map(ndt, function(data) {
      data[CJ(date = tidyr::full_seq(date, 1)), on = .(date)][CJ(date = date,
                                                                 items = items,
                                                                 unique = TRUE), on = .(date, items)][!is.na(items)][, items := factor(items,
                                                                                                                                       labels = c("N", "visit_time/min", "feed_intake/kg", "visit_number"))]
    })) %>%
    tidyfst::mutate_dt(
      plot = purrr::map2(
        ndt,
        location,
        ~ ggplot2::ggplot(data = .x, ggplot2::aes(x = date, y = values)) +
          ggplot2::geom_point(ggplot2::aes(col = items)) +
          ggplot2::geom_line(ggplot2::aes(col = items)) +
          ggplot2::theme_bw() +
          ggplot2::facet_grid(items ~ ., scales = "free") +
          ggplot2::scale_x_date(date_breaks = "1 day", date_labels =  "%m") +
          ggplot2::scale_colour_brewer(palette = "Set1") +
          ggplot2::theme(
            strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
            legend.position = "none",
            axis.title = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = -90),
            axis.text = ggplot2::element_text(size = 8),
            strip.placement = "outside",
            strip.background = ggplot2::element_rect(colour = "white", fill = "white")
          ) +
          ggplot2::ggtitle(paste0("Location:", .y)) +
          cowplot::background_grid(minor = "none")
      )
    ) %>%
    tidyfst::select_dt(-ndt)
  temp8 <- tidyfst::left_join_dt(temp7, temp6_2, by = "location")
  temp9 <- temp8 %>%
    dplyr::mutate(finals = purrr::pmap(list(plot, plot1),
                                       ~ patchwork::wrap_plots(..1, ..2, nrow = 2))) %>% tidyfst::mutate_dt(plot_name = paste0(location, "_station.png"))

  purrr::walk2(
    temp9$plot_name,
    temp9$finals,
    ~ ggplot2::ggsave(
      filename = paste0(path_out, .x),
      plot = .y,
      height = 7,
      width = 11
    )
  )
}
