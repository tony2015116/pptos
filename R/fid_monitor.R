#' fid_monitor
#'
#' @param data all csv data of locations
#' @param station_type nedap or fire stations
#' @param path_out the path to receive results
#' @param ... ggsave parameters
#'
#' @return pictures of each locations
#' @export
#'
#' @examples
#' #fid_monitor(data = test, station_type = "nedap", path_out = "", width =60, height = 500)
fid_monitor <- function(data, station_type, path_out, ...) {
  visit_time <- . <- location <- responder <- feed_intake <- total_intake <- percent_intake <-
    Date <- Consumed <- ndt <- plot1 <- plot2 <- all_feed_a_station_one_day <- NULL
  if (station_type == "nedap") {
    temp1 <-
      unique(data.table::copy(data.table::setDT(data)))[, c("date", "time") := data.table::tstrsplit(visit_time, " ", fixed =
                                                                   TRUE)][, c("date") := lubridate::ymd(date)][, !c("visit_time", "time")]
    temp2 <-
      temp1[, keyby = .(location, responder, date), .(total_intake = round(sum(feed_intake) /
                                                                             1000, digits = 4))][, all_feed_a_station_one_day := sum(total_intake), by = .(location, date)][, percent_intake := total_intake /
                                                                                                                                                                              all_feed_a_station_one_day]
    to_factor = c("location", "responder")
    temp2[, (to_factor) := map(.SD, as.factor), .SDcols = to_factor]
  } else if (station_type == "fire") {
    temp1 <- unique(data.table::copy(data.table::setDT(data)))[, Date := lubridate::ymd(Date)]
    data.table::setnames(temp1, 1:3, c("location", "responder", "date"))
    temp2 <-
      temp1[, keyby = .(location, responder, date), .(total_intake = round(sum(Consumed), digits = 4))][, all_feed_a_station_one_day := sum(total_intake), by = .(location, date)][, percent_intake := total_intake /
                                                                                                                                                                                     all_feed_a_station_one_day]
    to_factor = c("location", "responder")
    temp2[, (to_factor) := purrr::map(.SD, as.factor), .SDcols = to_factor]
  }
  temp3 <- temp2 %>%
    tidyfst::nest_dt(location) %>%
    tidyfst::mutate_dt(
      plot1 = purrr::map2(
        ndt,
        location,
        ~ ggplot2::ggplot(data = .x, ggplot2::aes(
          y = percent_intake, x = date, fill = responder
        )) +
          ggplot2::theme_bw() +
          ggplot2::geom_col(
            width = 0.8,
            na.rm = F,
            show.legend = T
          ) +
          ggplot2::ggtitle(.y) +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, 1),
            breaks = seq(0, 1, 0.1)
          ) +
          ggplot2::theme(
            plot.title = element_text(
              color = "black",
              hjust = 0.5,
              size = 20
            ),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 8, angle = 270),
            axis.text.y = element_text(size = 8),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 8),
            legend.position = "top"
          ) +
          ggplot2::guides(
            shape = guide_legend(override.aes = list(size = 7)),
            color = guide_legend(override.aes = list(size = 7))
          ) +
          ggplot2::ggtitle(paste0("location:", .y)) +
          ggplot2::scale_x_date(
            date_breaks = "1 day",
            date_labels =  "%m-%d",
            date_minor_breaks = "1 day"
          ) +
          ggplot2::scale_fill_manual(
            na.value = "black",
            values = c(
              "#a6cee3",
              "#1f78b4",
              "#b2df8a",
              "#33a02c",
              "#fb9a99",
              "#e31a1c",
              "#fdbf6f",
              "#ff7f00",
              "#cab2d6",
              "#6a3d9a",
              "#b15928",
              "#8dd3c7",
              "#d9d9d9",
              "#80b1d3",
              "#00AFBB",
              "#01665e",
              "#003c30",
              "blue",
              "pink",
              "yellow",
              "red",
              "green",
              "#999999",
              "#E69F00",
              "#56B4E9",
              "#009E73",
              "#F0E442",
              "#0072B2",
              "#D55E00",
              "#CC79A7",
              "#00AFBB",
              "#E7B800",
              "#FC4E07",
              "#1B9E77",
              "#D95F02",
              "#7570B3",
              "#E7298A",
              "#66A61E",
              "#E6AB02",
              "#A6761D",
              "#666666"
            )
          )
      ),

      plot2 = purrr::map2(
        ndt,
        location,
        ~ ggplot2::ggplot(data = .x, ggplot2::aes(
          y = total_intake, x = date, fill = responder
        )) +
          ggplot2::theme_bw() +
          ggplot2::geom_col(
            width = 0.8,
            na.rm = F,
            show.legend = F
          ) +
          ggplot2::theme(
            plot.title = element_text(
              color = "black",
              hjust = 0.5,
              size = 20
            ),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 8, angle = 270),
            axis.text.y = element_text(size = 8),
            legend.position = "none"
          ) +
          ggplot2::guides(
            shape = guide_legend(override.aes = list(size = 7)),
            color = guide_legend(override.aes = list(size = 7))
          ) +
          ggplot2::scale_x_date(
            date_breaks = "1 day",
            date_labels =  "%m-%d",
            date_minor_breaks = "1 day"
          ) +
          ggplot2::scale_fill_manual(
            na.value = "black",
            values = c(
              "#a6cee3",
              "#1f78b4",
              "#b2df8a",
              "#33a02c",
              "#fb9a99",
              "#e31a1c",
              "#fdbf6f",
              "#ff7f00",
              "#cab2d6",
              "#6a3d9a",
              "#b15928",
              "#8dd3c7",
              "#d9d9d9",
              "#80b1d3",
              "#00AFBB",
              "#01665e",
              "#003c30",
              "blue",
              "pink",
              "yellow",
              "red",
              "green",
              "#999999",
              "#E69F00",
              "#56B4E9",
              "#009E73",
              "#F0E442",
              "#0072B2",
              "#D55E00",
              "#CC79A7",
              "#00AFBB",
              "#E7B800",
              "#FC4E07",
              "#1B9E77",
              "#D95F02",
              "#7570B3",
              "#E7298A",
              "#66A61E",
              "#E6AB02",
              "#A6761D",
              "#666666"
            )
          )
      )
    )

  temp4 <- temp3 %>%
    dplyr::mutate(finals = purrr::pmap(list(plot1, plot2),
                                ~ patchwork::wrap_plots(..1, ..2, ncol = 1)))
  temp5 <- patchwork::wrap_plots(temp4$finals, ncol = 2)
  ggplot2::ggsave(
    paste0(path_out, "feed_intake_monitor.pdf"),
    temp5,
    units = "cm",
    dpi = "retina",
    limitsize = FALSE,
    ...
  )
}
