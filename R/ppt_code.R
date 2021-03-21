#' import_csv
#'
#' @param file_list full path list files
#' @param package from data.table, vroom or readr
#' @param ... other parameters in three packages
#'
#' @return a data.table format
#' @export
#'
#' @examples
import_csv <- function(file_list, package, ...) {
  if (package == "data.table") {
    data.table::rbindlist(map(file_list, function(x, ...) {
      data.table::fread(x, ...)
    }, ...))
  } else if (package == "vroom") {
    vroom::vroom(file_list, ...)
  } else if (package == "readr") {
    map_dfr(file_list, function(x, ...) {
      readr::read_csv(x, ...)
    }, ...)
  }
}

#' ppt_monitor
#'
#' @param data all csv data of locations
#' @param station_type nedap or fire stations
#' @param path_out the path to receive results
#'
#' @return pictures of each locations
#' @export
#'
#' @examples
ppt_monitor <- function(data, station_type, path_out) {
  if (station_type == "nedap") {
    temp1 <- copy(setDT(data))
    temp2 <-
      unique(temp1)[, c("date", "time") := tstrsplit(visit_time, " ", fixed =
                                                       TRUE)][, c("date") := ymd(date)][, !c("visit_time", "time")]
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
      copy(setDT(data))[, Entry := do.call(paste, c(.SD, sep = " ")), .SDcol =
                          c("Date", "Entry")][, Exit := do.call(paste, c(.SD, sep = " ")), .SDcol =
                                                c("Date", "Exit")][, c("Entry", "Exit") := lapply(.SD, lubridate::ymd_hms), .SDcol =
                                                                     c("Entry", "Exit")][, duration := fifelse(Exit - Entry < 0 &
                                                                                                                 hour(Exit) == 0,
                                                                                                               Exit - Entry + lubridate::ddays(1),
                                                                                                               Exit - Entry)]
    setnames(temp1,
             c(1:3, 9),
             c("location", "responder", "date", "weight"))
    temp2 <- unique(temp1)[, date := ymd(date)]
    temp3 <-
      unique(temp2, by = c("location", "responder", "date"))[!is.na(responder)][, keyby = .(location, date), .(animal_number = .N)]
    temp4 <- temp2[!is.na(responder)][, duration := as.numeric(duration)][, keyby = .(location, date), .(
      `total_intake_duration(min)` = round(sum(duration) / 60, digits = 4),
      total_intake = round(sum(Consumed), digits = 4),
      visit_number = .N
    )]
  }
  temp5 <- merge(temp3, temp4, all.x = TRUE)
  temp6 = melt(
    temp5,
    id.vars = c("location", "date"),
    variable.name = "items",
    value.name = "values"
  )
  temp6_1 <- temp2[, .(location, date, weight)]
  temp6_2 <- temp6_1 %>%
    tidyfst::nest_dt(location) %>%
    tidyfst::mutate_dt(ndt = map(ndt, function(data) {
      data[CJ(date = tidyr::full_seq(date, 1)), on = .(date)][CJ(date = date, unique =
                                                                   TRUE), on = .(date)]
    })) %>%
    tidyfst::mutate_dt(
      plot1 = map2(
        ndt,
        location,
        ~ ggplot(data = .x, aes(
          x = date, y = weight, group = date
        )) +
          geom_boxplot(outlier.color = "red") +
          background_grid(minor = "none") +
          scale_x_date(date_breaks = "1 day", date_labels =  "%d") +
          theme_bw() +
          theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = -90),
            axis.text = element_text(size = 8)
          )
      )
    ) %>%
    tidyfst::select_dt(-ndt)

  temp7 <- temp6 %>%
    nest_dt(location) %>%
    mutate_dt(ndt = map(ndt, function(data) {
      data[CJ(date = tidyr::full_seq(date, 1)), on = .(date)][CJ(date = date,
                                                                 items = items,
                                                                 unique = TRUE), on = .(date, items)][!is.na(items)][, items := factor(items,
                                                                                                                                       labels = c("N", "visit_time/min", "feed_intake/kg", "visit_number"))]
    })) %>%
    tidyfst::mutate_dt(
      plot = map2(
        ndt,
        location,
        ~ ggplot(data = .x, aes(x = date, y = values)) +
          geom_point(aes(col = items)) +
          geom_line(aes(col = items)) +
          theme_bw() +
          facet_grid(items ~ ., scales = "free") +
          scale_x_date(date_breaks = "1 day", date_labels =  "%m") +
          scale_colour_brewer(palette = "Set1") +
          theme(
            strip.text.y = element_text(angle = 0, hjust = 0),
            legend.position = "none",
            axis.title = element_blank(),
            axis.text.x = element_text(angle = -90),
            axis.text = element_text(size = 8),
            strip.placement = "outside",
            strip.background = element_rect(colour = "white", fill = "white")
          ) +
          ggtitle(paste0("Location：", .y)) +
          background_grid(minor = "none")
      )
    ) %>%
    tidyfst::select_dt(-ndt)
  temp8 <- tidyfst::left_join_dt(temp7, temp6_2, by = "location")
  temp9 <- temp8 %>%
    dplyr::mutate(finals = pmap(list(plot, plot1),
                                ~ wrap_plots(..1, ..2, nrow = 2))) %>% mutate_dt(plot_name = paste0(location, "_station.png"))

  walk2(
    temp9$plot_name,
    temp9$finals,
    ~ ggsave(
      filename = paste0(path_out, .x),
      plot = .y,
      height = 7,
      width = 11
    )
  )
}

#' feed_intake_monitor
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
feed_intake_monitor <- function(data, station_type, path_out, ...) {
  if (station_type == "nedap") {
    temp1 <-
      unique(copy(setDT(data)))[, c("date", "time") := tstrsplit(visit_time, " ", fixed =
                                                                   TRUE)][, c("date") := ymd(date)][, !c("visit_time", "time")]
    temp2 <-
      temp1[, keyby = .(location, responder, date), .(total_intake = round(sum(feed_intake) /
                                                                             1000, digits = 4))][, all_feed_a_station_one_day := sum(total_intake), by = .(location, date)][, percent_intake := total_intake /
                                                                                                                                                                              all_feed_a_station_one_day]
    to_factor = c("location", "responder")
    temp2[, (to_factor) := map(.SD, as.factor), .SDcols = to_factor]
  } else if (station_type == "fire") {
    temp1 <- unique(copy(setDT(data)))[, Date := ymd(Date)]
    setnames(temp1, 1:3, c("location", "responder", "date"))
    temp2 <-
      temp1[, keyby = .(location, responder, date), .(total_intake = round(sum(Consumed), digits = 4))][, all_feed_a_station_one_day := sum(total_intake), by = .(location, date)][, percent_intake := total_intake /
                                                                                                                                                                                     all_feed_a_station_one_day]
    to_factor = c("location", "responder")
    temp2[, (to_factor) := map(.SD, as.factor), .SDcols = to_factor]
  }
  temp3 <- temp2 %>%
    tidyfst::nest_dt(location) %>%
    tidyfst::mutate_dt(
      plot1 = map2(
        ndt,
        location,
        ~ ggplot(data = .x, aes(
          y = percent_intake, x = date, fill = responder
        )) +
          theme_bw() +
          geom_col(
            width = 0.8,
            na.rm = F,
            show.legend = T
          ) +
          ggtitle(.y) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, 1),
            breaks = seq(0, 1, 0.1)
          ) +
          theme(
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
          guides(
            shape = guide_legend(override.aes = list(size = 7)),
            color = guide_legend(override.aes = list(size = 7))
          ) +
          ggtitle(paste0("location：", .y)) +
          scale_x_date(
            date_breaks = "1 day",
            date_labels =  "%m-%d",
            date_minor_breaks = "1 day"
          ) +
          scale_fill_manual(
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

      plot2 = map2(
        ndt,
        location,
        ~ ggplot(data = .x, aes(
          y = total_intake, x = date, fill = responder
        )) +
          theme_bw() +
          geom_col(
            width = 0.8,
            na.rm = F,
            show.legend = F
          ) +
          theme(
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
          guides(
            shape = guide_legend(override.aes = list(size = 7)),
            color = guide_legend(override.aes = list(size = 7))
          ) +
          scale_x_date(
            date_breaks = "1 day",
            date_labels =  "%m-%d",
            date_minor_breaks = "1 day"
          ) +
          scale_fill_manual(
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
    dplyr::mutate(finals = pmap(list(plot1, plot2),
                                ~ wrap_plots(..1, ..2, ncol = 1)))
  temp5 <- wrap_plots(temp4$finals, ncol = 2)
  ggsave(
    paste0(path_out, "feed_intake_monitor.pdf"),
    temp5,
    units = "cm",
    dpi = "retina",
    limitsize = FALSE,
    ...
  )
}

#' transform
#'
#' @param station_type  nedap or fire stations
#' @param data all csv data of locations

#'
#' @return nedap or fire stations with same transform format data
#' @export
#'
#' @examples
tranform <- function(station_type, data) {
  if (station_type == "nedap") {
    datatoDT = setDT(data)[, c(1, 3:9)][, visit_time := lubridate::ymd_hms(visit_time)][!is.na(visit_time)]
    datatoDT2 = unique(datatoDT)[, c("date", "time") := tstrsplit(visit_time, " ", fixed =
                                                                    TRUE)]
    datatoDT3 = split(datatoDT2, by = "location")

    datatoDT4 = map(datatoDT3, function(x) {
      temp = x[order(visit_time)][responder != 0 & !is.na(location)]
      setnames(
        temp,
        c("feed_intake", "duration", "visit_time"),
        c("fiv", "otv", "entrancetime")
      )
      temp[, ':='(
        entrancefeedweight = 0,
        exitfeedweight = 0,
        exittime = entrancetime + lubridate::seconds(otv),
        frv = fiv / (otv / 60),
        ltd_entrance_step1 = shift(entrancetime, n = 1L, type = "lead")
      )][, ':='(
        ftd_exit_step1 = shift(exittime, n = 1L, type = "lag"),
        ltd = ltd_entrance_step1 - exittime
      )][, ':='(
        ftd = entrancetime - ftd_exit_step1,
        lwd_entrance_step1 = shift(entrancefeedweight, n = 1L, type = "lead"),
        fwd_exit_step1 = shift(exitfeedweight, n = 1L, type = "lag")
      )][, ':='(lwd = lwd_entrance_step1 - exitfeedweight,
                fwd = entrancefeedweight - fwd_exit_step1)]
    })
  } else if (station_type == "fire") {
    datatoDT = setDT(data)[, 1:10]
    names(datatoDT) <-
      c(
        "location",
        "responder",
        "date",
        "entrancetime",
        "exittime",
        "entrancefeedweight",
        "exitfeedweight",
        "fiv",
        "weight",
        "feed_given"
      )
    datatoDT3 = split(datatoDT, by = "location")
    datatoDT4 = map(datatoDT3, function(x) {
      x[responder != 0 &
          !is.na(location)][, entrancetime := do.call(paste, c(.SD, sep = " ")), .SDcol =
                              c("date", "entrancetime")][, exittime := do.call(paste, c(.SD, sep = " ")), .SDcol =
                                                           c("date", "exittime")][, c("entrancetime", "exittime") := lapply(.SD, lubridate::ymd_hms), .SDcol =
                                                                                    c("entrancetime", "exittime")][order(entrancetime)][, date := as.Date(date)][, otv := fifelse(
                                                                                      exittime - entrancetime < 0 &
                                                                                        hour(exittime) == 0,
                                                                                      exittime - entrancetime + lubridate::ddays(1),
                                                                                      exittime - entrancetime
                                                                                    )][, otv := as.numeric(otv)][, ':='(
                                                                                      fiv = 1000 * fiv,
                                                                                      weight = 1000 * weight,
                                                                                      entrancefeedweight = 1000 * entrancefeedweight,
                                                                                      exitfeedweight = 1000 * exitfeedweight
                                                                                    )][, frv := fiv / (otv / 60)][, ':='(
                                                                                      ltd_entrance_step1 = shift(entrancetime, type = "lead"),
                                                                                      ftd_exit_step1 = shift(exittime, type = "lag")
                                                                                    )][, ':='(ltd = ltd_entrance_step1 - exittime,
                                                                                              ftd = entrancetime - ftd_exit_step1)][, ':='(
                                                                                                lwd_entrance_step1 = shift(entrancefeedweight, type = "lead"),
                                                                                                fwd_exit_step1 = shift(exitfeedweight, type = "lag")
                                                                                              )][, ':='(lwd = lwd_entrance_step1 - exitfeedweight,
                                                                                                        fwd = entrancefeedweight - fwd_exit_step1)][, c("ltd", "ftd") := lapply(.SD, as.numeric), .SDcol = c("ltd", "ftd")]
    })
  }
  transform_0 <- map(datatoDT4, function(x) {
    x[, ':='(
      fiv_lo = fifelse(fiv < -20, 1, 0),
      fiv_hi = fifelse(fiv > 2000, 1, 0),
      fiv_0 = fifelse(otv == 0 & abs(fiv) > 20, 1, 0),
      otv_lo = fifelse(otv < 0, 1, 0),
      otv_hi = fifelse(otv > 3600, 1, 0),
      frv_hi_fiv_lo = fifelse(fiv > 0 & fiv < 50 & frv > 500, 1, 0),
      fiv_hi_strict = fifelse(fiv >= 50 &
                                any(
                                  shift(fiv, type = "lag") < -20,
                                  shift(fiv, type = "lead") < -20
                                ) & frv > 110, 1, 0),
      frv_hi = fifelse(fiv >= 50 &
                         any(
                           shift(fiv, type = "lag", n = 2) < -20,
                           shift(fiv, type = "lead", n = 2) < -20
                         ) & frv > 170, 1, 0),
      frv_0 = fifelse(frv == 0 & otv > 500, 1, 0),
      frv_lo = fifelse(frv != 0 & abs(frv) <= 2, 1, 0),
      lwd_lo = fifelse((!is.na(lwd)) & lwd < -20, 1, 0),
      lwd_hi = fifelse((!is.na(lwd)) & lwd > 1800, 1, 0),
      fwd_lo = fifelse((!is.na(fwd)) & fwd < -20, 1, 0),
      fwd_hi = fifelse((!is.na(fwd)) & fwd > 1800, 1, 0),
      ltd_lo = fifelse((!is.na(ltd)) & ltd < 0, 1, 0),
      ftd_lo = fifelse((!is.na(ftd)) & ftd < 0, 1, 0)
    )][, date := lubridate::ymd(as.Date(date))][order(entrancetime), seq_in_day := 1:.N, by = .(responder, date)][order(entrancetime), seq_days := .GRP, by = date][order(responder, entrancetime)]
  })
  transform2 =  rbindlist(transform_0, use.names = T, fill = T)
  transform3 <- transform2[,
                           .(
                             date,
                             seq_in_day,
                             seq_days,
                             location,
                             responder,
                             entrancetime,
                             exittime,
                             entrancefeedweight,
                             exitfeedweight,
                             weight,
                             otv,
                             fiv,
                             frv,
                             ltd,
                             ftd,
                             lwd,
                             fwd,
                             fiv_lo,
                             fiv_hi,
                             fiv_0,
                             otv_lo,
                             otv_hi,
                             frv_hi_fiv_lo,
                             fiv_hi_strict,
                             frv_hi,
                             frv_0,
                             frv_lo,
                             lwd_lo,
                             lwd_hi,
                             fwd_lo,
                             fwd_hi,
                             ltd_lo,
                             ftd_lo
                           )]
}

#' fiv_plot
#'
#' @param data the import csv data
#'
#' @return histgram plot of all feed intake
#' @export
#'
#' @examples
fiv_plot <- function(data) {
  ggplot(data, aes(fiv)) +
    geom_histogram(aes(y = stat(count) / sum(count)), binwidth = 5) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw()
}

#' error_type_freq
#'
#' @param data transformed data from function of transform
#'
#' @return frequency of 16 error types
#' @export
#'
#' @examples
error_type_freq <- function(data) {
  proportions1 <-
    lapply(copy(data)[, 18:33], function(x) {
      prop.table(table(x))
    })
  prop2 <- map(proportions1, as.data.frame)
  prop3 <-
    rbindlist(prop2,
              use.names = TRUE,
              fill = TRUE,
              idcol = "error_type")
  prop4 <- dcast(prop3, error_type ~ x, value.var = "Freq")
  return(prop4)
}


#' outlier_found
#'
#' @param data transformed data from function of transform
#' @param ... the rubost model fomula
#'
#' @return the weighting of each visit
#' @export
#'
#' @examples
outlier_found <- function(data, ...) {
  saferlm = safely(.f = rlm)
  temp1 = copy(data)[, c("responder", "location", "date", "seq_days", "weight")][, .(data = list(.SD)), responder][, safe_rlm := map(data, function(df, ...)
    saferlm(..., data = df), ...)][, model_rlm := map(safe_rlm, function(x)
      x$result)][, valid_rlm := map(model_rlm, is.null)]
  temp2 = temp1[valid_rlm == FALSE][, rlm_predict := map(model_rlm, predict)][, w := map(model_rlm, function(x)
    x$w)][, unchop_dt(.SD), .SDcol = c("data", "rlm_predict", "w"), by = responder]
}

#' growth_curve
#'
#' @param data results from function of outlier_found
#' @param path_out the path to put your results
#' @param weighting the weighting you choose to determine outliers
#'
#' @return pictures of growth curve
#' @export
#'
#' @examples
growth_curve <- function(data, path_out, weighting) {
  temp = copy(data)[, colors := fifelse(w >= {
    {
      weighting
    }
  }, "Normal", "Outlier")][, .(data = list(.SD)), location][, plot := map2(
    data,
    location,
    ~ ggplot(data = .x, aes(x = date, y = weight)) +
      theme_bw() +
      geom_point(size = 1, na.rm = F) +
      geom_point(aes(col = colors)) +
      scale_color_manual(
        values = c("Normal" = "green", "Outlier" = "red"),
        name = "robust regression"
      ) +
      scale_x_date(date_breaks = "1 day", date_labels =  "%m-%d") +
      geom_line(aes(x = date, y = rlm_predict)) +
      facet_wrap(~ as.numeric(responder), ncol = 2) +
      scale_y_continuous(
        breaks = seq(15000, 130000, 10000),
        ,
        limits = c(15000, 130000)
      ) +
      labs(title = paste("Location:", .y)) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = -90, size = 10),
        plot.title = element_text(size = 25, face = "bold")
      )
  )][]
  walk2(
    paste0(path_out, temp$location, ".png"),
    temp$plot,
    ggsave,
    width = 35,
    height = 45,
    units = "cm",
    dpi = "retina"
  )
}

#' outlier_remove
#'
#' @param data the results from function of outlier_found
#' @param begin_date the begin data of test
#'
#' @return the remaining visit data from oulier_found
#' @export
#'
#' @examples
outlier_remove <- function(data, begin_date) {
  lueee1 <-
    unique(copy(data)[, temp := .SD == min(seq_days), .SDcols = "seq_days", by = responder][temp == TRUE][, ':='(min_weight = median(weight)), by = .(responder)][, c(1:2, 9)])

  lueee2 <-
    unique(copy(data)[, temp := .SD == max(seq_days), .SDcols = "seq_days", by = responder][temp == TRUE][, ':='(max_weight = median(weight)), by = .(responder)][, c(1:2, 9)])
  lueee3 <- merge(lueee1, lueee2, all = TRUE)

  temp1 <- data[date >= as.Date({
    {
      begin_date
    }
  })][lueee3, on = c("responder", "location")][min_weight <= 60000 &
                                                 max_weight >= 110000][weight > 25000][, ':='(max_date = max(seq_days),
                                                                                              min_date = min(seq_days)), .(responder, location)][, n := .N, .(responder, location)][max_date - min_date > 30 &
                                                                                                                                                                                      n > 20][CJ(date = date,
                                                                                                                                                                                                 responder = responder,
                                                                                                                                                                                                 unique = TRUE), on = .(date, responder)][, location := zoo::na.locf(location, na.rm = FALSE, fromLast = T), responder][, location :=
                                                                                                                                                                                                                                                                                                                          zoo::na.locf(location, na.rm = FALSE, fromLast = F), responder][, date_length := as.integer(max(date) -
                                                                                                                                                                                                                                                                                                                                                                                                                        min(date) + 1), location][, date_na := sum(is.na(weight)), .(location, responder)][date_na /
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             date_length < 1 / 3][!is.na(weight)]
}

#' adg_get
#'
#' @param data the results from function of outlier remove
#' @param my_break the weight breaks, with only one break
#' @param weighting the weighting choosed to select visit data
#' @param ... linear model to get adg
#'
#' @return the adg of each animals
#' @export
#'
#' @examples
adg_get <- function(data, my_break, weighting, ...) {
  label_interval <-
    function(breaks) {
      paste0(breaks[1:length(breaks) - 1], "-", breaks[2:length(breaks)])
    }
  safelm = safely(.f = lm)
  temp1 = copy(data)[, ':='(
    min_weight = NULL,
    max_weight = NULL,
    max_date = NULL,
    min_date = NULL,
    n = NULL,
    date_length = NULL,
    date_na = NULL
  )][w >= {
    {
      weighting
    }
  }][, ':='(stage = cut(
    weight,
    breaks = {
      {
        my_break
      }
    },
    labels = label_interval(my_break / 1000)
  ))][!is.na(stage)][, .(data = list(.SD)), .(stage, responder)]
  saferlm = safely(.f = lm)
  temp2 = temp1[, safe_lm := map(data, function(df, ...)
    safelm(..., data = df), ...)][, model_lm := map(safe_lm, function(x)
      x$result)][, valid_lm := map(model_lm, is.null)]

  temp3 = temp2[valid_lm == FALSE][, adg_0 := map(model_lm, function(mod)
    coefficients(mod)[[2]])][, glance := map(model_lm, broom::glance)][, unchop_dt(.SD), .SDcol = c("data", "glance", "adg_0"), by = .(responder, stage)]
}

#' dfi_get
#'
#' @param origin_data the results from import_csv
#' @param adg_data the results from adg_get
#'
#' @return dfi results
#' @export
#'
#' @examples
dfi_get <- function(origin_data, adg_data) {
  temp1_base_info = unique(adg_data[, .(responder, location, stage, date, seq_days)])
  col_names = names(origin_data)[c(1:5, 10:17, 18:33)]
  error_type = col_names[14:29]
  temp1_inner_join = origin_data[temp1_base_info, on = c("responder", "location", "date"), nomatch = 0][, ..col_names][, OE := apply(.SD, 1, function(x)
    sum(x, na.rm = T)), .SDcols = error_type][]

  origin_dfi = temp1_inner_join[, .(origin_dfi = sum(fiv)), by = .(responder, seq_days)]

  right_dfi = temp1_inner_join[OE == 0][, .(dfi_right_part = sum(fiv)), by = .(responder, seq_days)]
  error_dfi_data = temp1_inner_join[OE != 0]

  error_dfi = temp1_inner_join[OE != 0][, .(dfi_error_part = sum(fiv)), by = .(responder, seq_days)]
  adg = unique(adg_data[, c("responder", "adg_0")])
  bw = adg_data[, .(bw = median(weight)), by = .(responder, seq_days)]

  otd_fid_trans <- function(data, name1, name2, name3, ...) {
    temp1 <- function(col_names, ...) {
      eval(as.name(data))[eval(as.name(col_names)) > 0, by = .(responder, seq_days), map(.SD, sum), .SDcols = name1]
    }
    temp2 <- map(name2, temp1)
    temp3 <-
      map2(temp2, name3, function(x, y)
        setnames(x, name1, y))
    temp4 <-
      reduce(temp3, full_join_dt, by = c("responder", "seq_days"))
    return(temp4)
  }

  useful_list = tibble::tribble(
    ~ data,
    ~ name1,
    ~ name2,
    ~ name3,
    "error_dfi_data",
    "otv",
    names(error_dfi_data)[c(14:15, 19:27)],
    paste0("otd_", c(1:2, 6:14)),
    "error_dfi_data",
    "fiv",
    names(error_dfi_data)[c(17:18, 28:29)],
    paste0("fid_", c(4:5, 15:16))
  )

  temp5 = pmap(useful_list, otd_fid_trans)

  temp6 = reduce(temp5, full_join_dt, by = c("responder", "seq_days"))

  cols_error_types = names(temp1_inner_join)[14:29]
  cols_error_types2 = paste0(cols_error_types, "_p")
  temp7 = temp1_inner_join[, lapply(.SD, sum), .SDcols = cols_error_types, by = .(responder, seq_days)][]
  temp8 = temp1_inner_join[, .N, by = .(responder, seq_days)][temp7, on = c("responder", "seq_days")][, lapply(.SD, function(x)
    x / N), .SDcols = cols_error_types, by = .(responder, seq_days)]
  setnames(temp8, cols_error_types, cols_error_types2)
  temp9 = temp6[temp8, on = c("responder", "seq_days")][adg, on = c("responder")][bw, on = c("responder", "seq_days")][error_dfi, on = c("responder", "seq_days")]
  temp9[is.na(temp9)] <- 0

  right_dfi_in_one_day = temp9[right_dfi, on = .(responder, seq_days)][!is.na(dfi_error_part)][, dfi_error_part := NULL]
  right_dfi_each_day = temp9[right_dfi, on = .(responder, seq_days)][is.na(dfi_error_part)][, .(responder, seq_days, dfi_right_part)]

  temp10 <- setDF(right_dfi_in_one_day) %>%
    recipes::recipe(dfi_right_part ~ .) %>%
    recipes::update_role(responder, seq_days, new_role = "id") %>%
    recipes::step_corr(all_predictors()) %>%
    recipes::step_zv(all_numeric()) %>%
    recipes::step_scale(all_predictors()) %>%
    recipes::prep() %>%
    recipes::juice() %>%
    mutate_at("responder", as.factor)
  all_name <- names(temp10)
  predictor_name <-
    all_name[!all_name %in% c("responder", "dfi_right_part", "seq_days")]
  temp11 <- temp10 %>% rownames_to_column(var = "rownames")
  model_formula <-
    as.formula(paste(
      "dfi_right_part~",
      paste(predictor_name, collapse = "+"),
      "+ (1 | responder)"
    ))
  mod <- lme4::lmer(model_formula, temp10)

  temp12 <- broom.mixed::augment(mod) %>% janitor::clean_names()

  if (nrow(temp12) == nrow(temp11) &
      (!c("rownames") %in% names(temp12))) {
    temp14 <- temp12 %>% bind_cols(temp10[, 2])
  } else if (nrow(temp12) != nrow(temp11) &
             (c("rownames") %in% names(temp12))) {
    temp14 <- temp12 %>%
      inner_join_dt(temp11, by = c("rownames", "dfi_right_part"))
  }

  temp15 <- setDF(error_dfi) %>%
    mutate_at("responder", as.factor) %>%
    right_join(temp14)

  right_dfi_each_day2 <- setDF(right_dfi_each_day) %>%
    mutate_at("responder", as.factor)

  origin_dfi2 <- setDF(origin_dfi) %>%
    mutate_at("responder", as.factor)

  temp1_base_info2 <- temp1_base_info %>%
    mutate_at("responder", as.factor)

  temp16 <- temp15 %>%
    dplyr::select(responder, seq_days, fixed) %>%
    rename(dfi_right_part = fixed) %>%
    bind_rows(right_dfi_each_day2) %>%
    arrange(responder, seq_days) %>%
    inner_join(temp1_base_info2) %>%
    rename(corrected_dfi = dfi_right_part) %>%
    inner_join(origin_dfi2) %>%
    dplyr::select(4, 6, 2, 5, 1, 7, 3) %>%
    group_by(responder) %>%
    mutate(begin_date_real = min(date),
           end_date_real = max(date))

  temp17 <- temp16 %>%
    group_by(responder, location, stage) %>%
    mutate(corrected_dfi = ifelse(corrected_dfi < 0, NA, corrected_dfi)) %>%
    summarise(adfi = mean(corrected_dfi, na.rm = TRUE))

  temp18 <- adg_data %>%
    mutate_at("responder", as.factor) %>%
    left_join(temp17) %>%
    mutate(fcr = adfi / adg_0) %>%
    dplyr::select(-c(4:8)) %>%
    distinct()

  list(
    error_free_data_trans = temp9,
    model_results = temp14,
    dfi_results = temp16,
    fcr_results = temp18
  )
}

#' dfi_plot
#'
#' @param dfi_data the resluts from dfi_get
#' @param path_out the path to put results
#'
#' @return pictures of dfi
#' @export
#'
#' @examples
dfi_plot <- function(dfi_data, path_out) {
  corrected_dfi <- setDT(dfi_data)
  mDT <- melt(
    corrected_dfi,
    measure.vars  = c("origin_dfi", "corrected_dfi"),
    variable.name = "state",
    value.name    = "dfi"
  )

  niuhei = mDT[, .(data = list(.SD)), location][, plot := map2(
    data,
    location,
    ~ ggplot(data = .x, aes(
      x = date, y = dfi, color = state
    )) +
      theme_bw() +
      geom_point(size = 1) +
      geom_line(aes(linetype = state), show.legend = F) +
      scale_x_date(date_breaks = "1 day", date_labels =  "%m-%d") +
      facet_wrap(~ responder, ncol = 2) +
      labs(title = paste("Location:", .y)) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = -90, size = 8),
        plot.title = element_text(size = 25, face = "bold")
      )
  )][]
  walk2(
    paste0(path_out, niuhei$location, "_dfi.png"),
    niuhei$plot,
    ggsave,
    width = 35,
    height = 48,
    units = "cm",
    dpi = "retina"
  )
}


