#' csv_transform
#'
#' @param station_type  nedap or fire stations
#' @param data all csv data of locations

#'
#' @return nedap or fire stations with same transform format data
#' @export
#'
#' @examples
#' #temp1 <- csv_tranform(data = test, station_type = "nedap")
csv_tranform <- function(data, station_type) {
  visit_time <- responder <- location <- entrancetime <- otv <- fiv <- exittime <- ltd_entrance_step1 <-
    ftd_exit_step1 <- entrancefeedweight <- exitfeedweight <- lwd_entrance_step1 <- fwd_exit_step1 <-
    weight <- frv <- lwd <- fwd <- ltd <- ftd <- seq_in_day <- . <- seq_days <- fiv_lo <- fiv_hi <-
    fiv_0 <- otv_lo <- otv_hi <- frv_hi_fiv_lo <- fiv_hi_strict <- frv_hi <- frv_0 <- frv_lo <-
    lwd_lo <- lwd_hi <- fwd_lo <- fwd_hi <- ltd_lo <- ftd_lo <- NULL
  if (station_type == "nedap") {
    datatoDT = data.table::setDT(data)[, c(1, 3:9)][, visit_time := lubridate::ymd_hms(visit_time)][!is.na(visit_time)]
    datatoDT2 = unique(datatoDT)[, c("date", "time") := data.table::tstrsplit(visit_time, " ", fixed =
                                                                    TRUE)]
    datatoDT3 = split(datatoDT2, by = "location")

    datatoDT4 = purrr::map(datatoDT3, function(x) {
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
        frv = fiv / (otv / 60)
      )][, ':='(lwd = 0,
                  fwd = 0,
                  ltd = 0,
                  ftd = 0)]#nedap station new add in 2021.4.13
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
      frv_hi_strict = fifelse(fiv >= 50 &
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
                             frv_hi_strict,
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
