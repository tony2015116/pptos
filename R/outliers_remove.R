#' outliers_remove
#'
#' @param data the results from function of outlier_found
#' @param begin_date the begin data of test
#'
#' @return the remaining visit data from oulier_found
#' @export
#'
#' @examples
#' #temp3 <- outliers_remove(temp2, begin_date = "2015-07-01")
outliers_remove <- function(data, begin_date) {
  temp <- seq_days <- responder <- weight <- . <- min_weight <- max_weight <- location <- n <- max_date <-
    min_date <- date_length <- date_na <- NULL

  lueee1 <- unique(data.table::copy(data))[, keyby = .(responder),temp := data.table::frankv(seq_days, ties.method = "dense") <= 3
  ][temp == TRUE
  ][weight <= 60000 & weight >= 15000][,keyby = .(responder, location),.(min_weight = stats::median(weight))]

  lueee2 <- unique(data.table::copy(data))[, keyby = .(responder),temp := data.table::frankv(-seq_days, ties.method = "dense") <= 3
  ][temp == TRUE
  ][weight >= 95000][,keyby = .(responder, location),.(max_weight = stats::median(weight))]

  lueee3 <- merge(lueee1, lueee2, all = TRUE)

  temp1 <- data[date >= as.Date({
    {
      begin_date
    }
  })][lueee3, on = c("responder", "location")][min_weight <= 60000 & max_weight >= 95000
                                               ][weight >= 15000 & weight <= 130000
                                                 ][, ':='(max_date = max(seq_days), min_date = min(seq_days)), .(responder, location)
                                                   ][, n := .N, .(responder, location)
                                                     ][max_date - min_date >= 40 & n >= 20
                                                       ][CJ(date = date,responder = responder, unique = TRUE), on = .(date, responder)
                                                         ][, location := zoo::na.locf(location, na.rm = FALSE, fromLast = T), responder
                                                           ][, location := zoo::na.locf(location, na.rm = FALSE, fromLast = F), responder
                                                             ][, date_length := as.integer(max(date) - min(date) + 1), location
                                                               ][, date_na := sum(is.na(weight)), .(location, responder)
                                                                 ][date_na / date_length < 1 / 3
                                                                   ][!is.na(weight)]
}

