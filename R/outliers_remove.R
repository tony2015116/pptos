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
  lueee1 <-
    unique(data.table::copy(data)[, temp := .SD == min(seq_days), .SDcols = "seq_days", by = responder][temp == TRUE][, ':='(min_weight = stats::median(weight)), by = .(responder)][, c(1:2, 9)])

  lueee2 <-
    unique(data.table::copy(data)[, temp := .SD == max(seq_days), .SDcols = "seq_days", by = responder][temp == TRUE][, ':='(max_weight = stats::median(weight)), by = .(responder)][, c(1:2, 9)])
  lueee3 <- merge(lueee1, lueee2, all = TRUE)

  temp1 <- data[date >= as.Date({
    {
      begin_date
    }
  })][lueee3, on = c("responder", "location")][min_weight <= 60000 &
                                                 max_weight >= 105000][weight > 25000][, ':='(max_date = max(seq_days),
                                                                                              min_date = min(seq_days)), .(responder, location)][, n := .N, .(responder, location)][max_date - min_date > 30 &
                                                                                                                                                                                      n > 20][CJ(date = date,
                                                                                                                                                                                                 responder = responder,
                                                                                                                                                                                                 unique = TRUE), on = .(date, responder)][, location := zoo::na.locf(location, na.rm = FALSE, fromLast = T), responder][, location :=
                                                                                                                                                                                                                                                                                                                          zoo::na.locf(location, na.rm = FALSE, fromLast = F), responder][, date_length := as.integer(max(date) -
                                                                                                                                                                                                                                                                                                                                                                                                                        min(date) + 1), location][, date_na := sum(is.na(weight)), .(location, responder)][date_na /
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             date_length < 1 / 3][!is.na(weight)]
}
