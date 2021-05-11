#' removed_freq
#'
#' @param data the results from function of outlier_found
#' @param begin_date the begin data of test
#'
#' @return the remaining visit data from oulier_found
#' @export
#'
#' @examples
#' #removed_freq(data = temp2, begin_date = "2015-07-01")
removed_freq <- function(data, begin_date) {
  temp <- seq_days <- responder <- weight <- . <- min_weight <- max_weight <- location <- n <- max_date <-
    min_date <- date_length <- date_na <- state <- NULL

  lueee1 <- unique(data.table::copy(data))[, keyby = .(responder),temp := data.table::frankv(seq_days, ties.method = "dense") <= 3
  ][temp == TRUE
  ][weight <= 60000][,keyby = .(responder, location),.(min_weight = stats::median(weight))]

  lueee2 <- unique(data.table::copy(data))[, keyby = .(responder),temp := data.table::frankv(-seq_days, ties.method = "dense") <= 3
  ][temp == TRUE
  ][weight >= 90000][,keyby = .(responder, location),.(max_weight = stats::median(weight))]

  lueee3 <- merge(lueee1, lueee2, all = TRUE)

  temp1 <- data[date >= as.Date({
    {
      begin_date
    }
  })][lueee3, on = c("responder", "location")
      ][, ':='(max_date = max(seq_days), min_date = min(seq_days)), .(responder, location)
      ][, n := .N, .(responder, location)
      ][CJ(date = date,responder = responder, unique = TRUE), on = .(date, responder)
      ][, location := zoo::na.locf(location, na.rm = FALSE, fromLast = T), responder
      ][, location := zoo::na.locf(location, na.rm = FALSE, fromLast = F), responder
      ][, date_length := as.integer(max(date) - min(date) + 1), location
      ][, date_na := sum(is.na(weight)), .(location, responder)
      ][,state := data.table::fcase(min_weight > 60000, "start_weight > 60",
                                    max_weight < 95000, "end_weight < 95",
                                    weight < 15000, "weight < 15",
                                    weight > 130000, "weight > 130",
                                    max_date - min_date < 40, "test_days < 40",
                                    n < 20, "test_records < 20",
                                    date_na / date_length >= 1 / 3, "data_na >= 1/3",
                                    default = "Right")][!is.na(weight)]

# table(temp1$state)
# tabyl(temp1$state, sort = F)
type = temp1$state
summarytools::freq(type, order = "freq")
}

