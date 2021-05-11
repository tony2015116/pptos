#' duplicated_responder_infos
#'
#' @param data the results from function of outlier_found
#' @param begin_date the begin data of test
#'
#' @return the duplicated responder infomations
#' @export
#'
#' @examples
#' #duplicated_responder_infos(temp2, begin_date = "2015-07-01")
duplicated_responder_infos <- function(data, begin_date) {
  temp <- seq_days <- responder <- weight <- . <- min_weight <- max_weight <- location <- n <- max_date <-
    min_date <- date_length <- date_na <- NULL

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
  })][lueee3, on = c("responder", "location")]

  temp1_1 <- lueee3 %>%
    janitor::get_dupes(responder)

  duplicated_responder <- unlist(temp1_1$responder)

  temp1_2 <- temp1[responder %chin% duplicated_responder
  ][, keyby = .(responder, location), .(begin_date = min(date),
                                        end_date = max(date),
                                        begin_days = min(seq_days),
                                        end_date = max(seq_days))]
  list(duplicated_info1 = temp1_1, duplicated_info2 = temp1_2)

}
