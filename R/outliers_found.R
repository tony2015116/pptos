#' outliers_found
#'
#' @param data transformed data from function of transform
#' @param ... the rubost model fomula
#'
#' @return the weighting of each visit
#' @export
#'
#' @examples
outliers_found <- function(data, ...) {
  . <- responder <- safe_rlm <- model_rlm <- valid_rlm <- rlm_predict <- w <- NULL
  saferlm = purrr::safely(.f = MASS::rlm)
  temp1 = data.table::copy(data)[, c("responder", "location", "date", "seq_days", "weight")][, .(data = list(.SD)), responder][, safe_rlm := purrr::map(data, function(df, ...)
    saferlm(..., data = df), ...)][, model_rlm := purrr::map(safe_rlm, function(x)
      x$result)][, valid_rlm := purrr::map(model_rlm, is.null)]
  temp2 = temp1[valid_rlm == FALSE][, rlm_predict := purrr::map(model_rlm, stats::predict)][, w := purrr::map(model_rlm, function(x)
    x$w)][, tidyfst::unchop_dt(.SD), .SDcol = c("data", "rlm_predict", "w"), by = responder]
}
