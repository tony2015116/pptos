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
  w <- weight <- stage <- responder <- safe_lm <- model_lm <- valid_lm <- adg_0 <- . <- glance <- unchop_dt <- NULL
  label_interval <-
    function(breaks) {
      paste0(breaks[1:length(breaks) - 1], "-", breaks[2:length(breaks)])
    }
  safelm = purrr::safely(.f = stats::lm)
  temp1 = data.table::copy(data)[, ':='(
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
  saferlm = purrr::safely(.f = stats::lm)
  temp2 = temp1[, safe_lm := purrr::map(data, function(df, ...)
    safelm(..., data = df), ...)][, model_lm := purrr::map(safe_lm, function(x)
      x$result)][, valid_lm := purrr::map(model_lm, is.null)]

  temp3 = temp2[valid_lm == FALSE][, adg_0 := purrr::map(model_lm, function(mod)
    stats::coefficients(mod)[[2]])][, glance := map(model_lm, broom::glance)][, tidyfst::unchop_dt(.SD), .SDcol = c("data", "glance", "adg_0"), by = .(responder, stage)]
}
