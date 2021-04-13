#' dfi_get
#'
#' @param origin_data the results from import_csv
#' @param adg_data the results from adg_get
#'
#' @return dfi results
#' @export
#'
#' @examples
#' #temp5 <- dfi_get(origin_data = temp1, adg_data = temp4)
dfi_get <- function(origin_data, adg_data) {
  . <- responder <- location <- stage <- seq_days <- col_names <- OE <- fiv <- median <- weight <-
    N <- dfi_error_part <- dfi_right_part <- fitted <- corrected_dfi <- adfi <- adg_0 <- NULL
  temp1_base_info = unique(adg_data[, .(responder, location, stage, date, seq_days)])
  col_names = names(origin_data)[c(1:5, 10:17, 18:33)]
  error_type = col_names[14:29]
  temp1_inner_join = origin_data[temp1_base_info, on = c("responder", "location", "date"), nomatch = 0
                                 ][, ..col_names
                                   ][, OE := apply(.SD, 1, function(x)sum(x, na.rm = T)), .SDcols = error_type][]

  origin_dfi = temp1_inner_join[, .(origin_dfi = sum(fiv)), by = .(responder, seq_days)]

  right_dfi = temp1_inner_join[OE == 0][, .(dfi_right_part = sum(fiv)), by = .(responder, seq_days)]
  error_dfi_data = temp1_inner_join[OE != 0]

  error_dfi = temp1_inner_join[OE != 0][, .(dfi_error_part = sum(fiv)), by = .(responder, seq_days)]
  adg = unique(adg_data[, c("responder", "adg_0")])
  bw = adg_data[, .(bw = as.double(stats::median(weight))), by = .(responder, seq_days)]

  otd_fid_trans <- function(data, name1, name2, name3, ...) {
    temp1 <- function(col_names, ...) {
      eval(as.name(data))[eval(as.name(col_names)) > 0, by = .(responder, seq_days), purrr::map(.SD, sum), .SDcols = name1]
    }
    temp2 <- purrr::map(name2, temp1)
    temp3 <-
      purrr::map2(temp2, name3, function(x, y)
        data.table::setnames(x, name1, y))
    temp4 <-
      purrr::reduce(temp3, tidyfst::full_join_dt, by = c("responder", "seq_days"))
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

  temp5 = purrr::pmap(useful_list, otd_fid_trans)

  temp6 = purrr::reduce(temp5, tidyfst::full_join_dt, by = c("responder", "seq_days"))

  cols_error_types = names(temp1_inner_join)[14:29]
  cols_error_types2 = paste0(cols_error_types, "_p")
  temp7 = temp1_inner_join[, lapply(.SD, sum), .SDcols = cols_error_types, by = .(responder, seq_days)][]
  temp8 = temp1_inner_join[, .N, by = .(responder, seq_days)][temp7, on = c("responder", "seq_days")][, lapply(.SD, function(x)
    x / N), .SDcols = cols_error_types, by = .(responder, seq_days)]
  data.table::setnames(temp8, cols_error_types, cols_error_types2)
  temp9 = temp6[temp8, on = c("responder", "seq_days")][adg, on = c("responder")][bw, on = c("responder", "seq_days")][error_dfi, on = c("responder", "seq_days")]
  temp9[is.na(temp9)] <- 0

  right_dfi_in_one_day = temp9[right_dfi, on = .(responder, seq_days)][!is.na(dfi_error_part)][, dfi_error_part := NULL]
  right_dfi_each_day = temp9[right_dfi, on = .(responder, seq_days)][is.na(dfi_error_part)][, .(responder, seq_days, dfi_right_part)]

  temp10 <- data.table::setDF(right_dfi_in_one_day) %>%
    recipes::recipe(dfi_right_part ~ .) %>%
    recipes::update_role(responder, seq_days, new_role = "id") %>%
    recipes::step_corr(recipes::all_predictors()) %>%
    recipes::step_zv(recipes::all_numeric()) %>%
    recipes::step_scale(recipes::all_predictors()) %>%
    recipes::prep() %>%
    recipes::juice() %>%
    dplyr::mutate_at("responder", as.factor)
  all_name <- names(temp10)
  predictor_name <-
    all_name[!all_name %in% c("responder", "dfi_right_part", "seq_days")]
  temp11 <- temp10 %>% tibble::rownames_to_column(var = "rownames")
  model_formula <-
    stats::as.formula(paste(
      "dfi_right_part~",
      paste(predictor_name, collapse = "+"),
      "+ (1 | responder)"
    ))
  mod <- lme4::lmer(model_formula, temp10)

  temp12 <- broom.mixed::augment(mod) %>% janitor::clean_names()

  if (nrow(temp12) == nrow(temp11) &
      (!c("rownames") %in% names(temp12))) {
    temp14 <- temp12 %>% dplyr::bind_cols(temp10[, 2])
  } else if (nrow(temp12) != nrow(temp11) &
             (c("rownames") %in% names(temp12))) {
    temp14 <- temp12 %>%
      tidyfst::inner_join_dt(temp11, by = c("rownames", "dfi_right_part"))
  }

  temp15 <- data.table::setDF(error_dfi) %>%
    dplyr::mutate_at("responder", as.factor) %>%
    dplyr::right_join(temp14)

  right_dfi_each_day2 <- data.table::setDF(right_dfi_each_day) %>%
    dplyr::mutate_at("responder", as.factor)

  origin_dfi2 <- data.table::setDF(origin_dfi) %>%
    dplyr::mutate_at("responder", as.factor)

  temp1_base_info2 <- temp1_base_info %>%
    dplyr::mutate_at("responder", as.factor)

  temp16 <- temp15 %>%
    dplyr::select(responder, seq_days, fitted) %>%
    dplyr::rename(dfi_right_part = fitted) %>%
    dplyr::bind_rows(right_dfi_each_day2) %>%
    dplyr::arrange(responder, seq_days) %>%
    dplyr::inner_join(temp1_base_info2) %>%
    dplyr::rename(corrected_dfi = dfi_right_part) %>%
    dplyr::inner_join(origin_dfi2) %>%
    dplyr::select(4, 6, 2, 5, 1, 7, 3) %>%
    dplyr::group_by(responder) %>%
    dplyr::mutate(begin_date_real = min(date),
           end_date_real = max(date))

  temp17 <- temp16 %>%
    dplyr::group_by(responder, location, stage) %>%
    dplyr::mutate(corrected_dfi = ifelse(corrected_dfi < 0, NA, corrected_dfi)) %>%
    dplyr::summarise(adfi = mean(corrected_dfi, na.rm = TRUE))

  temp18 <- adg_data %>%
    dplyr::mutate_at("responder", as.factor) %>%
    dplyr::left_join(temp17) %>%
    dplyr::mutate(fcr = adfi / adg_0) %>%
    dplyr::select(-c(4:8)) %>%
    dplyr::distinct()

  list(
    error_free_data_trans = temp9,
    model_results = temp14,
    dfi_results = temp16,
    fcr_results = temp18
  )
}
