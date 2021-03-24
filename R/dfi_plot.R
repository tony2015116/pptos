#' dfi_plot
#'
#' @param dfi_data the resluts from dfi_get
#' @param path_out the path to put results
#'
#' @return pictures of dfi
#' @export
#'
#' @examples
#' #temp5 <- dfi_get(origin_data = temp1, adg_data = temp4)
dfi_plot <- function(dfi_data, path_out) {
  . <- location <- data <- NULL
  corrected_dfi <- data.table::setDT(dfi_data)
  mDT <- data.table::melt(
    corrected_dfi,
    measure.vars  = c("origin_dfi", "corrected_dfi"),
    variable.name = "state",
    value.name    = "dfi"
  )

  niuhei = mDT[, .(data = list(.SD)), location][, plot := purrr::map2(
    data,
    location,
    ~ ggplot2::ggplot(data = .x, ggplot2::aes(
      x = date, y = dfi, color = state
    )) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(size = 1) +
      ggplot2::geom_line(ggplot2::aes(linetype = state), show.legend = F) +
      ggplot2::scale_x_date(date_breaks = "1 day", date_labels =  "%m-%d") +
      ggplot2::facet_wrap(~ responder, ncol = 2) +
      ggplot2::labs(title = paste("Location:", .y)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = -90, size = 8),
        plot.title = element_text(size = 25, face = "bold")
      )
  )][]
  purrr::walk2(
    paste0(path_out, niuhei$location, "_dfi.png"),
    niuhei$plot,
    ggplot2::ggsave,
    width = 35,
    height = 48,
    units = "cm",
    dpi = "retina"
  )
}


