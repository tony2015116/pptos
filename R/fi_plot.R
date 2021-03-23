#' fi_plot
#'
#' @param data the import csv data
#'
#' @return histgram plot of all feed intake
#' @export
#'
#' @examples
fi_plot <- function(data) {
  fiv <- visit_time <- NULL
  ggplot2::ggplot(data, ggplot2::aes(fiv)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::stat(dplyr::count) / sum(dplyr::count)), binwidth = 5) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_bw()
}
