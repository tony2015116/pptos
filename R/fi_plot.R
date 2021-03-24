#' fi_plot
#'
#' @param data the import csv data
#'
#' @return histgram plot of all feed intake
#' @export
#'
#' @examples
#' #fi_plot(data = test)
fi_plot <- function(data) {
  feed_intake <- NULL
  ggplot2::ggplot(data, ggplot2::aes(feed_intake)) +
    ggplot2::geom_histogram(binwidth = 8) +
    ggplot2::theme_bw()
}
