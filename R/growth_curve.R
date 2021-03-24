#' growth_curve
#'
#' @param data results from function of outlier_found
#' @param path_out the path to put your results
#' @param weighting the weighting you choose to determine outliers
#'
#' @return pictures of growth curve
#' @export
#'
#' @examples
#' #growth_curve(data = temp2, w = 0.5, path_out = "")
growth_curve <- function(data, path_out, weighting) {
  colors <- w <- . <- location <- NULL
  temp = data.table::copy(data)[, colors := data.table::fifelse(w >= {
    {
      weighting
    }
  }, "Normal", "Outlier")][, .(data = list(.SD)), location][, plot := purrr::map2(
    data,
    location,
    ~ ggplot2::ggplot(data = .x, ggplot2::aes(x = date, y = weight)) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(size = 1, na.rm = F) +
      ggplot2::geom_point(ggplot2::aes(col = colors)) +
      ggplot2::scale_color_manual(
        values = c("Normal" = "green", "Outlier" = "red"),
        name = "robust regression"
      ) +
      ggplot2::scale_x_date(date_breaks = "1 day", date_labels =  "%m-%d") +
      ggplot2::geom_line(ggplot2::aes(x = date, y = rlm_predict)) +
      ggplot2::facet_wrap(~ as.numeric(responder), ncol = 2) +
      ggplot2::scale_y_continuous(
        breaks = seq(15000, 130000, 10000),
        limits = c(15000, 130000)
      ) +
      ggplot2::labs(title = paste("Location:", .y)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = -90, size = 10),
        plot.title = element_text(size = 25, face = "bold")
      )
  )][]
  purrr::walk2(
    paste0(path_out, temp$location, ".png"),
    temp$plot,
    ggplot2::ggsave,
    width = 35,
    height = 45,
    units = "cm",
    dpi = "retina"
  )
}
