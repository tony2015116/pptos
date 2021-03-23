#' error_type_freq
#'
#' @param data transformed data from function of transform
#'
#' @return frequency of 16 error types
#' @export
#'
#' @examples
error_type_freq <- function(data) {
  proportions1 <-
    lapply(copy(data)[, 18:33], function(x) {
      prop.table(table(x))
    })
  prop2 <- map(proportions1, as.data.frame)
  prop3 <-
    rbindlist(prop2,
              use.names = TRUE,
              fill = TRUE,
              idcol = "error_type")
  prop4 <- dcast(prop3, error_type ~ x, value.var = "Freq")
  return(prop4)
}
