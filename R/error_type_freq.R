#' error_type_freq
#'
#' @param data transformed data from function of transform
#'
#' @return frequency of 16 error types
#' @export
#'
#' @examples
#' #error_type_freq(data = temp1)
error_type_freq <- function(data) {
  error_type <- x <- . <- Freq <- NULL
  proportions1 <-
    lapply(data.table::copy(data)[, 18:33], function(x) {
      prop.table(table(x))
    })
  prop2 <- purrr::map(proportions1, as.data.frame)
  prop3 <-
    data.table::rbindlist(prop2,
              use.names = TRUE,
              fill = TRUE,
              idcol = "error_type")
  prop4 <- prop3[data.table::CJ(error_type = error_type, x = x, unique=TRUE), on=.(error_type, x)][,"Freq/%" := 100*Freq][,Freq := NULL]
  prop5 <- data.table::dcast(prop4, error_type ~ x, value.var = "Freq/%")
  return(prop5)
}
