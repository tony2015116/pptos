#' import_csv
#'
#' @param file_list full path list files
#' @param package from data.table, vroom or readr
#' @param ... other parameters in three packages
#'
#' @return a data.table format
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom data.table fread
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @importFrom vroom vroom
#' @importFrom readr read_csv
#'
#' @examples
#' #all_csv <- import_csv(file_list, package = "data.table", header = T, skip = 0)
#' #all_csv <- import_csv(file = file_list, package = "data.table",header = F)
#' #all_csv <- import_csv(file = file_list, package = "vroom", col_names = T)
#' #all_csv <- import_csv(file = file_list, package = "readr", col_names = T, col_types = "dcddcddddc")



import_csv <- function(file_list, package, ...) {
  if (package == "data.table") {
    data.table::rbindlist(purrr::map(file_list, function(x, ...) {
      data.table::fread(x, ...)
    }, ...))
  } else if (package == "vroom") {
    vroom::vroom(file_list, ...)
  } else if (package == "readr") {
    purrr::map_dfr(file_list, function(x, ...) {
      readr::read_csv(x, ...)
    }, ...)
  }
}
