#' date of data query
#'
#' @param year assessment year
#'
#' @return
#' @export
#'
#' @examples
q_date <- function(year){
  txt = "Data were downloaded on:"
  dt = format(Sys.time(), "%Y-%m-%d")


  write.table(c(txt, dt), file = here::here(year, "data", "raw", "data_called.txt"),
              sep = "\t", col.names = F, row.names = F)
}
