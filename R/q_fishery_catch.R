#' Fishery catch data query
#'
#' @param year assessment year
#' @param fishery default is fsh1, change if age comps from multiple fisheries
#' @param species group species code
#' @param region GOA or BSAI
#' @param akfin the database to query
#' @param save save the file in designated folder
#'
#' @return
#' @export q_fishery_catch
#'
#' @examples
#'
q_fishery_catch <- function(year, fishery = "fsh1", species, area, akfin, save = TRUE){


  # get appropriate age compe query data for a specific fishery
  # e.g., ("fsh1", "fsh2")


  file <- grep(paste0(fishery,"_catch"),
               list.files(system.file("sql", package = "groundfishr")), value=TRUE)

  .c = sql_read(file)
  .c = sql_filter(sql_precode = "<=", year, sql_code = .c, flag = "-- insert year")
  .c = sql_filter(x = area, sql_code = .c, flag = "-- insert region")
  .c = sql_filter(x = species, sql_code = .c, flag = "-- insert species")

  if(species == "DUSK" & area == "GOA"){

  } else {

  }

  if(isTRUE(save)){

  sql_run(akfin, .c) %>%
    write.csv(here::here(year, "data", "raw", paste0(fishery, "_catch_data.csv")),
              row.names = FALSE)
  } else {
    sql_run(akfin, .c)
  }
}
