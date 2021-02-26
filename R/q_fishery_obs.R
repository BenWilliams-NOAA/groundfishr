#' fishery observer data query
#'
#' @param year assessment year
#' @param fishery default is fsh1, change if age comps from multiple fisheries
#' @param norpac_species norpac species code
#' @param region GOA or BSAI
#' @param akfin the database to query
#' @param save save the file in designated folder
#'
#' @return
#' @export
#'
#' @examples
#'
q_fishery_obs <- function(year, fishery = "fsh1", norpac_species, area, akfin, save = TRUE){

  # get appropriate age compe query data for a specific fishery
  # e.g., ("fsh1", "fsh2")
  file <- grep(paste0(fishery,"_obs"),
                list.files(system.file("sql", package = "groundfishr")), value=TRUE)

  .obs = sql_read(file)
  .obs = sql_filter(sql_precode = "", x = year-3, sql_code = .obs, flag = "-- insert year")
  .obs = sql_filter(sql_precode = "", x = year-1, sql_code = .obs, flag = "-- year2")
  .obs = sql_filter(x = area, sql_code = .obs, flag = "-- insert region")

  if(length(norpac_species) == 1){
    .obs = sql_filter(x = norpac_species, sql_code = .obs, flag = "-- insert species")
  } else {
    .obs = sql_filter(sql_precode = "IN", x = norpac_species,
                      sql_code = .obs, flag = "-- insert species")
  }


  dat = sql_run(akfin, .obs)

  if(isTRUE(save)){

      write.csv(dat, here::here(year, "data", "raw", paste0(fishery, "_obs_data.csv")),
                row.names = FALSE)
  } else {
    dat
  }


}
