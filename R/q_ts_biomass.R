#' trawl survey biomass data query
#'
#' @param year
#' @param survey
#' @param afsc_species
#' @param afsc
#' @param save
#'
#' @return
#' @export
#'
#' @examples
q_ts_biomass <- function (year, survey = "goa", afsc_species, afsc, save = TRUE){

  files <- grep(paste0(survey,"_ts_age"),
                list.files(system.file("sql", package = "groundfishr")), value=TRUE)

  .bio = sql_read(files[1])

  if(length(afsc_species) == 1){
    .bio = sql_filter(x = norpac_species, sql_code = .bio, flag = "-- insert species")
  } else {
    .bio = sql_filter(sql_precode = "IN", x = norpac_species,
                      sql_code = .bio, flag = "-- insert species")
  }

  if(isTRUE(save)){
    sql_run(afsc, .bio) %>%
      write.csv(here::here(year, "data", "raw", paste0(survey, "_ts_biomass_data.csv")),
                row.names = FALSE)
  } else {
    sql_run(afsc, .bio)
  }
}


