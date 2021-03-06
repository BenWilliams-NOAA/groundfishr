#' trawl survey biomass data query
#'
#' @param year
#' @param area
#' @param afsc_species
#' @param afsc
#' @param save
#'
#' @return
#' @export
#'
#' @examples
q_ts_biomass <- function (year, area = "goa", afsc_species, afsc, save = TRUE){

  files <- grep("_ts",
                list.files(system.file("sql", package = "groundfishr")), value=TRUE)

  if(length(afsc_species) == 1){
    if(afsc_species == 20510){
    .bio = sql_read(files[9])
  } else {
    .bio = sql_read(files[1])
  }}


  if(length(afsc_species) == 1){
    .bio = sql_filter(x = afsc_species, sql_code = .bio, flag = "-- insert species")
  } else {
    .bio = sql_filter(sql_precode = "IN", x = afsc_species,
                      sql_code = .bio, flag = "-- insert species")
  }

  if(isTRUE(save)){
    sql_run(afsc, .bio) %>%
      write.csv(here::here(year, "data", "raw", paste0(area, "_ts_biomass_data.csv")),
                row.names = FALSE)
  } else {
    sql_run(afsc, .bio)
  }
}


