#' Trawl survey size at age data query
#'
#' @param year assessment year
#' @param area default is "goa", change if age comps from multiple fisheries
#' @param afsc_species afsc species code
#' @param afsc the database to query
#' @param save save the file in designated folder
#'
#' @return
#' @export
#'
#' @examples
#'
q_ts_saa <- function(year, area = "goa", afsc_species,  afsc, save = TRUE){

  .files <- grep(paste0(area,"_ts_saa"),
                list.files(system.file("sql", package = "groundfishr")), value=TRUE)

  .one = sql_read(.files[1])
  .two = sql_read(.files[2])

  if(length(afsc_species) == 1){

    .one = sql_filter(x = afsc_species, sql_code = .one, flag = "-- insert species")
    .two = sql_filter(x = afsc_species, sql_code = .two, flag = "-- insert species")
  } else {
    .one = sql_filter(sql_precode = "IN", x = afsc_species,
                      sql_code = .one, flag = "-- insert species")
    .two = sql_filter(sql_precode = "IN", x = afsc_species,
                      sql_code = .two, flag = "-- insert species")
  }

  if(isTRUE(save)){
    sql_run(afsc, .one) %>%
      write.csv(here::here(year, "data", "raw", paste0(area, "_ts_saa_age_data.csv")),
                row.names = FALSE)
    sql_run(afsc, .two) %>%
      write.csv(here::here(year, "data", "raw", paste0(area, "_ts_saa_length_data.csv")),
                row.names = FALSE)
  } else {
    list(sql_run(afsc, .one),
         sql_run(afsc, .two))
  }

}
