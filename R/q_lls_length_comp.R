#' Longline survey length comp data query
#'
#' @param year assessment year
#' @param area default is fsh1, change if age comps from multiple fisheries
#' @param afsc_species afsc species code
#' @param akfin the database to query
#' @param save save the file in designated folder
#'
#' @return
#' @export
#'
#' @examples
#'
q_lls_length_comp <- function(year, area = "goa", afsc_species, akfin, save = TRUE){

  files <- grep(paste0(area,"_lls_length"),
                list.files(system.file("sql", package = "groundfishr")), value=TRUE)

  .one = sql_read(files[1])
  .two = sql_read(files[2])

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
    sql_run(akfin, .one) %>%
      write.csv(here::here(year, "data", "raw", paste0(area, "_lls_length_data.csv")),
                row.names = FALSE)
    sql_run(akfin, .two) %>%
      write.csv(here::here(year, "data", "raw", paste0(area, "_lls_specimen_data.csv")),
                row.names = FALSE)
  } else {
    list(sql_run(akfin, .one),
         sql_run(akfin, .two))
  }

}
