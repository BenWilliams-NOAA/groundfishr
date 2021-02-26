#' Fishery age comp data query
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
q_fishery_age_comp <- function(year, fishery = "fsh1", norpac_species, area, akfin, save = TRUE){


  # get appropriate age compe query data for a specific fishery
  # e.g., ("fsh1", "fsh2")

files <- grep(paste0(fishery,"_age"),
              list.files(system.file("sql", package = "groundfishr")), value=TRUE)

  .age = sql_read(files[1])
  .age = sql_filter(x = area, sql_code = .age, flag = "-- insert region")

  .age_h = sql_read(files[2])
  .age_h = sql_filter(x = area, sql_code = .age_h, flag = "-- insert region")

  .age_p = sql_read(files[3])
  .age_p = sql_filter(x = area, sql_code = .age_p, flag = "-- insert region")


if(length(norpac_species) == 1){

    .age = sql_filter(x = norpac_species, sql_code = .age, flag = "-- insert species")
    .age_h = sql_filter(x = norpac_species, sql_code = .age_h, flag = "-- insert species")
    .age_p = sql_filter(x = norpac_species, sql_code = .age_p, flag = "-- insert species")

  } else {

    .age = sql_filter(sql_precode = "IN", x = norpac_species,
                      sql_code = .age, flag = "-- insert species")
    .age_h = sql_filter(sql_precode = "IN", x = norpac_species,
                        sql_code = .age_h, flag = "-- insert species")
    .age_p = sql_filter(sql_precode = "IN", x = norpac_species,
                        sql_code = .age_p, flag = "-- insert species")
  }

  if(isTRUE(save)){
  dplyr::bind_cols(sql_run(akfin, .age),
                   sql_run(akfin, .age_h),
                   sql_run(akfin, .age_p)) %>%
            write.csv(here::here(year, "data", "raw", paste0(fishery, "_age_comp_data.csv")),
            row.names = FALSE)
  } else {
    dplyr::bind_cols(sql_run(akfin, .age),
                     sql_run(akfin, .age_h),
                     sql_run(akfin, .age_p))
  }
}

