' Fishery length comp data query
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
q_fishery_length_comp <- function(year, fishery = "fsh1", norpac_species, area, akfin, save = TRUE){


  # get appropriate age compe query data for a specific fishery
  # e.g., ("fsh1", "fsh2")

  files <- grep(paste0(fishery,"_length"),
                list.files(system.file("sql", package = "groundfishr")), value=TRUE)


  .length = sql_read(files[1])
  .length = sql_filter(x = area, sql_code = .length, flag = "-- insert region")

  .length_h = sql_read(files[2])
  .length_h = sql_filter(x = area, sql_code = .length_h, flag = "-- insert region")

  .length_p = sql_read(files[3])
  .length_p = sql_filter(x = area, sql_code = .length_p, flag = "-- insert region")

  if(length(norpac_species) == 1){

    .length = sql_filter(x = norpac_species, sql_code = .length, flag = "-- insert species")
    .length_h = sql_filter(x = norpac_species, sql_code = .length_h, flag = "-- insert species")
    .length_p = sql_filter(x = norpac_species, sql_code = .length_p, flag = "-- insert species")

  } else {
    .length = sql_filter(sql_precode = "IN", x = norpac_species,
                      sql_code = .length, flag = "-- insert species")
    .length_h = sql_filter(sql_precode = "IN", x =(norpac_species),
                        sql_code = .length_h, flag = "-- insert species")
    .length_p = sql_filter(sql_precode = "IN", x = norpac_species,
                        sql_code = .length_p, flag = "-- insert species")

  }



  if(isTRUE(save)){
  dplyr::bind_cols(sql_run(akfin, .length),
                   sql_run(akfin, .length_h),
                   sql_run(akfin, .length_p)) %>%
    write.csv(here::here(year, "data", "raw", paste0(fishery, "_length_comp_data.csv")),
              row.names = FALSE)
  } else {
    dplyr::bind_cols(sql_run(akfin, .length),
                     sql_run(akfin, .length_h),
                     sql_run(akfin, .length_p))
  }
}

