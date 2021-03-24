#' raw data query for sablefish
#'
#' @param year
#' @param akfin_user
#' @param akfin_pwd
#' @param afsc_user
#' @param afsc_pwd
#'
#' @return
#' @export sablefish
#'
#' @examples
sablefish <- function(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd){

  # globals ----
  species = "SABL"
  area = c("GOA", "BSAI")
  afsc_species =  20510
  norpac_species = 203

  # establish akfin connection
  akfin <- DBI::dbConnect(odbc::odbc(), "akfin",
                          UID = akfin_user, PWD = akfin_pwd)

  # catch data ----
  .c = sql_read("catch.sql")
  .c = sql_filter(sql_precode = "<=", year, sql_code = .c, flag = "-- insert year")
  .c = sql_filter(sql_precode = "IN", area, sql_code = .c, flag = "-- insert region")
  .c = sql_filter(x = species, sql_code = .c, flag = "-- insert species")

  sql_run(akfin, .c) %>%
    rename_all(tolower) %>%
    group_by(agency_gear_code) %>%
    mutate(year = as.numeric(year),
           type = case_when(agency_gear_code %in% c("BTR", "PTR", "NPT", "TRW") ~ "trawl",
                            agency_gear_code %in% c("HAL", "POT") ~ "fixed")) -> .df

  .df %>%
    filter(type == "fixed") %>%
    write.csv(here::here(year, "data", "raw", "fsh1_catch_data.csv"),
              row.names = FALSE)

  .df %>%
    filter(type == "trawl") %>%
    write.csv(here::here(year, "data", "raw", "fsh2_catch_data.csv"),
              row.names = FALSE)

  # whale depredation ----
  sql_read("catch.sql") %>%
    rename_all(tolower) %>%
    write.csv(here::here(year, "data", "raw", "fsh1_whale_dep_data.csv"),
              row.names = FALSE)

}
