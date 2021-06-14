#' raw data query for GOA dusky rockfish
#'
#' @param year assessment year
#' @param akfin_user user name
#' @param akfin_pwd user password
#' @param afsc_user user name
#' @param afsc_pwd user password
#'
#' @return
#' @export goa_dusk
#'
#' @examples
#' \dontrun{
#' goa_disk(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd)
#' }
goa_dusk <- function(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd){

  # globals ----
  species = "DUSK"
  area = "GOA"
  afsc_species =  30150
  afsc_species2 = 30152
  norpac_species = 330

  # establish akfin connection
  akfin <- DBI::dbConnect(odbc::odbc(), "akfin",
                          UID = akfin_user, PWD = akfin_pwd)

  # catch
  .c = sql_read("catch.sql")
  .c = sql_filter(sql_precode = "<=", year, sql_code = .c, flag = "-- insert year")
  .c = sql_filter(x = area, sql_code = .c, flag = "-- insert region")
  .c = sql_filter(x = species, sql_code = .c, flag = "-- insert species")
  .c = paste(c(.c,
        "OR COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE = 'PEL7'
        OR COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE = 'PELS'"))

  sql_run(akfin, .c) %>%
    write.csv(here::here(year, "data", "raw", "fsh1_catch_data.csv"),
              row.names = FALSE)


}
