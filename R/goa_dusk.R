#' raw data query for GOA dusky rockfish
#'
#' @param year assessment year
#' @param akfin_user user name
#' @param akfin_pwd user password
#' @param afsc_user user name
#' @param afsc_pwd user password
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return
#' @export goa_dusk
#'
#' @examples
#' \dontrun{
#' goa_disk(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd)
#' }
goa_dusk <- function(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd, off_yr = NULL){

  # globals ----
  species = "DUSK"
  area = "GOA"
  afsc_species1 =  30150
  afsc_species2 = 30152
  norpac_species = 330


if(!is.null(off_yr)){
  # establish akfin connection
  akfin = DBI::dbConnect(odbc::odbc(), "akfin",
                          UID = akfin_user, PWD = akfin_pwd)
  # catch
  q_fish_catch(year, fishery = "fsh1", species = species, area = area, akfin = akfin)
  q_fish_obs(year, fishery = "fsh1", norpac_species = norpac_species, area, akfin)

  DBI::dbDisconnect(akfin)

  afsc = DBI::dbConnect(odbc::odbc(), "afsc",
                      UID = afsc_user, PWD = afsc_pwd)

  q_ts_biomass(year, area = "goa", afsc_species = c(afsc_species1, afsc_species2), afsc = afsc)

  DBI::dbDisconnect(afsc)

  } else{
  # establish akfin connection
  akfin <- DBI::dbConnect(odbc::odbc(), "akfin",
                          UID = akfin_user, PWD = akfin_pwd)

  # catch
  .c = sql_read("fsh1_catch.sql")
  .c = sql_filter(sql_precode = "<=", year, sql_code = .c, flag = "-- insert year")
  .c = sql_filter(x = area, sql_code = .c, flag = "-- insert region")
  .c = sql_filter(x = species, sql_code = .c, flag = "-- insert species")
  .c = paste(c(.c,
        "OR COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE = 'PEL7'
        OR COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE = 'PELS'"))

  sql_run(akfin, .c) %>%
    write.csv(here::here(year, "data", "raw", "fsh1_catch_data.csv"),
              row.names = FALSE)

  # establish akfin connection
  akfin = DBI::dbConnect(odbc::odbc(), "akfin",
                         UID = akfin_user, PWD = akfin_pwd)

  q_fish_obs(year, fishery = "fsh1", norpac_species = norpac_species, area, akfin)
  q_fish_age_comp(year, fishery = "fsh1", norpac_species = norpac_species, area = area, akfin = akfin)
  q_fish_length_comp(year, fishery = "fsh1", norpac_species = norpac_species, area = area, akfin = akfin)

  DBI::dbDisconnect(akfin)

  #establish afsc connection ----
  afsc = DBI::dbConnect(odbc::odbc(), "afsc",
                        UID = afsc_user, PWD = afsc_pwd)

  q_ts_biomass(year, area = "goa", afsc_species = c(afsc_species1, afsc_species2), afsc = afsc)
  q_ts_age_comp(year, area = "goa", afsc_species = c(afsc_species1, afsc_species2), afsc = afsc)
  q_ts_length_comp(year, area = "goa", afsc_species = c(afsc_species1, afsc_species2), afsc = afsc)
  q_ts_saa(year, area = "goa", afsc_species = c(afsc_species1, afsc_species2), afsc = afsc)

  DBI::dbDisconnect(afsc)

  file.copy(system.file("data", "goa_dusk_catch_1977_1990.rda", package = "groundfishr"),
            here::here(year, "data", "user_input"))
}
  q_date(year)

}
