#' raw data query for GOA RE/BS
#'
#' @param year
#' @param akfin_user
#' @param akfin_pwd
#' @param afsc_user
#' @param afsc_pwd
#'
#' @return
#' @export goa_nork
#'
#' @examples
goa_nork <- function(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd){

  # globals ----
  species = "NORK"
  area = "GOA"
  afsc_species = 30420
  norpac_species = 303

  # establish akfin connection
  akfin = DBI::dbConnect(odbc::odbc(), "akfin",
                          UID = akfin_user, PWD = akfin_pwd)

  # catch
  q_fishery_catch(year, fishery = "fsh1", species = species, area = area, akfin = akfin)
  q_fishery_obs(year, fishery = "fsh1", norpac_species = norpac_species, area, akfin)
  q_fishery_age_comp(year, fishery = "fsh1", norpac_species = norpac_species, area = area, akfin = akfin)
  q_fishery_length_comp(year, fishery = "fsh1", norpac_species = norpac_species, area = area, akfin = akfin)

  DBI::dbDisconnect(akfin)

  #establish afsc connection ----
  afsc = DBI::dbConnect(odbc::odbc(), "afsc",
                         UID = afsc_user, PWD = afsc_pwd)

  q_ts_biomass(year, survey = "goa", afsc_species = afsc_species, afsc = afsc)
  q_ts_age_comp(year, survey = "goa", afsc_species = norpac_species, afsc = afsc)
  q_ts_length_comp(year, survey = "goa", afsc_species = afsc_species, afsc = afsc)
  q_ts_saa(year, survey = "goa", afsc_species = afsc_species, afsc = afsc)

  DBI::dbDisconnect(afsc)


  q_date(year)
}