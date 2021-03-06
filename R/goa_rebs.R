#' raw data query for GOA RE/BS rockfish
#'
#' @param year
#' @param akfin_user
#' @param akfin_pwd
#' @param afsc_user
#' @param afsc_pwd
#'
#' @return
#' @export goa_rebs
#'
#' @examples
goa_rebs <- function(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd){

  # globals ----
  species = "REYE"
  area = "GOA"
  afsc_species = 30050
  afsc_species2 = 30051
  afsc_species3 = 30052
  norpac_species = 307
  norpac_species2 = 357

  # establish akfin connection
  akfin <- DBI::dbConnect(odbc::odbc(), "akfin",
                          UID = akfin_user, PWD = akfin_pwd)

  # catch
  q_fish_catch(year, fishery = "fsh1", species = species, area = area, akfin = akfin)
  q_fish_obs(year, fishery = "fsh1", norpac_species = c(norpac_species, norpac_species2),
                area, akfin)
  q_fish_age_comp(year, fishery = "fsh1", norpac_species = c(norpac_species, norpac_species2),
                     area = area, akfin = akfin)
  q_fish_length_comp(year, fishery = "fsh1", norpac_species = c(norpac_species, norpac_species2),
                        area = area, akfin = akfin)
  q_lls_biomass(year, area = "goa", afsc_species = afsc_species, akfin = akfin)

  q_lls_length_comp(year, area = "goa", afsc_species = afsc_species, akfin = akfin)

  DBI::dbDisconnect(akfin)

  #establish afsc connection ----
  afsc = DBI::dbConnect(odbc::odbc(), "afsc",
                        UID = afsc_user, PWD = afsc_pwd)

  q_ts_biomass(year, area = "goa",
               afsc_species = c(afsc_species, afsc_species2, afsc_species3), afsc = afsc)
  q_ts_age_comp(year, area = "goa",
                afsc_species = c(afsc_species, afsc_species2, afsc_species3), afsc = afsc)
  q_ts_length_comp(year, area = "goa",
                   afsc_species = c(afsc_species, afsc_species2, afsc_species3), afsc = afsc)
  q_ts_saa(year, area = "goa",
           afsc_species = c(afsc_species, afsc_species2, afsc_species3), afsc = afsc)

  DBI::dbDisconnect(afsc)

  q_date(year)

}
