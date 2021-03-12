#' Get the survey biomass
#'
#' @param year of interest
#' @param type if not using the design-based abundance, the file name must be stated (e.g. "GAP_VAST.csv")
#' @param file
#'
#' @return
#' @export ts_biomass
#'
#' @examples
#'
ts_biomass <- function(year, survey = "goa", file = NULL){

  if(is.null(file)){

    read.csv(here::here(year, "data", "raw", "goa_ts_biomass_data.csv")) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(biomass = total_biomass,
                       se = sqrt(biomass_var),
                       lci = biomass - 1.96 * se,
                       uci = biomass + 1.96 * se) %>%
      dplyr::mutate(lci = ifelse(lci < 0, 0, lci)) %>%
      dplyr::mutate_if(is.double, round) %>%
    dplyr::filter(biomass > 0) -> sb
  } else {
    read.csv(here::here(year, "data", "user_input", file)) -> sb
  }

  write.csv(sb, here::here(year, "data", "output", paste0(survey, "_ts_biomass.csv")), row.names = FALSE)

  sb
}
