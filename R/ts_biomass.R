#' survey biomass
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
      dplyr::rename_all(tolower)  -> df

    # sablefish are different...
    if("summary_depth" %in% names(df)){
      df %>%
      dplyr::filter(summary_depth < 995, year != 2001) %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(biom = sum(area_biomass) / 1000,
                         se = sqrt(sum(biomass_var)) / 1000) %>%
        dplyr::mutate(lci = biom - 1.96 * se,
                      uci = biom + 1.96 * se) -> sb
    } else{
      df %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(biomass = sum(total_biomass),
                       se = sqrt(sum(biomass_var)),
                       lci = biomass - 1.96 * se,
                       uci = biomass + 1.96 * se) %>%
        dplyr::mutate(lci = ifelse(lci < 0, 0, lci)) %>%
        dplyr::mutate_if(is.double, round) %>%
        dplyr::filter(biomass > 0) -> sb
    }
  } else {
    read.csv(here::here(year, "data", "user_input", file)) -> sb
  }

  write.csv(sb, here::here(year, "data", "output", paste0(survey, "_ts_biomass.csv")), row.names = FALSE)

  sb
}
