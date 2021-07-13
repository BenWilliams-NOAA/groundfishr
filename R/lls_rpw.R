#' longline survey relative population weight
#'
#' @param year of interest
#' @param area "goa", "bsai"
#' @param file using an alt-ll survey rpw? (file must have format: year, rpw, cv, sd, lci, uci)
#' @param filter_yrs any years that would like removed from survey biomass estimates e.g., 1997 or c(2001, 2003)
#'
#' @return
#' @export lls_rpw
#'
#' @examples
#'
lls_rpw <- function(year, area, file = NULL, filter_yrs = NULL){

  # For GOA only at this time
  if(is.null(filter_yrs)){
    if(is.null(file)){

      read.csv(here::here(year, "data", "raw", paste0(area, "_lls_biomass_data.csv"))) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::filter(survey != "Japan",
                      area_id >= 3, # goa
                      year > 1992) %>% # poor data prior
        dplyr::group_by(year) %>%
        dplyr::summarise(rpw = sum(rpw)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cv = sqrt(var(rpw)) / mean(rpw),
                      sd = cv * rpw,
                      lci = rpw - 1.96 * sd,
                      uci = rpw + 1.96 * sd) %>%
        dplyr::mutate(lci = ifelse(lci < 0, 0, lci)) %>%
        dplyr::mutate_if(is.double, round) %>%
        dplyr::select(-cv) -> sb
    } else {
      read.csv(here::here(year, "data", "user_input", file)) -> sb
    }
  } else {
    if(is.null(file)){

      read.csv(here::here(year, "data", "raw", paste0(area, "_lls_biomass_data.csv"))) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::filter(!(year %in% filter_yrs)) %>%
        dplyr::filter(survey != "Japan",
                      area_id >= 3, # goa
                      year > 1992) %>% # poor data prior
        dplyr::group_by(year) %>%
        dplyr::summarise(rpw = sum( )) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cv = sqrt(var(rpw)) / mean(rpw),
                      sd = cv * rpw,
                      lci = rpw - 1.96 * sd,
                      uci = rpw + 1.96 * sd) %>%
        dplyr::mutate(lci = ifelse(lci < 0, 0, lci)) %>%
        dplyr::mutate_if(is.double, round) %>%
        dplyr::select(-cv)
    } else {
      read.csv(here::here(year, "data", "user_input", file)) %>%
        dplyr::filter(!(year %in% filter_yrs)) -> sb
    }

  }

  write.csv(sb, here::here(year, "data", "output", paste0(area, "_lls_biomass.csv")), row.names = FALSE)

  sb
}
