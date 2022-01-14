#' fishery age composition analysis
#'
#' @param year assessment year
#' @param fishery default is fsh1, change if age comps from multiple fisheries (e.g., fsh2)
#' @param rec_age recruitment age
#' @param plus_age plus age group
#' @param save
#'
#' @return
#' @export  fish_age_comp
#'
#' @examples
#' \dontrun{
#' fish_age_comp(year, fishery = "fsh1", rec_age, plus_age)
#' }
fish_age_comp <- function(year, fishery = "fsh", rec_age, plus_age, save = TRUE){

  vroom::vroom(here::here(year, "data", "raw", paste0(fishery, "_age_comp_data.csv")),
           col_types = list(HAUL_JOIN = "c",
                          PORT_JOIN = "c")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(specimen_type!=3, !is.na(age), age>=rec_age) %>%
    dplyr::mutate(age = ifelse(age>plus_age, plus_age, age)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tot = dplyr::n()) %>%
    dplyr::filter(tot>49) %>%
    dplyr::mutate(n_h = length(unique(na.omit(haul_join))) + length(unique(na.omit(port_join)))) %>%
    dplyr::group_by(year, age) %>%
    dplyr::summarise(n_s = mean(tot),
                     n_h = mean(n_h),
                     age_tot = dplyr::n()) %>%
    dplyr::mutate(prop = age_tot / n_s) %>%
    dplyr::left_join(expand.grid(year = unique(.$year), age = rec_age:plus_age), .) %>%
    tidyr::replace_na(list(prop = 0)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(AA_Index = 1,
                  n_s = mean(n_s, na.rm = T),
                  n_h = mean(n_h, na.rm = T)) %>%
    dplyr::select(-age_tot) %>%
    tidyr::pivot_wider(names_from = age, values_from = prop) -> fac

  if(isTRUE(save)){
    readr::write_csv(fac, here::here(year, "data", "output", paste0(fishery, "_age_comp.csv")))
    fac
  } else {
    fac
  }


}
