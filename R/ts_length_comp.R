#' trawl survey length composition analysis
#'
#' @param year assessment year
#' @param area survey area default = "goa"
#' @param lenbins lenbin file if left NULL it looks for (year/data/user_input/len_bins.csv")
#' @param bysex should the length comp be calculated by sex - default is null (not differentiated)
#' @return
#' @export ts_length_comp
#'
#' @examples
#'
ts_length_comp <- function(year, area = "goa", lenbins = NULL, bysex = NULL){


  read.csv(here::here(year, "data", "raw", paste0(area, "_ts_length_data.csv"))) %>%
    dplyr::rename_all(tolower) -> df

  if(!("summary_depth" %in% names(df)) & is.null(lenbins)){
    stop("Please provide the length bin file that is in the user_input folder e.g.,('lengthbins.csv')")
  } else if(!("summary_depth" %in% names(df))){
    lenbins =  vroom::vroom(here::here(year, "data", "user_input", lenbins), delim = ",")$len_bins
  }

  vroom::vroom(here::here(year, "data", "raw", paste0(area, "_ts_length_specimen_data.csv"))) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!is.na(length)) %>%
    dplyr::mutate(length = length / 10) -> dat

  if("frequency" %in% colnames(dat)){
    dat %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(n_s = sum(frequency),
                       n_h = length(unique(hauljoin))) %>%
      dplyr::ungroup() -> dat
  } else {
    dat %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(n_s = dplyr::n(),
                       n_h = length(unique(hauljoin))) %>%
      dplyr::ungroup() -> dat
  }

  if("summary_depth" %in% names(df)){
    df %>%
      dplyr::rename_all(tolower) %>%
      dplyr::filter(summary_depth < 995, year != 2001) %>%
      tidyr::pivot_longer(cols = c(males, females, unsexed)) %>%
      dplyr:: mutate(bin = round((length / 10 - 0.5) / 20, 1) * 20 + 1) %>%
      dplyr::filter(bin %in% lenbins) %>%
      dplyr::group_by(year, name, bin) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(bin, tidyr::nesting(year, name), fill = list(value = 0)) %>%
      dplyr::group_by(year, name) %>%
      dplyr::mutate(prop = value / sum(value)) %>%
      dplyr::select(-value) %>%
      dplyr::left_join(dat) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(SA_Index = 1,
                    n_s = mean(n_s, na.rm = T),
                    n_h = mean(n_h, na.rm = T)) %>%
      tidyr::pivot_wider(names_from = bin, values_from = prop) -> size_comp
  } else {
    df %>%
      dplyr::rename_all(tolower) %>%
      dplyr::mutate(length = length / 10,
                    length = ifelse(length >= max(lenbins), max(lenbins), length)) %>%
      dplyr::filter(length %in% lenbins) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(tot = sum(total)) %>%
      dplyr::group_by(year, length) %>%
      dplyr::summarise(prop = sum(total) / mean(tot)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(expand.grid(year = unique(.$year), length = lenbins), .) %>%
      tidyr::replace_na(list(prop = 0)) %>%
      dplyr::left_join(dat) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(SA_Index = 1,
                    n_s = mean(n_s, na.rm = T),
                    n_h = mean(n_h, na.rm = T)) %>%
      tidyr::pivot_wider(names_from = length, values_from = prop) -> size_comp
  }

  vroom::vroom_write(size_comp, here::here(year, "data", "output", paste0(area, "_ts_length_comp.csv")), ",")

  size_comp

}
