#' size at age for 1960s POP
#'
#' @param year analysis year
#' @param rec_age recruitment age
#' @param lenbins length bin file - this should be inyour user_input folder
#' @return
#' @export size_at_age_pop_60
#'
#' @examples
size_at_age_pop_60 <- function(year, rec_age, lenbins = NULL){

  if (!file.exists(here::here(year,"data", "output", "ae_model.csv"))){
    stop("You must first run the age-error function 'ageage()")
  } else {
    nages_m = nrow(read.csv(here::here(year, "data", "output", "ae_model.csv")))
    ages_m = rec_age:(rec_age + nages_m - 1)
  }

  if(is.null(lenbins)){
    stop("Please provide the length bin file that is in the user_input folder e.g.,('lengthbins.csv')")
  } else {
    lenbins = read.csv(here::here(year, "data", "user_input", lenbins))$len_bins
  }

  # move parameters to user_input folder
  file.copy(system.file("data", "saa_pop_60.rda", package = "groundfishr"),
            here::here(year, "data", "user_input"))

  pars = read.csv(here::here(year, "data", "user_input", "saa_pop_60.rda"))

expand.grid(age = ages_m,
            length = lenbins) %>%
  dplyr::mutate(lbar = pars$linf * (1 - exp(-pars$k * (age - pars$t0))),
                lbar = ifelse(age == max(ages_m), 0.5 * (lbar + pars$linf), lbar),
                sd_lbar = pars$a * log(age) + pars$b,
                prob = ifelse(length == min(length),
                              pnorm(length + 0.5, lbar, sd_lbar),
                              pnorm(length + 0.5, lbar, sd_lbar) -
                              pnorm(length -0.5, lbar, sd_lbar)),
                prob = round(prob, digits = 4)) %>%
  dplyr::select(age, length, prob) %>%
  tidyr::pivot_wider(names_from = length, values_from = prob) %>%
  dplyr::mutate(!!rev(names(.))[1] := 1 - rowSums(.[2:(ncol(.) - 1)])) %>%
  dplyr::mutate_at(2:ncol(.), round, 4)
}
