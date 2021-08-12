#' size at age analysis
#'
#' @param year analysis year
#' @param admb_home location admb exists on your computer
#' @param rec_age recruitment age
#' @param max_age max age for age error analysis - default = 100
#' @param lenbins length bin file
#' @return
#' @export size_at_age
#'
#' @examples
size_at_age <- function(year, admb_home = NULL, rec_age, lenbins = NULL){

  if (!dir.exists(here::here(year, "data", "output"))){
    dir.create(here::here(year, "data", "output"), recursive=TRUE)
  }

  if(is.null(admb_home)){
    R2admb::setup_admb()
  } else {
    R2admb::setup_admb(admb_home)
  }

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

  read.csv(here::here(year, "data", "raw", "goa_ts_saa_age_data.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(year, age, length) %>%
    dplyr::filter(year>=1990, !is.na(age))  %>%
    dplyr::select(-year) %>%
    dplyr::group_by(age) %>%
    dplyr::filter(dplyr::n()>1) %>%
    dplyr::group_by(length) %>%
    dplyr::mutate(n_l = dplyr::n()) %>%
    dplyr::arrange(age, length) %>%
    dplyr::group_by(age) %>%
    dplyr::mutate(sample_size =  dplyr::n()) -> inter

  dat <- read.csv(here::here(year, "data", "raw", "goa_ts_saa_length_data.csv")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(year>=1990, !is.na(length))

  if(is.null(dat$FREQUENCY)){
    dat %>%
      dplyr::group_by(length) %>%
      dplyr::summarise(tot = dplyr::n()) -> dat
  } else {
  dat %>%
    dplyr::select(frequency, length) %>%
    dplyr::group_by(length) %>%
    dplyr::summarise(tot = sum(frequency)) -> dat
  }

  dat %>%
    dplyr::left_join(inter, .) %>%
    dplyr::group_by(age, length) %>%
    dplyr::mutate(prop =  dplyr::n() / n_l * tot) %>%
    dplyr::distinct() %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(sample_size = mean(sample_size),
                     Lbar = sum(prop * length) / sum(prop) * 0.1,
                     SD_Lbar = sqrt(1 / (sum(prop) - 1) * sum(prop * (length / 10 - Lbar)^2))) %>%
    dplyr::filter(SD_Lbar>=0.01) -> laa_stats

  write.csv(laa_stats, here::here(year, "data", "output", "laa_stats.csv"), row.names = FALSE)

  laa_stats


  # run models ----

  setwd(here::here(year, "data", "models", "vonb"))
  # Estimate mean length
  c("# Data file for LVB model of mean length",
    "# Number of ages (nages)",
    nrow(laa_stats),
    "# Ages with observed mean length (ages)",
    paste(laa_stats$age, collapse=" "),
    "# Observed mean length (Lbar_obs)",
    paste(laa_stats$Lbar, collapse=" "),
    "# SD in Observed mean length (Lbar_obs)",
    paste(laa_stats$SD_Lbar, collapse=" ")) %>%
    write.table("vbl.dat", quote=FALSE, row.names=FALSE, col.names=FALSE)

  R2admb::compile_admb("vbl", verbose = TRUE)
  R2admb::run_admb("vbl", verbose = TRUE)

  # retrieve output

  REP = readLines("vbl.rep", warn=FALSE)
  Linf = as.numeric(sub(".*? ", "", REP[1]))
  k = as.numeric(sub(".*? ", "", REP[2]))
  t0 = as.numeric(sub(".*? ", "", REP[3]))


  # run model 2
  setwd(here::here(year, "data", "models", "length_sd"))

  c("# Data file for LVB model of mean length",
    "# Number of ages (nages)",
    nrow(laa_stats),
    "# Ages with observed mean length (ages)",
    paste(laa_stats$age, collapse=" "),
    "# SD in Observed mean length (Lbar_obs)",
    paste(laa_stats$SD_Lbar, collapse=" "),
    "# Sample size vector",
    paste(laa_stats$sample_size, collapse=" ")) %>%
    write.table("lengthsd.dat", quote=FALSE, row.names=FALSE, col.names=FALSE)


  R2admb::compile_admb("lengthsd", verbose = TRUE)
  R2admb::run_admb("lengthsd", verbose = TRUE)
  STD <- read.delim("lengthsd.std", sep="")
  a <- STD$value[1]
  b <- STD$value[2]
  (params <- cbind(Linf, k, t0, a, b))

  write.csv(params, here::here(year, "data", "output", "lbar_params.csv"), row.names = FALSE)


  # Compute Sz@A transition matrix

  expand.grid(age = ages_m,
              length = lenbins) %>%
    dplyr::mutate(Lbar = Linf * (1 - exp(-k * (age - t0))),
                  Lbar = ifelse(age == max(ages_m), 0.5 * (Lbar + Linf), Lbar),
                  SD_Lbar = a * log(age) + b,
                  prob = ifelse(length == min(length),
                                pnorm(length + 0.5, Lbar, SD_Lbar),
                                pnorm(length + 0.5, Lbar, SD_Lbar) -
                                  pnorm(length -0.5, Lbar, SD_Lbar)),
                  prob = round(prob, digits = 4)) %>%
    dplyr::select(age, length, prob) %>%
    tidyr::pivot_wider(names_from = length, values_from = prob) %>%
    dplyr::mutate(!!rev(names(.))[1] := 1 - rowSums(.[2:(ncol(.) - 1)])) %>%
    dplyr::mutate_at(2:ncol(.), round, 4) -> saa

  write.csv(saa, here::here(year, "data", "output", "saa.csv"), row.names = FALSE)
  saa

}
