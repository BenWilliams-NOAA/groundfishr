#' aging error analysis
#'
#' @param read_tester = if left blank it looks for the following file:"data/user_input/reader_tester.csv"
#' @param species = "NORK"
#' @param year = year of the assessment
#' @param admb_home = location admb exists on your computer - if is "c:/admb" can leave NULL
#' @param area = "GOA" (BSAI not currently setup)
#' @param rec_age = recruitment age
#' @param plus_age = max age for modeling
#' @param max_age = max age for age error analysis - default = 100
#'
#' @return
#' @export age_error
#'
#' @examples ageage(species = "NORK", year = 2020, admb_home = NULL, area = "GOA", rec_age = 2, plus_age = 45, max_age = 100, ...)
#'
age_error <- function(reader_tester = NULL, species, year, admb_home = NULL, area = "GOA", rec_age = 2, plus_age = 45, max_age = 100){


  if(is.null(reader_tester)){
    rt = read.csv(here::here(year, "data", "user_input", "reader_tester.csv"))
  } else{
    rt = read.csv(rstudioapi::selectFile("Select File"))
  }

  if(species == "NORK"){
    norpac_species = 303
  }

  if(species == "REBS"){
    norpac_species = c(307, 357)
  }

  if(is.null(admb_home)){
    R2admb::setup_admb()
  } else {
    R2admb::setup_admb(admb_home)
  }

  nages = length(rec_age:plus_age)

  rt %>%
    dplyr::filter(Species %in% norpac_species,
                  Region == area,
                  Read_Age > 0,
                  Test_Age > 0,
                  Final_Age > 0) %>%
    dplyr::group_by(Test_Age, Read_Age) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::summarise(freq = dplyr::n()) -> dat


  dplyr::left_join(expand.grid(test_age = unique(dat$test_age),
                               read_age = unique(dat$read_age)),
                   dat) %>%
    tidyr::replace_na(list(freq = 0)) %>%
    dplyr::group_by(test_age) %>%
    dplyr::mutate(num = dplyr::case_when(test_age != read_age ~ freq)) %>%
    dplyr::summarise(num = sum(num, na.rm = TRUE),
                     den = sum(freq)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ape = 1 - (num / den),
                  ape = ifelse(is.nan(ape), 0, ape)) %>%
    dplyr::select(age = test_age, ape, ss = den) %>%
    dplyr::left_join(data.frame(age = min(dat$test_age):max(dat$test_age)), .) %>%
    tidyr::replace_na(list(ape = -9, ss = -9)) -> dats

  c("# Number of obs", nrow(dats),
    "# Age vector", dats$age,
    "# Percent agreement vector", dats$ape,
    "# Sample size vector", dats$ss) %>%
    write.table(here::here(year, "data", "models", "ageage", "ageage.dat"),
                sep="", quote=F, row.names=F, col.names=F)

  setwd(here::here(year, "data", "models", "ageage"))
  R2admb::compile_admb("ageage", verbose = TRUE)
  R2admb::run_admb("ageage", verbose=TRUE)

  setwd(here::here())
  read.delim(here::here(year, "data", "models", "ageage", "ageage.std"), sep="") %>%
    dplyr::filter(grepl("_a", name)) %>%
    dplyr::bind_cols(dats) %>%
    dplyr::select(age, value) -> sds

  fit = lm(value ~ age, data = sds)

  # fit out to age 100 (aka: max_age)
  data.frame(age = rec_age:max_age) %>%
    dplyr::mutate(ages_sd = predict(fit, .)) -> fits

  ages = fits$age

  mtx100 = matrix(nrow = nrow(fits), ncol = nrow(fits))
  colnames(mtx100) = rownames(mtx100) = ages

  for(j in 1:nrow(fits)){
    mtx100[j,1] = pnorm(ages[1] + 0.5,
                        ages[j],
                        fits[which(fits[,1] == ages[j]), 2])


    for(i in 2:(nrow(fits) - 1)){
      mtx100[j,i] = pnorm(ages[i] + 0.5,
                          ages[j],
                          fits[which(fits[,1] == ages[j]), 2]) -
        pnorm(ages[i-1] + 0.5,
              ages[j],
              fits[which(fits[,1] == ages[j]), 2])

    }
    mtx100[j,nrow(fits)] = 1 - sum(mtx100[j, 1:(nrow(fits) - 1)])
  }

  write.csv(mtx100, here::here(paste0(year, "data", "output", "ae_mtx_", max_age, ".csv")), row.names = FALSE)
  write.csv(fits,  here::here(year,"data", "output", "ae_SD.csv"), row.names = FALSE)

  # Compute ageing error matrix for model
  ae_Mdl = matrix(nrow=length(ages), ncol=nages)
  ae_Mdl[, 1:(nages-1)] = as.matrix(mtx100[, 1:(nages-1)])
  ae_Mdl[, nages] = rowSums(mtx100[, nages:length(ages)])
  ae_Mdl = round(ae_Mdl, digits=4)
  r = which(ae_Mdl[, nages]>=0.999)[1]
  ae_Mdl = ae_Mdl[1:r,]

  write.csv(ae_Mdl,  here::here(year, "data", "output", "ae_model.csv"), row.names = FALSE)
  ae_Mdl

}
