#' Concatenate a .dat file
#'
#' @param year assessment year
#' @param species "NORK", "REBS", "SABL"
#' @param area "goa", "bsai", "everywhere"
#' @param model folder that the `.tpl` will be in
#' @param dat_name what to call the .dat file - ".dat" will be appended to the name
#' @param rec_age recruitment age
#' @param plus_age plus age group
#' @param spawn_mo spawning month
#' @param maturity if maturity is from outside the model (should be placed in user_input folder)
#' @param n_ageage number of age error transmission matrices default is 1
#' @param n_sizeage number of size at age transmission matrices default is 1
#' @param retro not yet implemented
#' @param n_fleets number of fishing fleets e.g., ll fleet, trawl fleet, default is 1
#' @param n_ts number of trawl surveys dafault is 1
#' @param n_lls number of longline surveys e.g., domestic and japanes, default is 1
#' @export concat_dat
#'
#' @examples concat_dat(year = 2020, species = "NORK",  area = "goa", model = "base", dat_name = "goa_nr", rec_age = 2, plus_age = 45)
#'
concat_dat <- function(year, species, area = "goa", model, dat_name, rec_age, plus_age, spawn_mo = 5,
                       maturity = NULL, n_ageage = 1, n_sizeage = 1, retro = NULL, n_fleets = 1, n_ts = NULL, n_lls = NULL){

  # create directory
  if (!dir.exists(here::here(year, model))){
    dir.create(here::here(year, model), recursive=TRUE)
  }


  if(length(grep(paste0(area,"_lls"),
                 list.files(here::here(year, "data", "output")), value=TRUE)) > 0){
    llslc = read.csv(here::here(year, "data", "output", paste0(area, "_lls_length_comp.csv")))
    llsb = read.csv(here::here(year, "data", "output", paste0(area, "_lls_biomass.csv")))
  }

  if(!is.null(maturity)){
    mature = as.vector(read.csv(paste0(here::here(year, "data", "user_input", maturity))) %>%
                         dplyr::rename_all(tolower) %>%
                         dplyr::select(-age))
  }

  fishery = grep("fsh", list.files(here::here(year, 'data', "output")), value=TRUE)
  survey = grep("ts_", list.files(here::here(year, 'data', "output")), value=TRUE)
  ll_survey = grep("lls_", list.files(here::here(year, 'data', "output")), value=TRUE)

  catch = read.csv(here::here(year, "data", "output", grep("catch", fishery, value=TRUE)))
  waa = read.csv(here::here(year, "data", "output", "waa.csv"))
  saa = read.csv(here::here(year, "data", "output", "saa.csv"))
  ae = read.csv(here::here(year, "data", "output", "ae_model.csv"))
  fishac = read.csv(here::here(year, "data", "output", grep("age", fishery, value=TRUE)))
  fishlc = read.csv(here::here(year, "data", "output", grep("length", fishery, value=TRUE)))
  tsac = read.csv(here::here(year, "data", "output", grep("age", survey, value=TRUE)))
  tslc = read.csv(here::here(year, "data", "output", grep("length", survey, value=TRUE)))
  tsb = read.csv(here::here(year, "data", "output", grep("biomass", survey, value=TRUE)))

  if(length(ll_survey) > 0){
    llsrpw = read.csv(here::here(year, "data", "output", grep("biomass", ll_survey, value=TRUE)))
    llsslc = read.csv(here::here(year, "data", "output", grep("length", ll_survey, value=TRUE)))
    llsrpn = read.csv(here::here(year, "data", "output", grep("numbers", ll_survey, value=TRUE)))
  }

  names(tsb) <- c("year", "biomass", "se", "lci", "uci")
  m_nages = nrow(ae)
  nages = length(rec_age:plus_age)

  # get length bin info
  lbin = as.numeric(gsub("[^0-9.]", "",  colnames(tslc)))
  lbin = lbin[!is.na(lbin)]
  nlenbins = length(lbin)

  if(is.null(n_ageage)){
    n_ageage = 1
  }

  if(is.null(n_sizeage)){
    n_sizeage = 1
  }

  sep = "# -------------------------------------------------------------------"

  # header ----
  header = c(sep,
             paste0("#", area, " ", species, " Rockfish .dat file for ADMB optimization"),
             paste ("# New data provided on:", read.table(file = here::here(year, "data/raw/data_called.txt"),
                                                          sep = "\t")[2,1]),
             "# Notes:",
             "#   ~ Weight-at-age and length-age transition matrix automatically updated",
             "#   ~ Formatted to conduct automated retrospective analysis",
             "#   ~ Does not use most recent years fishery size data",
             "#   ~ Does not use fishery size data in years when ages are expected",
             sep,
             "#",
             "#")

  # model inputs ----

  if(is.null(maturity)){
    mipv <- c(sep,
              "# Model input parameters/vectors",
              sep,
              "# Start and end years, recruitment age, number of age and length bins",
              "# Model start year (styr):",
              as.character(min(catch$Year)),
              "# Model end year (endyr): #!",
              as.character(year),
              "# Age at recruitment (rec_age): #-",
              as.character(rec_age),
              "# Number of ages in data (nages_D):",
              as.character(nages),
              "# Number of ages in model (nages_M):",
              as.character(m_nages),
              "# Number of length bins (nlenbins):",
              as.character(nlenbins),
              "# Number of age-age transition matrices (n_ageage_mat):",
              as.character(n_ageage),
              "# Number of size-age transition matrices (n_sizeage_mat):",
              as.character(n_sizeage),
              "# Length bin labels (len_bin_labels):",
              paste(lbin, collapse=" "),
              "# Spawn month (spawn_fract):",
              as.character(spawn_mo),
              "#",
              "#")

  } else {
    mipv <- c(sep,
              "# Model input parameters/vectors",
              sep,
              "# Start and end years, recruitment age, number of age and length bins",
              "# Model start year (styr):",
              as.character(min(catch$Year)),
              "# Model end year (endyr): #!",
              as.character(year),
              "# Age at recruitment (rec_age): #-",
              as.character(rec_age),
              "# Number of ages in data (nages_D):",
              as.character(nages),
              "# Number of ages in model (nages_M):",
              as.character(m_nages),
              "# Number of length bins (nlenbins):",
              as.character(nlenbins),
              "# Number of age-age transition matrices (n_ageage_mat):",
              as.character(n_ageage),
              "# Number of size-age transition matrices (n_sizeage_mat):",
              as.character(n_sizeage),
              "# Length bin labels (len_bin_labels):",
              paste(lbin, collapse=" "),
              "# Spawn month (spawn_fract):",
              as.character(spawn_mo),
              "#",
              "#")

    mat = c(sep,
            "Proportion mature at age (p_mature):",
            paste0("#! ",
                   paste(mature$mature, collapse = " ")),
            "#-",
            "",
            "")
  }

  waa = c(sep,
          "Weight-at-age (wt):",
          paste0("#! ",
                 paste(waa$x, collapse=" ")),
          "#-",
          "#",
          "#")

  # fishery catch ----
  fishery_catch = c(sep,
                    "# Fishery catch (mt): obs_catch(styr,endyr)",
                    sep,
                    paste0("#! ", paste(min(catch$Year):year, collapse=" ")),
                    paste(catch$Catch, collapse=" "),
                    "#-",
                    "",
                    "")
  # cpue ----
  # not currently used for northern rockfish
  cpue = c(sep,
           "# CPUE Data",
           sep,
           "# Number of CPUE years",
           "0",
           "# CPUE observations (leave blank if 0)",
           "",
           "")

  # trawl biomass ----
  trawl_biomass = c(sep,
                    "# Trawl Survey Biomass",
                    sep,
                    "#! Number of trawl surveys: nyrs_srv1",
                    as.character(nrow(tsb)),
                    "#- Trawl survey years: yrs_srv1(1,nyrs_srv1) #!",
                    paste(tsb$year, collapse=" "),
                    "#- Observed trawl survey biomass (mt): obs_srv1_biom(1,nyrs_srv1) #!",
                    paste(tsb$biomass, collapse=" "),
                    "#- SE of observed trawl survey biomass: obs_srv1_se(1,nyrs_srv1) #!",
                    paste(tsb$se, collapse=" "),
                    "#- Lower CI, 1.96*SE #!",
                    paste(tsb$lci, collapse=" "),
                    "#- Upper CI, 1.96*SE #!",
                    paste(tsb$uci, collapse=" "),
                    "#-",
                    "",
                    "")
  # long line survey biomass ----

  if(exists("llsrpw")){
    ll_biomass = c(
      sep,
      "# Longline Survey Biomass",
      sep,
      "# Number of longline surveys: nyrs_srv2",
      as.character(nrow(llsb)),
      "# Longline survey years: yrs_srv2(1,nyrs_srv2)",
      paste(llsb$year, collapse=" "),
      "# Observed longline survey biomass (mt): obs_srv2_biom(1,nyrs_srv2)",
      paste(llsb$rpw, collapse=" "),
      "# SE of observed longline survey biomass: obs_srv2_se(1,nyrs_srv2)",
      paste(llsb$sd, collapse=" "),
      "# Lower CI, 1.96*SE",
      paste(llsb$lci, collapse=" "),
      "# Upper CI, 1.96*SE",
      paste(llsb$uci, collapse=" "),
      "",
      "")
  } else {
    ll_biomass = c(
      sep,
      "# Longline Survey Biomass",
      sep,
      "# Number of longline surveys: nyrs_srv2",
      "1",
      "# Longline survey years: yrs_srv2(1,nyrs_srv2)",
      "1999",
      "# Observed longline survey biomass (mt): obs_srv2_biom(1,nyrs_srv2)",
      "1000",
      "# SE of observed longline survey biomass: obs_srv2_se(1,nyrs_srv2)",
      "100",
      "# Lower CI, 1.96*SE",
      "10",
      "# Upper CI, 1.96*SE",
      "10000",
      "",
      "")
  }

  # fishery age comp ----
  fac <- c(
    sep,
    "# Fishery Age Composition",
    sep,
    "#! Number of years: nyrs_fish_age",
    as.character(nrow(fishac)),
    "#- Fishery age comp years: yrs_fish_age #!",
    paste(fishac$year, collapse=" "),
    "#- Number of samples: nsamples_fish_age(1,nyrs_fish_age) #!",
    paste(fishac$n_s, collapse=" "),
    "#- Number of hauls: nhauls_fish_age(1,nyrs_fish_age) #!",
    paste(fishac$n_h, collapse=" "),
    "#- Index for age-age error matrix #!",
    paste(fishac$AA_Index, collapse=" "),
    "#- Observed fishery age compositions (proportions at age): oac_fish(1,nyrs_fish_age,1,nages) #!",
    collapse_row(dplyr::select(fishac, -year, -n_s, -n_h, -AA_Index)),
    "#-",
    "",
    "")

  # trawl survey age comp ----

  tsac <- c(sep,
            "# Trawl Survey Age Composition",
            sep,
            "#! Number of years: nyrs_srv1_age",
            as.character(nrow(tsac)),
            "#- Trawl Survey age comp years: yrs_srv1_age #!",
            paste(tsac$year, collapse=" "),
            "#- Number of samples: nsamples_srv1_age(1,nyrs_srv1_age) #!",
            paste(tsac$n_s, collapse=" "),
            "#- Number of hauls: nhauls_srv1_age(1,nyrs_srv1_age) #!",
            paste(tsac$n_h, collapse=" "),
            "#- Index for age-age error matrix #!",
            paste(tsac$AA_Index, collapse=" "),
            "#- Observed trawl survey age compositions (proportions at age): oac_srv1(1,nyrs_srv1_age,1,nages) #!",
            collapse_row(dplyr::select(tsac, -year, -n_s, -n_h, -AA_Index)),
            "#-",
            "",
            "")

  # fishery length comp ----
  flc <- c(
    sep,
    "# Fishery Size Composition",
    sep,
    "#! Number of years:",
    as.character(nrow(fishlc)),
    "#- Fishery size comp years: #!",
    paste(fishlc$year, collapse=" "),
    "#- Number of samples:  #!",
    paste(fishlc$n_s, collapse=" "),
    "#- Number of hauls:  #!",
    paste(fishlc$n_h, collapse=" "),
    "#- Index for size-age error matrix #!",
    paste(fishlc$SA_Index, collapse=" "),
    "#- Observed fishery size compositions (proportions at age)#!",
    collapse_row(dplyr::select(fishlc, -year, -n_s, -n_h, -SA_Index)),
    "#-",
    "",
    "")

  # trawl survey size comp ----
  tslc <- c(
    sep,
    "# Trawl Survey Size Composition",
    sep,
    "#! Number of years:",
    as.character(nrow(tslc)),
    "#- Survey Years: #!",
    paste(tslc$year, collapse=" "),
    "#- Number of samples:#!",
    paste(tslc$n_s, collapse=" "),
    "#- Number of hauls: #!",
    paste(tslc$n_h, collapse=" "),
    "#- Index for size-age error matrix #!",
    paste(tslc$SA_Index, collapse=" "),
    "#- Observed survey size compositions (proportions at age): oac_fish(1,nyrs_fish_age,1,nages) #!",
    collapse_row(dplyr::select(tslc, -year, -n_s, -n_h, -SA_Index)),
    "#-",
    "",
    "")

  # longline survey size comp ----
  if(exists("llslc")){

    llsc <- c(sep,
              "# Longline Survey Size Composition",
              sep,
              "# Number of years: nyrs_srv2_size",
              as.character(nrow(llslc)),
              "# Longline Survey size comp years: yrs_srv1_size",
              paste(llslc$year, collapse=" "),
              "# Number of samples: nsamples_srv2_size(1,nyrs_srv2_size)",
              paste(llslc$n_s, collapse=" "),
              "# Number of hauls: nhauls_srv2_size(1,nyrs_srv2_size)",
              paste(llslc$n_h, collapse=" "),
              "# Index for size-age error matrix",
              paste(llslc$SA_Index, collapse=" "),
              "# Observed longline survey size compositions (proportions at length): osc_srv2(1,nyrs_srv2_size,1,nlenbins)",
              collapse_row(dplyr::select(llslc, -year, -n_s, -n_h, -SA_Index)),
              "",
              "")
  } else {
    llsc <- c(sep,
              "# Longline Survey Size Composition, NOT USED IN MODEL, include one year of fake data",
              sep,
              "# Number of years: nyrs_srv2_size",
              "1",
              "# Longline Survey size comp years: yrs_srv1_size",
              "1999",
              "# Number of samples: nsamples_srv2_size(1,nyrs_srv2_size)",
              "99",
              "# Number of hauls: nhauls_srv2_size(1,nyrs_srv2_size)",
              "99",
              "# Index for size-age error matrix",
              "1",
              "# Observed longline survey size compositions (proportions at length): osc_srv2(1,nyrs_srv2_size,1,nlenbins)",
              paste(seq(1/nlenbins, 1/nlenbins, length.out=nlenbins), collapse=" "),
              "",
              "")
  }

  # size-age transition matrix ----
  sizeage <- c(sep,
               "# Size-age transition matrix: proportion at size given age: ",
               sep,
               collapse_row(dplyr::select(saa, -age)),
               "#",
               "",
               "")

  # age error matrix ----
  aa <- c(sep,
          "# age error transition matrix: ",
          sep,
          collapse_row(ae),
          "#",
          "",
          "")

  # eof ----
  eof <- c(sep,
           "# end of file marker",
           sep,
           "42",
           "#!")

  # Compile DAT file for ADMB ----

  if(is.null(maturity)){
    dat <- c(header,
             mipv,
             waa,
             fishery_catch,
             cpue,
             trawl_biomass,
             ll_biomass,
             fac,
             tsac,
             flc,
             tslc,
             llsc,
             sizeage,
             aa,
             eof)
  } else {
    dat <- c(header,
             mipv,
             mat,
             waa,
             fishery_catch,
             cpue,
             trawl_biomass,
             ll_biomass,
             fac,
             tsac,
             flc,
             tslc,
             llsc,
             sizeage,
             aa,
             eof)
  }


  write.table(dat, file = here::here(year, model, paste0(dat_name, ".dat")) ,
              quote=FALSE, row.names=FALSE, col.names=FALSE)
}
