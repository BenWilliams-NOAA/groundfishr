setup_proj <- function(year, model, dat_file, full_assess_yr = "even"){
  vroom::vroom(here::here(year, "data", "raw", paste0(fishery, "_catch_data.csv"))) -> catch_data
  vroom::vroom(here::here(year, "data", "raw",  paste0(fishery, "_obs_data.csv"))) -> obs_data
  vroom(here::here(year, "data", "output",  "yld_rat.csv")) -> yld_rat

  end_date = max(catch_data$WEEK_END_DATE)

  catch |>
    dplyr::filter(Year >= year - 1) |>
    dplyr::mutate(Catch = ifelse(Year == year, round(Catch * yld_rat$catch_rat, 4), Catch)) -> end_yr

  setup <- readLines(here::here(year, model, "projection", "setup.dat"))

  # format setup.dat file
    if(full_assess_yr == "even"){
    if(year %% 2==1){
      setup[grep("#_Begin Year", setup)] = paste(year - 1, "#_Begin Year")
    } else {
      setup[grep("#_Begin Year", setup)] = paste(year, "#_Begin Year")
    }
  } else {
    if(year %% 2==0){
      setup[grep("#_Begin Year", setup)] = paste(year - 1, "#_Begin Year")
    } else {
      setup[grep("#_Begin Year", setup)] = paste(year, "#_Begin Year")
    }
  }

  spp_catch = c(# format run max F projection scenario,
    if(full_assess_yr == "even"){
      if(year %% 2 == 1){2} else{1}
    } else {
      if(year %% 2 == 0){2} else{1}
    },
    "# Number of species",
    1,
    "# data files for each species",
    paste("data/goa_nrthrns.dat",sep=""),
    "# ABC Multipliers",
    1,
    "# Population scalars",
    1000,
    "# Number of TAC model categories",
    1,
    "# TAC model indices (for aggregating)",
    1,
    "# Catch in each future year",
    if(full_assess_yr == "even" & year %% 2 == 1  | full_assess_yr == "odd" & year %% 2 == 0){
      c(paste(paste(end_yr$Year[1], end_yr$Catch[1], sep = "\t"),
              "# Finalized previous year's catch"),
        paste(paste(year, end_yr$Catch[2], sep = "\t"),
              "# Estimated from catch thru", end_date, "with expansion factor =", Endyr_ratio))
    } else {
      paste(paste(year, end_yr$Catch[2], sep = "\t"), "# Estimated from catch thru",
            end_date, "with expansion factor =", Endyr_ratio)
    }
  )

  write.table(spp_catch, file = here::here(year, model, "projection", "spp_catch.dat"), quote=F, row.names=F, col.names=F)
  write.table(spp_catch, file = here::here(year, model, "projection", "nr_max_spcat.dat"), quote=F, row.names=F, col.names=F)

}
