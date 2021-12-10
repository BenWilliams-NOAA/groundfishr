#' Title
#'
#' @param year assessment year
#' @param model the folder the ADMB model is in
#' @param species identifier for file names
#' @param area e.g. "goa"
#' @param off_yr is this a partial year (TRUE) otherwise NULL
#'
#' @return
#' @export setup_proj
#'
#' @examples
setup_proj <- function(year, model, species, area = "goa", off_yr = NULL){

  # create a directory for the projection
  dir.create(here::here(year, "proj", "data"), recursive = TRUE)

  file.copy(system.file("models", "proj.tpl", package = "groundfishr"),
            here::here(year, "proj"))

  file.copy(system.file("models", "tacpar.dat", package = "groundfishr"),
            here::here(year, "proj"))

  # move proj file to new folder
  file.copy(here::here(year, model, "proj.dat"),
            here::here(year, "proj", "data", paste0(area, "_", species,".dat")))

  if(!is.null(off_yr)){
    begin_yr = year - 1
  } else {
    begin_yr = year
  }

  setup = readLines(system.file("models", "setup.dat", package = "groundfishr"), warn=FALSE)
  setup[grep("#_Begin", setup)] <- paste(begin_yr, "#_Begin Year")
  write.table(setup, file=here::here(year, "proj", "setup.dat"), quote=F, row.names=F, col.names=F)

}
