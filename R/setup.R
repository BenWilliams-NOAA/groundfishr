#' Setup folder structure
#'
#' Creates a common folder structure for ADMB assessments
#'
#' @param year
#'
#' @return
#' @export setup
#'
#' @examples
#' setup(2020)
#'
setup <- function(year){

  dirs = c("raw", "user_input", "output", "sara", "models")
  folders = c("ageage", "allometric", "vonb", "wvonb", "length_sd")

  for(i in 1:length(dirs)){
    if(dir.exists(here::here(year, "data", dirs[i])) == FALSE){
      dir.create(here::here(year, "data", dirs[i]), recursive=TRUE)
    }
  }
  for(i in 1:length(folders)){
    dir.create(here::here(year, "data", "models", folders[i]))
  }

  file.copy(system.file("models", "AGEAGE.tpl", package = "groundfishr"),
            here::here(year, "data", "models", "ageage"))

  file.copy(system.file("models", "allometric.tpl", package = "groundfishr"),
            here::here(year, "data", "models", "allometric"))

  file.copy(system.file("models", "VBL.tpl", package = "groundfishr"),
            here::here(year, "data", "models", "vonb"))

  file.copy(system.file("models", "wVBL.tpl", package = "groundfishr"),
            here::here(year, "data", "models", "wvonb"))

  file.copy(system.file("models", "lvb.ctl", package = "groundfishr"),
            here::here(year, "data", "models", "wvonb"))

  file.copy(system.file("models", "lengthSD.tpl", package = "groundfishr"),
            here::here(year, "data", "models", "length_sd"))

}
