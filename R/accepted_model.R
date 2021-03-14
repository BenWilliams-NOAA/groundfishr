#' copy the previous SSC accepted model as the new "base" model
#'
#' @description copy of all base model files, also add a "base_model.txt" file that identifies the model used
#' @param base_year year of the base model
#' @param base_model name of the base model (folder name)
#' @param year current year
#'
#' @return
#' @export accepted_model
#'
#' @examples
#' accepted_model(base_year = 2020, base_model = "m18.2b", year = 2021)
accepted_model <- function(base_year, base_model, year){


  x = dir.exists(here::here(year, "base"))
  y = NA

  if (!x) {
    dir.create(here::here(year, "base"))
  } else if(x){
    y = readline("To overwrite the base folder enter 1, else 0: ")
  }

  if(y==1){

    file.copy(list.files(here::here(base_year, base_model), full.names = TRUE),
              here::here(year, "base"),
              recursive = TRUE, overwrite = TRUE)

    write.table(c(paste("You are using", base_model, "from", base_year, "as your base model.")),
                file = here::here(year, "base", paste0("base_model_", base_year, "_", base_model,".txt")),
                sep = "\t", col.names = F, row.names = F)
  } else {
    cat("folder not overwritten.\n")
  }

}
