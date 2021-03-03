
#' @export sql_read
#' @export collapse_filters
#' @export sql_filter
#' @export sql_run
#' @export purrit
#' @export collapse_row
sql_read <- function(x) {
  if(file.exists(system.file("sql", x, package = "groundfishr"))) {
    readLines(system.file("sql", x, package = "groundfishr"))
  } else {
    stop("The sql file does not exist.")
  }
}


collapse_filters <- function(x) {
  sprintf("'%s'", paste(x, collapse = "','"))
}

sql_filter <- function(sql_precode = "=", x, sql_code, flag = "-- insert species") {

  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(x), ")"
  )
  sql_code
}

sql_run <- function(database, query) {
  query = paste(query, collapse = "\n")
  DBI::dbGetQuery(database, query, as.is=TRUE, believeNRows=FALSE)
}


#' helper function for comp data
#'
#' obs  observed data from .rep file
#' pred predicted data from .rep file (if used)
#' rec_age  recruitement age
#' plus_age plus age group
#' comp age or length - default is length
#' lenbins set to base unless using alt in which case the file should be in the user_input folder and the name needs to be provided e.g., lengthbins.csv - the column must be named len_bin



purrit <- function(obs, pred = NULL, rec_age, plus_age, comp = "length", lenbins = NULL){

  if(is.null(lenbins)){
    lenbins = read.csv(here::here(year, "data", "user_input", "len_bin_labels.csv"))$len_bins
  } else {
    lenbins = read.csv(here::here(year, "data", "user_input", lenbins))$len_bins
  }

  obs = stringr::str_split(obs, " ")

  purrr::map_if(obs[-1], is.character, as.numeric) %>%
    purrr::map_df(., ~as.data.frame(t(.))) %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) -> obs


  if(comp == "age" & !is.null(pred)){
    obs %>%
      dplyr::select(1:(plus_age - rec_age + 2)) -> obs

    pred = stringr::str_split(pred, " ")
    purrr::map_if(pred[-1], is.character, as.numeric) %>%
      purrr::map_df(., ~as.data.frame(t(.))) %>%
      dplyr::select_if(~sum(!is.na(.)) > 0) %>%
      dplyr::select(1:(plus_age - rec_age + 2)) -> pred

    names(pred) <- names(obs) <- c("year", rec_age:plus_age)

    obs %>%
      tidyr::pivot_longer(-year, "age") %>%
      dplyr::mutate(groups = "obs") %>%
      dplyr::bind_rows(pred %>%
                         tidyr::pivot_longer(-year, "age") %>%
                         dplyr::mutate(groups = "pred")) %>%
      dplyr::mutate(age = as.integer(age),
                    Age = factor(age),
                    Year = factor(year)) -> dat


  } else if(comp != "age" & !is.null(pred)){

    obs %>%
      dplyr::select(1:(length(lenbins) + 1)) -> obs

    pred = stringr::str_split(pred, " ")
    purrr::map_if(pred[-1], is.character, as.numeric) %>%
      purrr::map_df(., ~as.data.frame(t(.))) %>%
      dplyr::select_if(~sum(!is.na(.)) > 0) %>%
      dplyr::select(1:(length(lenbins) + 1)) -> pred

    names(pred) <- names(obs) <- c("year", lenbins)

    obs %>%
      tidyr::pivot_longer(-year, "length") %>%
      dplyr::mutate(groups = "obs") %>%
      dplyr::bind_rows(pred %>%
                         tidyr::pivot_longer(-year, "length") %>%
                         dplyr::mutate(groups = "pred")) %>%
      dplyr::mutate(length = as.integer(length),
                    Length = factor(length),
                    Year = factor(year)) -> dat

  } else if(comp == "age" & is.null(pred)){
    obs %>%
      dplyr::select(1:(plus_age - rec_age + 2)) -> obs

    names(obs) <- c("year", rec_age:plus_age)

    obs %>%
      tidyr::pivot_longer(-year, "age") %>%
      dplyr::mutate(groups = "obs") %>%
      dplyr::mutate(age = as.integer(age),
                    Age = factor(age),
                    Year = factor(year)) -> dat

  } else if(comp != "age" & is.null(pred)){

    obs %>%
      dplyr::select(1:(length(lenbins) + 1)) -> obs

    names(obs) <- c("year", lenbins)

    obs %>%
      tidyr::pivot_longer(-year, "length") %>%
      dplyr::mutate(groups = "obs") %>%
      dplyr::mutate(length = as.integer(length),
                    Length = factor(length),
                    Year = factor(year)) -> dat
  }

  dat
}

#' helper function for creating dat file

collapse_row <- function(data){

  l1 = paste(as.vector(data[1,]), collapse = " ")

  for(i in 2:nrow(data)){
    l2 = paste(as.vector(data[i,]), collapse = " ")
    l1 = c(l1, l2)
  }
  l1
}



